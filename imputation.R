# step1----
prepare_data <- function() {
  eco_burden <- bar_Kt_Yt %>%
    filter(year == 2019) %>%
    select(country, disease, diff_Yt, bar_Yt) %>%
    mutate(Yt = bar_Yt - diff_Yt) %>%  
    mutate(
      burden_ratio = diff_Yt / Yt,  
      log_burden_ratio = log(burden_ratio)  
    )
  dalys_data <- data.DALYs.rate.124.all.age %>%
    filter(year == 2019) %>%
    select(country, disease, val, SDI_quitile)
  combined_data <- inner_join(eco_burden, dalys_data,
                              by = c("country", "disease"))
  combined_data <- combined_data %>%
    mutate(
      High_SDI = ifelse(SDI_quitile == "High SDI", 1, 0),
      High_middle_SDI = ifelse(SDI_quitile == "High-middle SDI", 1, 0),
      Low_middle_SDI = ifelse(SDI_quitile == "Low-middle SDI", 1, 0),
      Low_SDI = ifelse(SDI_quitile == "Low SDI", 1, 0)
    )
  
  return(combined_data)
}
run_models <- function(data) {
  all_diseases <- unique(data$disease)
  results_list <- list()
  for(d in all_diseases) {
    disease_data <- data %>% filter(disease == d)
    if(nrow(disease_data) < 10) {
      warning(paste("跳过疾病", d, "因为数据点数量不足"))
      next
    }
    model <- lm(log_burden_ratio ~ val + High_SDI + High_middle_SDI + Low_middle_SDI + Low_SDI, data = disease_data)
    model_summary <- summary(model)
    model_results <- tidy(model)
    model_results$disease <- d
    model_results$r_squared <- model_summary$r.squared
    results_list[[length(results_list) + 1]] <- model_results
  }
  all_results <- bind_rows(results_list)
  
  return(all_results)
}
format_results <- function(results) {
  table_data <- results %>%
    mutate(
      term = case_when(
        term == "(Intercept)" ~ "Constant",
        term == "val" ~ "DALYs coefficient",
        term == "High_SDI" ~ "High vs Middle",
        term == "High_middle_SDI" ~ "High-middle vs Middle",
        term == "Low_middle_SDI" ~ "Low-middle vs Middle",
        term == "Low_SDI" ~ "Low vs Middle",
        TRUE ~ term
      ),
      p_formatted = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)),
      estimate_formatted = case_when(
        abs(estimate) < 1e-8 & estimate != 0 ~ sprintf("%.4e", estimate),
        abs(estimate) < 0.01 ~ sprintf("%.10f", estimate),
        TRUE ~ sprintf("%.6f", estimate)
      )
    ) %>%
    select(disease, term, estimate_formatted, p_formatted, r_squared)
  r_squared_df <- table_data %>%
    group_by(disease) %>%
    dplyr::summarize(R_squared = mean(r_squared, na.rm = TRUE))
  wide_table <- table_data %>%
    select(-r_squared) %>%
    pivot_wider(
      id_cols = disease,
      names_from = term,
      values_from = c(estimate_formatted, p_formatted)
    )
  wide_table <- left_join(wide_table, r_squared_df, by = "disease")
  
  return(wide_table)
}
visualize_models <- function(data, results) {
  all_diseases <- unique(data$disease)
  plot_list <- list()
  
  for(d in all_diseases) {
    disease_data <- data %>% filter(disease == d)
    if(nrow(disease_data) < 10) next
    model <- tryCatch({
      lm(log_burden_ratio ~ val + High_SDI + High_middle_SDI + Low_middle_SDI + Low_SDI, data = disease_data)
    }, error = function(e) {
      warning(paste("无法为疾病创建模型:", d, e$message))
      return(NULL)
    })
    
    if(is.null(model)) next
    disease_data$predicted_log <- predict(model)
    disease_data$predicted_original <- exp(disease_data$predicted_log)
    r2 <- summary(model)$r.squared
    p <- ggplot(disease_data, aes(x = predicted_log, y = log_burden_ratio, color = SDI_quitile)) +
      geom_point(alpha = 0.7) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(
        title = paste("Disease (log scale):", d),
        subtitle = paste("R² =", round(r2, 3)),
        x = "Predicted log(burden ratio)",
        y = "Actual log(burden ratio)",
        color = "SDI Quintile"
      ) +
      theme_minimal()
    
    print(p)
    plot_list[[d]] <- p
  }
  
  return(plot_list)
}
main <- function() {
  cat("准备数据...\n")
  combined_data <- prepare_data()
  cat("运行模型...\n")
  model_results <- run_models(combined_data)
  cat("格式化结果...\n")
  formatted_table <- format_results(model_results)
  cat("\n回归结果:\n")
  print(kable(formatted_table, caption = "Regression Results by Disease (Log Scale)"))
  cat("\n创建可视化...\n")
  plots <- visualize_models(combined_data, model_results)
  return(list(
    data = combined_data,
    results = model_results,
    table = formatted_table,
    plots = plots
  ))
}
library(broom)
library(knitr)
results <- main()
combined_data <- results$data
model.coeff.1 <- results$table

names(model.coeff.1) <- c("disease",
                          "beta0", "beta1_DALYs", "beta2_HighSDI", "beta3_High-middle","beta4_Low-middle","beta5_Low",
                          "p_beta0", "p_DALYs", "p_HighSDI", "p_High-middle","p_Low-middle","p_Low",
                          "R_squared")






# step2----

predict_yimputed_66_log <- function(bar_Kt_Yt, pre.data.DALYs.rate.190.all.age, model_coeff) {
  library(dplyr)
  existing_countries <- unique(bar_Kt_Yt$country)
  all_countries <- unique(pre.data.DALYs.rate.190.all.age$country)
  countries_to_predict <- setdiff(all_countries, existing_countries)
  Yimputed_66 <- data.frame()
  total <- length(countries_to_predict)
  current <- 0
  for(country_i in countries_to_predict) {
    current <- current + 1
    cat(sprintf("处理国家 %d/%d: %s\n", current, total, country_i))
    country_sdi_data <- pre.data.DALYs.rate.190.all.age %>%
      filter(country == country_i, !is.na(SDI_quitile)) %>%
      select(SDI_quitile) %>%
      distinct()
    if(nrow(country_sdi_data) == 0) {
      warning(paste("跳过国家", country_i, "因为没有找到SDI等级"))
      next
    }
    
    country_sdi <- country_sdi_data$SDI_quitile[1]
    High_SDI <- ifelse(country_sdi == "High SDI", 1, 0)
    High_middle_SDI <- ifelse(country_sdi == "High-middle SDI", 1, 0)
    Low_middle_SDI <- ifelse(country_sdi == "Low-middle SDI", 1, 0)
    Low_SDI <- ifelse(country_sdi == "Low SDI", 1, 0)
    for(disease_i in unique(model_coeff$disease)) {
      coeff <- model_coeff %>%
        filter(disease == disease_i)
      if(nrow(coeff) == 0) {
        warning(paste("跳过疾病", disease_i, "因为没有找到系数"))
        next
      }
      beta0 <- as.numeric(coeff$beta0)
      beta1 <- as.numeric(coeff$beta1_DALYs)
      beta2 <- as.numeric(coeff$beta2_HighSDI)
      beta3 <- as.numeric(coeff$`beta3_High-middle`)
      beta4 <- as.numeric(coeff$`beta4_Low-middle`)
      beta5 <- as.numeric(coeff$beta5_Low)
      dalys_data <- pre.data.DALYs.rate.190.all.age %>%
        filter(country == country_i, disease == disease_i, year >= 2015, year <= 2050) %>%
        select(year, val)
      if(nrow(dalys_data) == 0) {
        warning(paste("跳过国家", country_i, "疾病", disease_i, "因为没有DALYs数据"))
        next
      }
      for(i in 1:nrow(dalys_data)) {
        year_i <- dalys_data$year[i]
        dalys_val <- dalys_data$val[i]
        log_Yimputed_val <- beta0 +
          beta1 * dalys_val +
          beta2 * High_SDI +
          beta3 * High_middle_SDI +
          beta4 * Low_middle_SDI +
          beta5 * Low_SDI
        Yimputed_val <- exp(log_Yimputed_val)
        Yimputed_66 <- rbind(Yimputed_66, data.frame(
          country = country_i,
          disease = disease_i,
          year = year_i,
          log_Yimputed = log_Yimputed_val,
          Yimputed = Yimputed_val
        ))
      }
    }
  }
  
  return(Yimputed_66)
}
Yimputed_66_log <- predict_yimputed_66_log(bar_Kt_Yt.13, pre.data.DALYs.rate.190.all.age, model_coeff.13)



# step3

library(rms)
library(broom)
library(tidyverse)

rcs_diseases <- c("Gallbladder and biliary diseases",
                  "Gallbladder and biliary tract cancer",
                  "Vascular intestinal disorders")

prepare_data_for_rcs_multivar <- function() {
  eco_burden <- bar_Kt_Yt %>%
    dplyr::filter(year == 2019) %>%
    dplyr::select(country, disease, diff_Yt, bar_Yt) %>%
    dplyr::mutate(
      Yt = bar_Yt - diff_Yt,
      burden_ratio = diff_Yt / Yt,
      log_burden_ratio = log(burden_ratio)
    )
  dalys_data <- data.DALYs.rate.124.all.age %>%
    dplyr::filter(year == 2019) %>%
    dplyr::select(country, disease, val, SDI_quitile) %>%  # 保留SDI_quitile
    dplyr::rename(val_dalys = val)
  incidence_data <- data.Inci.124.all.age %>%
    dplyr::filter(year == 2016) %>%
    dplyr::select(country, disease, val) %>%  # ✅ 不要SDI_quitile
    dplyr::rename(val_incidence = val)
  combined_data <- eco_burden %>%
    dplyr::inner_join(dalys_data, by = c("country", "disease")) %>%
    dplyr::inner_join(incidence_data, by = c("country", "disease")) %>%
    dplyr::mutate(
      High_SDI = ifelse(SDI_quitile == "High SDI", 1, 0),
      High_middle_SDI = ifelse(SDI_quitile == "High-middle SDI", 1, 0),
      Low_middle_SDI = ifelse(SDI_quitile == "Low-middle SDI", 1, 0),
      Low_SDI = ifelse(SDI_quitile == "Low SDI", 1, 0)
    )
  
  return(combined_data)
}
run_rcs_multivar <- function(data, diseases = rcs_diseases) {
  results_list <- list()
  
  for(d in diseases) {
    cat("正在建模:", d, "\n")
    
    disease_data <- data %>% dplyr::filter(disease == d)
    if(nrow(disease_data) < 15) {
      warning(paste("数据点不足:", d, "只有", nrow(disease_data), "个样本"))
      next
    }
    model_multi <- lm(log_burden_ratio ~ 
                        rcs(val_dalys, 4) + 
                        rcs(val_incidence, 4) + 
                        High_SDI + High_middle_SDI + 
                        Low_middle_SDI + Low_SDI, 
                      data = disease_data)
    model_dalys <- lm(log_burden_ratio ~ 
                        rcs(val_dalys, 4) + 
                        High_SDI + High_middle_SDI + 
                        Low_middle_SDI + Low_SDI, 
                      data = disease_data)
    model_inci <- lm(log_burden_ratio ~ 
                       rcs(val_incidence, 4) + 
                       High_SDI + High_middle_SDI + 
                       Low_middle_SDI + Low_SDI, 
                     data = disease_data)
    model_summary_multi <- summary(model_multi)
    model_summary_dalys <- summary(model_dalys)
    model_summary_inci <- summary(model_inci)
    
    model_results <- tidy(model_multi)
    model_results$disease <- d
    model_results$r_squared <- model_summary_multi$r.squared
    model_results$adj_r_squared <- model_summary_multi$adj.r.squared
    results_list[[d]] <- list(
      model_multi = model_multi,
      model_dalys = model_dalys,
      model_inci = model_inci,
      coefficients = model_results,
      data = disease_data,
      r_squared_multi = model_summary_multi$r.squared,
      adj_r_squared_multi = model_summary_multi$adj.r.squared,
      r_squared_dalys = model_summary_dalys$r.squared,
      r_squared_inci = model_summary_inci$r.squared
    )
    
    cat("  多变量模型 R² =", round(model_summary_multi$r.squared, 4), "\n")
    cat("  仅DALYs R² =", round(model_summary_dalys$r.squared, 4), "\n")
    cat("  仅发病率 R² =", round(model_summary_inci$r.squared, 4), "\n")
    cat("  提升 =", round((model_summary_multi$r.squared - model_summary_dalys$r.squared) * 100, 2), "%\n\n")
  }
  
  return(results_list)
}
plot_rcs_fit_multivar <- function(rcs_results) {
  plots <- list()
  
  for(disease_name in names(rcs_results)) {
    result <- rcs_results[[disease_name]]
    disease_data <- result$data
    model <- result$model_multi
    val_dalys_range <- seq(min(disease_data$val_dalys), 
                           max(disease_data$val_dalys), 
                           length.out = 100)
    median_incidence <- median(disease_data$val_incidence)
    
    pred_data_list <- list()
    for(sdi in unique(disease_data$SDI_quitile)) {
      pred_df <- data.frame(
        val_dalys = val_dalys_range,
        val_incidence = median_incidence,  
        High_SDI = ifelse(sdi == "High SDI", 1, 0),
        High_middle_SDI = ifelse(sdi == "High-middle SDI", 1, 0),
        Low_middle_SDI = ifelse(sdi == "Low-middle SDI", 1, 0),
        Low_SDI = ifelse(sdi == "Low SDI", 1, 0),
        SDI_quitile = sdi
      )
      pred_df$predicted <- predict(model, newdata = pred_df)
      pred_data_list[[sdi]] <- pred_df
    }
    pred_data <- dplyr::bind_rows(pred_data_list)
    p <- ggplot() +
      geom_point(data = disease_data, 
                 aes(x = val_dalys, y = log_burden_ratio, color = SDI_quitile),
                 alpha = 0.6, size = 2.5) +
      geom_line(data = pred_data,
                aes(x = val_dalys, y = predicted, color = SDI_quitile),
                linewidth = 1.2) +
      labs(
        title = paste(disease_name, "(多变量模型)"),
        subtitle = paste0("RCS Model (DALYs + Incidence) | R² = ", 
                          round(result$r_squared_multi, 3),
                          " | vs DALYs only: ", 
                          round(result$r_squared_dalys, 3),
                          " | Δ = +",
                          round((result$r_squared_multi - result$r_squared_dalys) * 100, 1), "%"),
        x = "DALYs (per 100,000 population)",
        y = "log(Economic Burden / GDP)",
        color = "SDI Quintile",
        caption = paste0("Note: Incidence rate fixed at median (", 
                         round(median_incidence, 1), " per 100k)")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        legend.position = "bottom",
        plot.caption = element_text(size = 9, hjust = 0)
      )
    
    print(p)
    plots[[disease_name]] <- p
  }
  
  return(plots)
}



cat("\n开始建立多变量RCS模型（DALYs + 发病率）...\n")
cat(strrep("=", 70), "\n")
cat("\n步骤1: 准备数据\n")
combined_data_multivar <- prepare_data_for_rcs_multivar()
cat("数据准备完成！样本量:", nrow(combined_data_multivar), "\n")
cat("数据列名:", paste(names(combined_data_multivar), collapse = ", "), "\n")
cat("\n步骤2: 建立多变量RCS模型\n")
rcs_results_multivar <- run_rcs_multivar(combined_data_multivar)
cat("\n", strrep("=", 70), "\n")
cat("模型比较结果:\n")
cat(strrep("=", 70), "\n")

for(disease_name in names(rcs_results_multivar)) {
  result <- rcs_results_multivar[[disease_name]]
  
  cat(sprintf("\n【%s】\n", disease_name))
  cat(sprintf("  样本量: %d\n", nrow(result$data)))
  cat(sprintf("  %-30s R² = %.4f\n", "多变量模型 (DALYs+发病率):", result$r_squared_multi))
  cat(sprintf("  %-30s R² = %.4f\n", "仅DALYs:", result$r_squared_dalys))
  cat(sprintf("  %-30s R² = %.4f\n", "仅发病率:", result$r_squared_inci))
  
  improvement <- (result$r_squared_multi - result$r_squared_dalys) * 100
  if(improvement > 0) {
    cat(sprintf("  ✅ 多变量模型提升: +%.2f%%\n", improvement))
  } else {
    cat(sprintf("  ⚠️ 多变量模型未提升: %.2f%%\n", improvement))
  }
  aic_multi <- AIC(result$model_multi)
  aic_dalys <- AIC(result$model_dalys)
  aic_inci <- AIC(result$model_inci)
  
  cat(sprintf("  AIC比较（越小越好）:\n"))
  cat(sprintf("    多变量: %.1f\n", aic_multi))
  cat(sprintf("    仅DALYs: %.1f\n", aic_dalys))
  cat(sprintf("    仅发病率: %.1f\n", aic_inci))
  
  if(aic_multi < aic_dalys) {
    cat("  ✅ 多变量模型AIC更优\n")
  } else {
    cat("  ⚠️ 仅DALYs模型AIC更优（多变量可能过拟合）\n")
  }
}

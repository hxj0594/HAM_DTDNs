
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






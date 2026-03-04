
  
  # calculate schooling ------------------------------------------------------------------

  predict_schooling_multiple <- function(data, start_year = 2015, end_year = 2050, 
                                         min_schooling = 0, max_schooling = 20) {
    required_cols <- c("country", "year", "age_grp", "avg_schooling")
    if(!all(required_cols %in% names(data))) {
      stop("数据缺少必要的列: ", paste(setdiff(required_cols, names(data)), collapse = ", "))
    }
    data$year <- as.numeric(as.character(data$year))
    data$avg_schooling <- as.numeric(data$avg_schooling)
    countries <- unique(data$country)
    age_groups <- unique(data$age_grp)
    pred_years <- seq(start_year, end_year, 1)
    result_df <- data.frame()
    for(country_i in countries) {
      cat("正在处理国家:", country_i, "\n")
      country_data <- subset(data, country == country_i)
      for(age_i in age_groups) {
        age_data <- subset(country_data, age_grp == age_i)
        if(nrow(age_data) < 3) {
          warning(paste("国家", country_i, "年龄组", age_i, "的数据点不足，使用最后一个观测值"))
          if(nrow(age_data) > 0) {
            last_year <- max(age_data$year)
            last_value <- age_data$avg_schooling[age_data$year == last_year][1]
            pred_df <- data.frame(
              country = rep(country_i, length(pred_years)),
              year = pred_years,
              age_grp = rep(age_i, length(pred_years)),
              avg_schooling = rep(last_value, length(pred_years))
            )
            result_df <- rbind(result_df, pred_df)
          }
          next
        }
        fit <- lm(avg_schooling ~ year, data = age_data)
        pred_values <- predict(fit, newdata = data.frame(year = pred_years))
        pred_values <- pmin(pmax(pred_values, min_schooling), max_schooling)
        pred_df <- data.frame(
          country = rep(country_i, length(pred_years)),
          year = pred_years,
          age_grp = rep(age_i, length(pred_years)),
          avg_schooling = pred_values
        )
        result_df <- rbind(result_df, pred_df)
      }
    }
    
    return(result_df)
  }
  predicted_data <- predict_schooling_multiple(result)

  
  
  
  
  
  

  

  
  # calculate labor -------------------------------------------------------------------
  
  calculate_multi_country_lta <- function(data, target_years = 2015:2050) {
    results_list <- list()
    country_age_combinations <- unique(data[, c("country", "age_grp")])
    for(i in 1:nrow(country_age_combinations)) {
      current_country <- country_age_combinations$country[i]
      current_age_grp <- country_age_combinations$age_grp[i]
      subset_data <- subset(data, 
                            country == current_country & 
                              age_grp == current_age_grp)
      if(nrow(subset_data) >= 3) {  
        subset_data$logit_lta <- log(subset_data$lta/(1-subset_data$lta))
        tryCatch({
          fit <- lm(logit_lta ~ year, data = subset_data)
          pred_df <- data.frame(year = target_years)
          pred_logit <- predict(fit, newdata = pred_df)
          pred_lta <- exp(pred_logit)/(1 + exp(pred_logit))
          result_df <- data.frame(
            country = current_country,
            age_grp = current_age_grp,
            year = target_years,
            lta = pred_lta
          )
          results_list[[length(results_list) + 1]] <- result_df
          
        }, error = function(e) {
          warning(paste("预测失败:", current_country, current_age_grp, "- 错误:", e$message))
          last_value <- subset_data$lta[which.max(subset_data$year)]
          result_df <- data.frame(
            country = current_country,
            age_grp = current_age_grp,
            year = target_years,
            lta = last_value
          )
          results_list[[length(results_list) + 1]] <- result_df
        })
      } else {
        warning(paste("数据点不足:", current_country, current_age_grp, 
                      "- 仅有", nrow(subset_data), "个观测值"))
        if(nrow(subset_data) > 0) {
          last_value <- subset_data$lta[which.max(subset_data$year)]
          result_df <- data.frame(
            country = current_country,
            age_grp = current_age_grp,
            year = target_years,
            lta = last_value
          )
          results_list[[length(results_list) + 1]] <- result_df
        }
      }
    }
    final_result <- do.call(rbind, results_list)
    
    return(final_result)
  }
  lta_results <- calculate_multi_country_lta(lta_dt.124)
  
  
  
  
  
  # calculate human capital --------------------------------------------------------------------
  
  calculate_multi_country_Ht <- function(hta_data, lta_data, Nta_data) {
    hta_dt <- data.frame(hta_data)
    final_lta_dt <- data.frame(lta_data)
    Nta_dt <- data.frame(Nta_data)
    if(is.character(final_lta_dt$year)) 
      final_lta_dt$year <- as.integer(final_lta_dt$year)
    if(is.character(Nta_dt$year)) 
      Nta_dt$year <- as.integer(Nta_dt$year)
    if(is.character(hta_dt$year)) 
      hta_dt$year <- as.integer(hta_dt$year)
    if(!is.numeric(hta_dt$hta))
      hta_dt$hta <- as.numeric(as.character(hta_dt$hta))
    if(!is.numeric(final_lta_dt$lta))
      final_lta_dt$lta <- as.numeric(as.character(final_lta_dt$lta))
    if(!is.numeric(Nta_dt$Nta))
      Nta_dt$Nta <- as.numeric(as.character(Nta_dt$Nta))
    Hta_dt1 <- dplyr::left_join(hta_dt, final_lta_dt, by = c("country", "age_grp", "year"))
    Hta_dt <- dplyr::left_join(Hta_dt1, Nta_dt, by = c("country", "age_grp", "year"))
    Hta_dt$Hta <- Hta_dt$hta * Hta_dt$lta * Hta_dt$Nta
    Ht_dt <- stats::aggregate(Hta ~ country + year, data = Hta_dt, FUN = sum, na.rm = TRUE)
    names(Ht_dt)[names(Ht_dt) == "Hta"] <- "Ht"
    return(list(Hta_detailed = Hta_dt, Ht_summary = Ht_dt))
  }
  results <- calculate_multi_country_Ht(
    hta_dt.124,
    final_lta_dt.124, 
    Nta_dt.124
  )

  
  
  
  


  
  # calculate physical capital --------------------------------------------------------------------
  
  calculate_multiple_countries_capital <- function(Kt.124, sav.rate.124, Yt_dt.124, delta = 0.05) {
    result_data <- data.frame()
    countries <- unique(Yt_dt.124$country)
    for (country_name in countries) {
      tryCatch({
        initial_K_row <- Kt.124[Kt.124$country == country_name & Kt.124$year == 2015, "Kt"]
        initial_K <- as.numeric(initial_K_row[1])
        if (length(initial_K) == 0 || is.na(initial_K)) {
          warning(paste("缺少国家", country_name, "的初始资本存量数据，跳过计算"))
          next
        }
        savings_rate_row <- sav.rate.124[sav.rate.124$country == country_name, "avg.2010_2019"]
        savings_rate <- as.numeric(savings_rate_row[1])
        if (length(savings_rate) == 0 || is.na(savings_rate)) {
          warning(paste("缺少国家", country_name, "的储蓄率数据，跳过计算"))
          next
        }
        country_gdp <- Yt_dt.124[Yt_dt.124$country == country_name, ]
        years <- 2015:2050
        country_K <- numeric(length(years))
        country_K[1] <- initial_K  
        for (i in 2:length(years)) {
          prev_year <- years[i-1]
          prev_gdp_row <- country_gdp[country_gdp$year == prev_year, "Yt"]
          if (length(prev_gdp_row) > 0) {
            prev_gdp <- as.numeric(prev_gdp_row[1])
          } else {
            warning(paste("国家", country_name, "在", prev_year, "年缺少GDP数据"))
            prev_gdp <- 0
          }
          if (!is.numeric(country_K[i-1]) || is.na(country_K[i-1])) {
            country_K[i-1] <- 0
            warning(paste("国家", country_name, "在", prev_year, "年的资本存量不是数值，已设为0"))
          }
          country_K[i] <- (1 - delta) * country_K[i-1] + savings_rate * prev_gdp
        }
        country_Kt_dt <- data.frame(
          country = country_name,
          year = years,
          Kt = country_K
        )
        result_data <- rbind(result_data, country_Kt_dt)
        
      }, error = function(e) {
        warning(paste("计算国家", country_name, "的资本存量时出错:", e$message))
      })
    }
    
    return(result_data)
  }
  
  
  
  
 
  
  
  
  

  # get mortality ----------------------------------------------------------------
  log_linear_predict_mortality_df_improved <- function(mort_data, countries = NULL, diseases = NULL) {
    if(is.null(countries)) {
      countries <- unique(mort_data$country)
    }
    if(is.null(diseases)) {
      diseases <- unique(mort_data$disease)
    }
    age_label <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-999")
    mort_m <- data.frame()
    model_years <- 2010:2019
    pred_years <- 2020:2050
    newdata <- data.frame(year = pred_years)
    total_combinations <- length(countries) * length(diseases)
    counter <- 0
    for(country_i in countries) {
      for(disease_i in diseases) {
        counter <- counter + 1
        message(sprintf("处理组合 %d/%d: %s - %s", counter, total_combinations, country_i, disease_i))
        mort_sub <- mort_data %>% 
          filter(country == country_i, disease == disease_i, year >= 2010, year <= 2019)
        if(nrow(mort_sub) == 0) {
          next
        }
        
        for(agei in age_label) {
          mort_sub_age <- subset(mort_sub, age_grp == agei)
          pred_values <- numeric(length(pred_years))
          if(nrow(mort_sub_age) == 0) {
            next
          }
          if(all(mort_sub_age$val == 0)) {
            pred_values <- rep(0, length(pred_years))
          }
          else {
            nonzero_years <- mort_sub_age$year[mort_sub_age$val > 0]
            if(length(nonzero_years) < nrow(mort_sub_age)) {
              min_nonzero_year <- min(nonzero_years)
              max_nonzero_year <- max(nonzero_years)
              mort_sub_age_filtered <- mort_sub_age %>% 
                filter(year >= min_nonzero_year, year <= max_nonzero_year, val > 0)
              if(nrow(mort_sub_age_filtered) >= 2) {
                tryCatch({
                  fit <- lm(log(val) ~ year, data = mort_sub_age_filtered)
                  pred_values <- exp(predict(fit, newdata = newdata))
                  if(pred_values[length(pred_years)] > 2 * pred_values[1]) {
                    base_value <- pred_values[1]  
                    for(i in 2:length(pred_years)) {
                      years_after_2020 <- i - 1
                      pred_values[i] <- base_value * (1.02)^years_after_2020
                    }
                  }
                }, error = function(e) {
                  last_val <- tail(mort_sub_age_filtered$val, 1)
                  pred_values <- rep(last_val, length(pred_years))
                })
              } else {
                pred_values <- rep(mean(mort_sub_age_filtered$val), length(pred_years))
              }
            }
            else {
              tryCatch({
                fit <- lm(log(val) ~ year, data = mort_sub_age)
                pred_values <- exp(predict(fit, newdata = newdata))
                if(pred_values[length(pred_years)] > 2 * pred_values[1]) {
                  base_value <- pred_values[1]  
                  for(i in 2:length(pred_years)) {
                    years_after_2020 <- i - 1
                    pred_values[i] <- base_value * (1.02)^years_after_2020
                  }
                }
              }, error = function(e) {
                pred_values <- rep(mean(mort_sub_age$val), length(pred_years))
              })
            }
          }
          
          temp_df <- data.frame(
            country = country_i,
            disease = disease_i,
            year = pred_years,
            age_grp = agei,
            val = pred_values
          )
          
          mort_m <- rbind(mort_m, temp_df)
        }
      }
    }
    
    return(mort_m)
  }
  
  predicted_mortality <- log_linear_predict_mortality_df_improved(result)
  

  
  
  # calculate counterfactual population -------------------------------------------------------------

  calculate_bar_Nta <- function(mort_data,  
                                pop_data, 
                                countries = NULL, 
                                diseases = NULL,
                                parallel = TRUE,
                                cores = NULL,
                                return_format = "dataframe") {  
    
    library(dplyr)
    if(parallel) {
      library(parallel)
      library(foreach)
      library(doParallel)
    }
    if(is.null(countries)) countries <- unique(mort_data$country)
    if(is.null(diseases)) diseases <- unique(mort_data$disease)
    combinations <- expand.grid(
      country = countries,
      disease = diseases,
      stringsAsFactors = FALSE
    )
    age_label <- c("15-19", "20-24", "25-29", "30-34", "35-39", 
                   "40-44", "45-49", "50-54", "55-59", "60-64", "65-999")

    years <- 2015:2050
    process_combination <- function(country_i, disease_i) {
      message(sprintf("处理: %s - %s", country_i, disease_i))
      Nta_dt <- pop_data %>% 
        filter(country == country_i)
      mort_sub <- mort_data %>% 
        filter(country == country_i, disease == disease_i)
      Nta_m <- matrix(NA, nrow = length(years), ncol = length(age_label))
      rownames(Nta_m) <- years
      colnames(Nta_m) <- age_label
      for(i in 1:nrow(Nta_m)) {
        for(j in 1:ncol(Nta_m)) {
          year_val <- as.character(rownames(Nta_m)[i])
          age_val <- colnames(Nta_m)[j]
          pop_entry <- Nta_dt %>% 
            filter(year == year_val, age_grp == age_val)
          if(nrow(pop_entry) > 0) {
            Nta_m[i,j] <- pop_entry$Nta[1]
          } else {
            Nta_m[i,j] <- NA
          }
        }
      }
      temp_rownames <- rownames(Nta_m)
      temp_colnames <- colnames(Nta_m)
      rownames(Nta_m) <- 0:(length(years)-1)
      colnames(Nta_m) <- 0:(length(age_label)-1)
      bar_Nta_m <- matrix(NA, nrow = length(years), ncol = length(age_label))
      rownames(bar_Nta_m) <- 0:(length(years)-1)
      colnames(bar_Nta_m) <- 0:(length(age_label)-1)
      bar_Nta_m[,"0"] <- Nta_m[,"0"]  
      bar_Nta_m["0",] <- Nta_m["0",]  
      mort_lookup <- mort_sub %>%
        select(year, age_grp, val) %>%
        mutate(year_idx = match(as.character(year), as.character(years)) - 1,
               age_idx = match(age_grp, age_label) - 1) %>%
        select(year_idx, age_idx, val)
      max_row <- nrow(bar_Nta_m) - 1
      max_col <- ncol(bar_Nta_m) - 1
      for(t in 1:max_row) {
        for(a in 1:max_col) {
          min_sub <- min(t,a) - 1  
          mort_val <- c()
          for(k in 0:min_sub) {
            t_idx <- t - 1 - k  
            a_idx <- a - 1 - k  
            mort_entry <- mort_lookup %>%
              filter(year_idx == t_idx, age_idx == a_idx)
            
            if(nrow(mort_entry) > 0) {
              mort_val <- c(mort_val, mort_entry$val[1])
            } else {
              mort_val <- c(mort_val, 0)  
            }
          }
          if(all(mort_val == 0)) {
            bar_Nta_m[as.character(t), as.character(a)] <- Nta_m[as.character(t), as.character(a)]
          } else {
            survival_factors <- 1 - mort_val/10^5  
            bar_Nta_m[as.character(t), as.character(a)] <- 
              Nta_m[as.character(t), as.character(a)] / prod(survival_factors)
          }
        }
      }
      result_matrix <- bar_Nta_m
      rownames(result_matrix) <- temp_rownames
      colnames(result_matrix) <- temp_colnames
      return(list(
        country = country_i,
        disease = disease_i,
        bar_Nta = result_matrix
      ))
    }
    results_raw <- list()
    
    if(parallel) {
      if(is.null(cores)) cores <- detectCores() - 1
      cl <- makeCluster(cores)
      registerDoParallel(cl)
      results_raw <- foreach(i = 1:nrow(combinations), 
                             .packages = c("dplyr")) %dopar% {
                               combo <- combinations[i,]
                               process_combination(combo$country, combo$disease)
                             }
      
      stopCluster(cl)
    } else {
      for(i in 1:nrow(combinations)) {
        combo <- combinations[i,]
        result <- process_combination(combo$country, combo$disease)
        results_raw[[i]] <- result
      }
    }
    if(return_format == "dataframe") {
      results_df <- data.frame()
      
      for(result in results_raw) {
        if(!is.null(result)) {
          country_name <- result$country
          disease_name <- result$disease
          matrix_data <- result$bar_Nta
          for(year in rownames(matrix_data)) {
            for(age in colnames(matrix_data)) {
              population <- matrix_data[year, age]
              results_df <- rbind(results_df, data.frame(
                country = country_name,
                disease = disease_name,
                year = as.numeric(year),
                age_group = age,
                population = population
              ))
            }
          }
        }
      }
      return(results_df)
      
    } else if(return_format == "matrix" && length(countries) == 1 && length(diseases) == 1) {
      if(length(results_raw) > 0 && !is.null(results_raw[[1]])) {
        return(results_raw[[1]]$bar_Nta)
      } else {
        return(NULL)
      }
      
    } else {
      organized_results <- list()
      for(result in results_raw) {
        if(!is.null(result)) {
          country_name <- result$country
          disease_name <- result$disease
          if(!country_name %in% names(organized_results)) {
            organized_results[[country_name]] <- list()
          }
          organized_results[[country_name]][[disease_name]] <- result$bar_Nta
        }
      }
      return(organized_results)
    }
  }

  
  
  
  
  
  
  
  
  
  
  # calculate counterfactual labor -------------------------------------------------------------
 
  calculate_bar_lta_efficient <- function(lta_data, mort_data, pi_data, zeta_data, 
                                          countries = NULL, diseases = NULL, 
                                          age_label, years = 2015:2050,
                                          num_cores = 19) {
    library(data.table)
    library(parallel)
    library(pbapply)
    library(doParallel)
    lta_dt <- as.data.table(lta_data)
    mort_dt <- as.data.table(mort_data)
    pi_dt <- as.data.table(pi_data) 
    zeta_dt <- as.data.table(zeta_data)
    setkey(lta_dt, country, age_grp, year)
    setkey(mort_dt, country, disease, age_grp, year)
    setkey(pi_dt, country, disease, age_grp)
    setkey(zeta_dt, country, disease, age_grp)
    if (!is.null(countries)) {
      lta_dt <- lta_dt[country %in% countries]
      mort_dt <- mort_dt[country %in% countries]
      pi_dt <- pi_dt[country %in% countries]
      zeta_dt <- zeta_dt[country %in% countries]
    }
    if (!is.null(diseases)) {
      mort_dt <- mort_dt[disease %in% diseases]
      pi_dt <- pi_dt[disease %in% diseases]
      zeta_dt <- zeta_dt[disease %in% diseases]
    }
    unique_combinations <- CJ(
      country = unique(lta_dt$country),
      disease = unique(mort_dt$disease)
    )
    num_combinations <- nrow(unique_combinations)
    message(sprintf("需要计算 %d 个国家-疾病组合", num_combinations))
    
    cl <- makeCluster(min(num_cores, num_combinations, detectCores() - 1))
    registerDoParallel(cl)
    clusterExport(cl, c("lta_dt", "mort_dt", "pi_dt", "zeta_dt", 
                        "age_label", "years"), envir = environment())
    clusterEvalQ(cl, {
      library(data.table)
    })
    process_combination <- function(idx, combo_data, progress_callback = NULL) {
      country_val <- combo_data$country[idx]
      disease_val <- combo_data$disease[idx]
      lta_subset <- lta_dt[country == country_val]
      mort_subset <- mort_dt[country == country_val & disease == disease_val]
      pi_subset <- pi_dt[country == country_val & disease == disease_val]
      zeta_subset <- zeta_dt[country == country_val & disease == disease_val]
      lta_m <- matrix(NA, nrow = length(years), ncol = length(age_label))
      mort_m <- matrix(NA, nrow = length(years), ncol = length(age_label))
      pi_m <- matrix(NA, nrow = 1, ncol = length(age_label))
      theta_m <- matrix(NA, nrow = 1, ncol = length(age_label))
      for (i in 1:length(age_label)) {
        curr_age <- age_label[i]
        pi_val <- pi_subset[age_grp == curr_age, pi]
        if (length(pi_val) > 0) pi_m[1, i] <- pi_val[1]
        zeta_val <- zeta_subset[age_grp == curr_age, zeta]
        if (length(zeta_val) > 0) theta_m[1, i] <- zeta_val[1]
        for (j in 1:length(years)) {
          curr_year <- years[j]
          
          lta_val <- lta_subset[age_grp == curr_age & year == curr_year, lta]
          if (length(lta_val) > 0) lta_m[j, i] <- lta_val[1]
          
          mort_val <- mort_subset[age_grp == curr_age & year == curr_year, mort]
          if (length(mort_val) > 0) mort_m[j, i] <- mort_val[1]
        }
      }
      rownames(lta_m) <- rownames(mort_m) <- 0:(length(years)-1)
      colnames(lta_m) <- colnames(mort_m) <- colnames(pi_m) <- colnames(theta_m) <- 0:(length(age_label)-1)
      bar_lta_m <- matrix(NA, nrow = length(years), ncol = length(age_label))
      rownames(bar_lta_m) <- 0:(length(years)-1)
      colnames(bar_lta_m) <- 0:(length(age_label)-1)
      bar_lta_m[,"0"] <- lta_m[,"0"]
      bar_lta_m["0",] <- lta_m["0",]
      max_row <- nrow(bar_lta_m) - 1
      max_col <- ncol(bar_lta_m) - 1
      
      for(t in 1:max_row) {
        for(a in 1:max_col) {
          min_sub <- min(t,a)-1
          
          if(min_sub >= 0) {
            prod_vals <- numeric(min_sub + 1)
            
            for(tau in 0:min_sub) {
              t_idx <- as.character(t-1-tau)
              a_idx <- as.character(a-1-tau)
              tau_idx <- as.character(tau)
              a_tau_idx <- as.character(a-1-tau)
              
              if(!is.na(mort_m[t_idx, a_idx]) && !is.na(pi_m[1, tau_idx]) && !is.na(theta_m[1, a_tau_idx])) {
                prod_vals[tau + 1] <- 1 - (mort_m[t_idx, a_idx]/10^5) * pi_m[1, tau_idx] * theta_m[1, a_tau_idx]
              } else {
                prod_vals[tau + 1] <- 1 
              }
            }
            
            if(!any(is.na(prod_vals)) && !any(prod_vals == 0)) {
              t_str <- as.character(t)
              a_str <- as.character(a)
              bar_lta_m[t_str, a_str] <- lta_m[t_str, a_str] / prod(prod_vals)
            } else {
              bar_lta_m[as.character(t), as.character(a)] <- lta_m[as.character(t), as.character(a)]
            }
          } else {
            bar_lta_m[as.character(t), as.character(a)] <- lta_m[as.character(t), as.character(a)]
          }
        }
      }
      result_size <- (max_row + 1) * (max_col + 1)
      result_dt <- data.table(
        country = rep(country_val, result_size),
        disease = rep(disease_val, result_size),
        year = rep(NA_integer_, result_size),
        age_grp = rep(NA_character_, result_size),
        lta = rep(NA_real_, result_size),
        bar_lta = rep(NA_real_, result_size)
      )
      row_idx <- 1
      for(t in 0:max_row) {
        for(a in 0:max_col) {
          result_dt[row_idx, `:=`(
            year = years[t+1],
            age_grp = age_label[a+1],
            lta = lta_m[as.character(t), as.character(a)],
            bar_lta = bar_lta_m[as.character(t), as.character(a)]
          )]
          row_idx <- row_idx + 1
        }
      }
      
      return(result_dt)
    }
    message("开始并行计算，请稍候...")
    pboptions(type = "txt", char = "=", style = 3)
    results_list <- pblapply(1:nrow(unique_combinations), function(idx) {
      process_combination(idx, unique_combinations)
    }, cl = cl)
    stopCluster(cl)
    message("合并结果...")
    final_result <- rbindlist(results_list)
    
    message(sprintf("计算完成! 总共处理了 %d 个国家-疾病组合，共 %d 条记录", 
                    nrow(unique_combinations), nrow(final_result)))
    return(final_result)
  }

  
  
  
  # calculate counterfactual human capital -------------------------------------------------------------
  library(parallel)
  library(foreach)
  library(doParallel)
  library(dplyr)
  calculate_multi_bar_Ht_parallel <- function(Hta_dt, Mta_m, num_cores = 19) {
    countries <- unique(Hta_dt$country)
    diseases <- unique(Mta_m$disease)
    country_disease_pairs <- expand.grid(
      country = countries,
      disease = diseases,
      stringsAsFactors = FALSE
    )
    cl <- makeCluster(min(num_cores, nrow(country_disease_pairs), detectCores() - 1))
    registerDoParallel(cl)
    clusterExport(cl, c("Hta_dt", "Mta_m"), envir = environment())
    result_list <- foreach(i = 1:nrow(country_disease_pairs), 
                           .packages = c("dplyr", "stats")) %dopar% {
                             
                             country_i <- country_disease_pairs$country[i]
                             disease_i <- country_disease_pairs$disease[i]
                             country_Hta_dt <- subset(Hta_dt, country == country_i)
                             disease_Mta <- subset(Mta_m, country == country_i & disease == disease_i)
                             bar_Hta_dt <- merge(country_Hta_dt, disease_Mta, 
                                                 by = c("country", "year", "age_grp"), 
                                                 all.x = TRUE)
                             bar_Hta_dt$Mta[is.na(bar_Hta_dt$Mta)] <- 0
                             bar_Hta_dt$delta_Hta <- bar_Hta_dt$Hta * bar_Hta_dt$Mta
                             bar_Hta_dt$bar_Hta <- bar_Hta_dt$Hta + bar_Hta_dt$delta_Hta
                             bar_Ht_dt <- aggregate(bar_Hta ~ year + country, data = bar_Hta_dt, FUN = sum)
                             names(bar_Ht_dt)[3] <- "bar_Ht"
                             bar_Ht_dt$disease <- disease_i
                             return(bar_Ht_dt[, c("country", "disease", "year", "bar_Ht")])
                           }

    stopCluster(cl)
    result_df <- do.call(rbind, result_list)
    
    return(result_df)
  }
  unique(Hta_dt$country)
  system.time({
    bar_Ht_results <- calculate_multi_bar_Ht_parallel(Hta_dt, Mta_m, num_cores = 19)
  })
  
  
  
  
  
  
  
 
  
  
  
  
  
  
  
  
  
  # calculate bar_Kt,bar_Yt ---------------------------------------------------------

  calculate_complete_results_with_ci <- function(
    data_prevalence,        
    sav_rate_data,          
    Kt_initial_data,        
    bar_Ht_data,            
    Ht_data,                
    At_data,                
    start_year = 2015,      
    end_year = 2050,        
    TC_start_year = 2016,   
    alpha = alpha,         
    delta = delta, 
    chi_factor = 1,         
    rou = 1,                
    cores = cores_to_use,   
    bootstrap_samples = 1000 
  ) {
    years <- start_year:end_year
    country_disease_pairs <- unique(data_prevalence[, c("country", "disease")])
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    set.seed(12345)
    results <- foreach(
      i = 1:nrow(country_disease_pairs),
      .packages = c("dplyr", "tidyr"),
      .combine = "rbind",
      .export = c("chi_factor", "alpha", "delta", "rou")
    ) %dopar% {
      current_country <- country_disease_pairs$country[i]
      current_disease <- country_disease_pairs$disease[i]
      cost_data <- data_prevalence %>%
        filter(country == current_country, disease == current_disease)
      TC_start <- cost_data$disease.expend[1]  
      TC_rate <- cost_data$growth.2000.2016[1] 
      has_ci <- "disease.expend.lower" %in% names(cost_data) &&
        "disease.expend.upper" %in% names(cost_data)
      
      if (has_ci) {
        TC_start_lower <- cost_data$disease.expend.lower[1]  # 置信区间下限
        TC_start_upper <- cost_data$disease.expend.upper[1]  # 置信区间上限
      }
      current_sav_rate <- sav_rate_data %>%
        filter(country == current_country) %>%
        pull(avg.2010_2019) %>%
        first()
      chi <- current_sav_rate * chi_factor  
      Kt_initial <- Kt_initial_data %>%
        filter(country == current_country, year == start_year) %>%
        pull(Kt) %>%
        first()
      bar_Ht_values <- bar_Ht_data %>%
        filter(country == current_country, disease == current_disease, year %in% years) %>%
        arrange(year) %>%
        pull(bar_Ht)
      
      Ht_values <- Ht_data %>%
        filter(country == current_country, year %in% years) %>%
        arrange(year) %>%
        pull(Ht)
      
      At_values <- At_data %>%
        filter(country == current_country, year %in% years) %>%
        arrange(year) %>%
        pull(At)
      if(length(bar_Ht_values) != length(years) ||
         length(Ht_values) != length(years) ||
         length(At_values) != length(years) ||
         is.na(Kt_initial)) {
        warning(paste("国家:", current_country, "疾病:", current_disease, "的数据不完整"))
        return(NULL)
      }
      TC_years <- TC_start_year:end_year
      TC_values <- TC_start * (1 + TC_rate)^(TC_years - TC_start_year)
      TC_full <- c(0, TC_values)
      names(TC_full) <- years
      Kt <- numeric(length(years))
      Yt <- numeric(length(years))
      Kt[1] <- Kt_initial
      Yt[1] <- At_values[1] * (Ht_values[1]^(1-alpha)) * (Kt[1]^alpha)
      for (j in 2:length(years)) {
        Kt[j] <- current_sav_rate * Yt[j-1] + (1-delta) * Kt[j-1]
        Yt[j] <- At_values[j] * (Ht_values[j]^(1-alpha)) * (Kt[j]^alpha)
      }
      bar_Kt <- numeric(length(years))
      bar_Yt <- numeric(length(years))
      bar_Kt[1] <- Kt_initial  
      bar_Yt[1] <- At_values[1] * (bar_Ht_values[1]^(1-alpha)) * (bar_Kt[1]^alpha)
      for (j in 2:length(years)) {
        bar_Kt[j] <- current_sav_rate * bar_Yt[j-1] + chi * TC_full[j-1] * rou + (1-delta) * bar_Kt[j-1]
        bar_Yt[j] <- At_values[j] * (bar_Ht_values[j]^(1-alpha)) * (bar_Kt[j]^alpha)
      }
      diff_Kt <- bar_Kt - Kt
      diff_Yt <- bar_Yt - Yt
      if (has_ci && !is.na(TC_start_lower) && !is.na(TC_start_upper)) {
        bar_Yt_bootstrapped <- matrix(NA, nrow = bootstrap_samples, ncol = length(years))
        for(b in 1:bootstrap_samples) {
          TC_start_bootstrap <- runif(1, TC_start_lower, TC_start_upper)
          TC_values_bootstrap <- TC_start_bootstrap * (1 + TC_rate)^(TC_years - TC_start_year)
          TC_full_bootstrap <- c(0, TC_values_bootstrap)
          bar_Kt_bootstrap <- numeric(length(years))
          bar_Yt_bootstrap <- numeric(length(years))
          bar_Kt_bootstrap[1] <- Kt_initial
          bar_Yt_bootstrap[1] <- At_values[1] * (bar_Ht_values[1]^(1-alpha)) * (bar_Kt_bootstrap[1]^alpha)
          for (j in 2:length(years)) {
            bar_Kt_bootstrap[j] <- current_sav_rate * bar_Yt_bootstrap[j-1] +
              chi * TC_full_bootstrap[j-1] * rou +
              (1-delta) * bar_Kt_bootstrap[j-1]
            bar_Yt_bootstrap[j] <- At_values[j] * (bar_Ht_values[j]^(1-alpha)) * (bar_Kt_bootstrap[j]^alpha)
          }
          bar_Yt_bootstrapped[b,] <- bar_Yt_bootstrap
        }
        bar_Yt_lower <- apply(bar_Yt_bootstrapped, 2, function(x) quantile(x, 0.025, na.rm = TRUE))
        bar_Yt_upper <- apply(bar_Yt_bootstrapped, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
      } else {
        bar_Yt_lower <- rep(NA, length(years))
        bar_Yt_upper <- rep(NA, length(years))
      }
      data.frame(
        country = current_country,
        disease = current_disease,
        year = years,
        bar_Kt = bar_Kt,
        bar_Yt = bar_Yt,
        bar_Yt_lower = bar_Yt_lower,
        bar_Yt_upper = bar_Yt_upper,
        Kt = Kt,
        Yt = Yt,
        diff_Kt = diff_Kt,
        diff_Yt = diff_Yt,
        rou = rou
      )
    }
    stopCluster(cl)
    return(results)
  }
  
  
  
  
  
  
  
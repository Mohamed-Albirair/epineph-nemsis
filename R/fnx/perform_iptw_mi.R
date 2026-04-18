
# Function to perform IPTW with multiple imputation (using glm with robust SE)
perform_iptw_mi <- function(mids_obj,
                            outcome_var,
                            treatment_var = "epineph",
                            confounders   = c("age", "sex", "rythm", "ems",
                                              "mechanism", "bt", "defib"),
                            trim_weights  = TRUE) {

      n_imp <- mids_obj$m
      results_list <- list()

      for (i in 1:n_imp) {
            # Get complete dataset for this imputation
            data_complete <- mice::complete(mids_obj, i)

            # Formula for propensity score model
            ps_formula <- as.formula(paste(treatment_var, "~",
                                           paste(confounders, collapse = " + ")))

            # Fit propensity score model
            ps_model <- glm(ps_formula, family = binomial, data = data_complete)

            # Calculate propensity scores and weights
            data_complete$ps <- predict(ps_model, type = "response")
            data_complete$weight <- ifelse(data_complete[[treatment_var]] == 1,
                                           1 / data_complete$ps,
                                           1 / (1 - data_complete$ps))

            # Trim extreme weights (optional, but recommended)
            if (trim_weights) {
                  weight_cutoff <- quantile(data_complete$weight, 0.99, na.rm = TRUE)
                  data_complete$weight <- pmin(data_complete$weight, weight_cutoff)
            }

            # Fit weighted outcome model using glm
            outcome_formula <- as.formula(paste(outcome_var, "~", treatment_var))
            weighted_model <- glm(outcome_formula,
                                  data = data_complete,
                                  family = binomial(link = "log"), # poisson,
                                  weights = weight)

            # Get robust standard errors using sandwich estimator
            library(sandwich)
            library(lmtest)
            robust_vcov <- sandwich::vcovHC(weighted_model, type = "HC3")
            robust_se <- sqrt(diag(robust_vcov))

            # Store results with robust SE
            model_summary <- broom::tidy(weighted_model)
            model_summary$std.error <- robust_se

            # Calculate confidence intervals
            crit_val <- qnorm(0.975)
            model_summary$conf.low <- model_summary$estimate - crit_val * robust_se
            model_summary$conf.high <- model_summary$estimate + crit_val * robust_se

            results_list[[i]] <- model_summary
      }

      # Pool results across imputations using Rubin's rules
      pool_mi_results <- function(results_list) {
            m     <- length(results_list)
            terms <- results_list[[1]]$term

            pooled_results <- data.frame()

            for (term_idx in seq_along(terms)) {
                  term      <- terms[term_idx]

                  # Extract estimates and variances for this term across imputations
                  estimates <- sapply(results_list, function(x) x$estimate[term_idx])
                  variances <- sapply(results_list, function(x) x$std.error[term_idx]^2)

                  # Rubin's rules
                  Q_bar     <- mean(estimates)  # Pooled estimate
                  U_bar     <- mean(variances)  # Within-imputation variance
                  B         <- var(estimates)   # Between-imputation variance

                  # Total variance
                  T_total   <- U_bar + (1 + 1 / m) * B
                  se_pooled <- sqrt(T_total)

                  # Degrees of freedom for t-distribution
                  df        <- (m - 1) * (1 + (U_bar / ((1 + 1 / m) * B))) ^ 2

                  # If B is 0 (all estimates identical), use simpler formula
                  if (B == 0) {
                        df <- Inf
                  }

                  # Confidence intervals and p-value
                  t_mult   <- qt(0.975, df = df)
                  ci_lower <- Q_bar - t_mult * se_pooled
                  ci_upper <- Q_bar + t_mult * se_pooled
                  t_stat   <- Q_bar / se_pooled
                  p_value  <- 2 * pt(-abs(t_stat), df = df)

                  pooled_results <- rbind(
                        pooled_results,
                        data.frame(term      = term,
                                   estimate  = exp(Q_bar),
                                   conf.low  = exp(ci_lower),
                                   conf.high = exp(ci_upper),
                                   p.value   = p_value,
                                   std.error = se_pooled,
                                   statistic = t_stat,
                                   df        = df)
                  )
            }

            return(pooled_results)
      }

      pooled_result <- pool_mi_results(results_list)
      return(pooled_result)
}

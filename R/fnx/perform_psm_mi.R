
# Propensity Score Matching with Multiple Imputation
perform_psm_mi <- function(mids_obj,
                           outcome_var,
                           treatment_var = "epineph",
                           confounders   = c("age", "sex", "rythm", "ems",
                                             "mechanism", "bt", "defib"),
                           ratio         = 1,
                           method        = "nearest",
                           caliper       = 0.2) {

      # library(MatchIt)
      n_imp <- mids_obj$m
      results_list <- list()

      for (i in 1:n_imp) {
            data_complete <- mice::complete(mids_obj, i)

            # Create formula for matching
            match_formula <- as.formula(paste(treatment_var, "~",
                                              paste(confounders, collapse = " + ")))

            # Perform matching
            matched_obj <- MatchIt::matchit(
                  match_formula,
                  data     = data_complete,
                  method   = method,        # "nearest", "optimal", "full", "genetic"
                  distance = "glm",         # logistic regression for PS
                  link     = "logit",
                  ratio    = ratio,         # 1:1, 1:2, etc.
                  caliper  = caliper,       # maximum distance for matching
                  replace  = FALSE,         # without replacement
                  estimand = "ATT"          # Average Treatment effect on the Treated
            )

            # Get matched data
            matched_data <- MatchIt::match.data(matched_obj)

            # Fit outcome model on matched data
            outcome_formula <- as.formula(paste(outcome_var, "~", treatment_var))
            matched_model   <- glm(outcome_formula, data = matched_data, family = poisson)

            # Get robust standard errors
            # library(sandwich)
            robust_vcov <- sandwich::vcovHC(matched_model, type = "HC3")
            robust_se   <- sqrt(diag(robust_vcov))

            # Store results
            model_summary <- broom::tidy(matched_model)
            model_summary$std.error <- robust_se

            # Calculate confidence intervals
            crit_val <- qnorm(0.975)
            model_summary$conf.low <- model_summary$estimate - crit_val * robust_se
            model_summary$conf.high <- model_summary$estimate + crit_val * robust_se

            # Add matching diagnostics
            model_summary$n_treated <- sum(matched_data[[treatment_var]] == 1)
            model_summary$n_control <- sum(matched_data[[treatment_var]] == 0)
            model_summary$n_dropped <- nrow(data_complete) - nrow(matched_data)

            results_list[[i]] <- model_summary
      }

      # Pool results (using same pooling function as before)
      pooled_result <- pool_mi_results(results_list)

      # Add overall matching diagnostics
      pooled_result$method <- paste("PSM", method, paste0(ratio, ":1"))

      return(pooled_result)
}

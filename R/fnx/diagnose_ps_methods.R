
# Diagnose PS methods
diagnose_ps_methods <- function(mids_obj,
                                treatment_var = "epineph",
                                confounders = c("age", "sex", "rythm", "ems",
                                                "mechanism", "bt", "defib"),
                                outcome_var = NULL) {  # Optional, for final check

      n_imp <- mids_obj$m

      # Store diagnostics across imputations
      diagnostics <- list()
      all_ps_data <- data.frame()

      for (i in 1:n_imp) {
            # Get imputed dataset
            data_i <- mice::complete(mids_obj, i)
            data_i$imputation <- i

            # Fit propensity score model
            ps_formula <- as.formula(paste(treatment_var, "~",
                                           paste(confounders, collapse = " + ")))
            ps_model <- glm(ps_formula, data = data_i, family = binomial())

            # Calculate propensity scores
            data_i$ps <- predict(ps_model, type = "response")

            # Calculate weights
            data_i$weight <- ifelse(data_i[[treatment_var]] == 1,
                                    1 / data_i$ps,
                                    1 / (1 - data_i$ps))

            # Store for this imputation
            all_ps_data <- rbind(all_ps_data,
                                 data_i[, c("imputation", treatment_var, "ps", "weight")])

            ## PSM Diagnostics
            # Attempt matching to see how many would be dropped
            tryCatch({
                  match_obj <- MatchIt::matchit(
                        ps_formula,
                        data = data_i,
                        method = "nearest",
                        distance = "glm",
                        ratio = 1,
                        caliper = 0.2,
                        estimand = "ATT"
                  )

                  matched_data <- MatchIt::match.data(match_obj)

                  diagnostics[[i]] <- list(
                        imputation = i,

                        # PS distribution
                        ps_treated = summary(data_i$ps[data_i[[treatment_var]] == 1]),
                        ps_control = summary(data_i$ps[data_i[[treatment_var]] == 0]),

                        # Overlap metrics
                        min_treated = min(data_i$ps[data_i[[treatment_var]] == 1]),
                        max_treated = max(data_i$ps[data_i[[treatment_var]] == 1]),
                        min_control = min(data_i$ps[data_i[[treatment_var]] == 0]),
                        max_control = max(data_i$ps[data_i[[treatment_var]] == 0]),

                        # Common support
                        overlap_lower = max(min(data_i$ps[data_i[[treatment_var]] == 1]),
                                            min(data_i$ps[data_i[[treatment_var]] == 0])),
                        overlap_upper = min(max(data_i$ps[data_i[[treatment_var]] == 1]),
                                            max(data_i$ps[data_i[[treatment_var]] == 0])),

                        # Percent off-support
                        pct_treated_off = mean(data_i$ps[data_i[[treatment_var]] == 1] <
                                                     max(min(data_i$ps[data_i[[treatment_var]] == 1]),
                                                         min(data_i$ps[data_i[[treatment_var]] == 0])) |
                                                     data_i$ps[data_i[[treatment_var]] == 1] >
                                                     min(max(data_i$ps[data_i[[treatment_var]] == 1]),
                                                         max(data_i$ps[data_i[[treatment_var]] == 0]))) * 100,
                        pct_control_off = mean(data_i$ps[data_i[[treatment_var]] == 0] <
                                                     max(min(data_i$ps[data_i[[treatment_var]] == 1]),
                                                         min(data_i$ps[data_i[[treatment_var]] == 0])) |
                                                     data_i$ps[data_i[[treatment_var]] == 0] >
                                                     min(max(data_i$ps[data_i[[treatment_var]] == 1]),
                                                         max(data_i$ps[data_i[[treatment_var]] == 0]))) * 100,

                        # Matching diagnostics
                        n_total = nrow(data_i),
                        n_treated = sum(data_i[[treatment_var]] == 1),
                        n_control = sum(data_i[[treatment_var]] == 0),
                        n_matched = nrow(matched_data),
                        n_dropped = nrow(data_i) - nrow(matched_data),
                        pct_dropped = (nrow(data_i) - nrow(matched_data)) / nrow(data_i) * 100,

                        # IPW diagnostics
                        weight_summary = summary(data_i$weight),
                        weight_sd = sd(data_i$weight),
                        weight_max = max(data_i$weight),
                        weight_min = min(data_i$weight),
                        weight_ratio = max(data_i$weight) / min(data_i$weight),
                        weight_cv = sd(data_i$weight) / mean(data_i$weight),

                        # Effective sample size for IPW
                        ess = (sum(data_i$weight)^2) / sum(data_i$weight^2),
                        ess_pct = ((sum(data_i$weight)^2) / sum(data_i$weight^2)) / nrow(data_i) * 100,

                        # Extreme weights (common thresholds)
                        pct_weights_gt_10 = mean(data_i$weight > 10) * 100,
                        pct_weights_gt_20 = mean(data_i$weight > 20) * 100,
                        pct_weights_gt_50 = mean(data_i$weight > 50) * 100
                  )
            }, error = function(e) {
                  diagnostics[[i]] <- list(imputation = i, error = e$message)
            })
      }

      diag_df <- map_dfr(diagnostics, function(x) {
            # Convert to tibble
            df <- as_tibble(x)

            # Convert special classes to numeric
            df <- df %>%
                  mutate(across(where(~ inherits(., "table") | inherits(., "summaryDefault")),
                                as.numeric))
            df
      })

      # Create visualizations
      plots <- list()

      # Plot 1: PS distribution by treatment (first imputation)
      plots[[1]] <- all_ps_data %>%
            filter(imputation == 1) %>%
            ggplot(aes(x = ps, fill = factor(get(treatment_var)))) +
            geom_density(alpha = 0.5) +
            geom_vline(xintercept = c(0.1, 0.9), linetype = "dashed", alpha = 0.5) +
            labs(title = "Propensity Score Distribution (Imputation 1)",
                 x = "Propensity Score", y = "Density",
                 fill = treatment_var) +
            theme_minimal()

      # Plot 2: Weight distribution (IPW diagnostic)
      plots[[2]] <- all_ps_data %>%
            filter(imputation == 1) %>%
            ggplot(aes(x = weight)) +
            geom_histogram(bins = 50, alpha = 0.7) +
            geom_vline(xintercept = 10, color = "red", linetype = "dashed") +
            scale_x_log10() +
            labs(title = "Weight Distribution (log scale)",
                 x = "Weight (log10)", y = "Count") +
            theme_minimal()

      # Plot 3: Overlap assessment
      plots[[3]] <- all_ps_data %>%
            filter(imputation == 1) %>%
            ggplot(aes(x = ps, y = factor(get(treatment_var)),
                       color = factor(get(treatment_var)))) +
            geom_jitter(alpha = 0.3, width = 0, height = 0.2) +
            geom_violin(alpha = 0.2, scale = "width") +
            labs(title = "Propensity Score Overlap",
                 x = "Propensity Score", y = treatment_var) +
            theme_minimal()

      # Summary table for decision
      decision_table <- diag_df %>%
            summarise(
                  # Overlap metrics
                  avg_overlap_lower = mean(overlap_lower),
                  avg_overlap_upper = mean(overlap_upper),
                  avg_pct_treated_off = mean(pct_treated_off),
                  avg_pct_control_off = mean(pct_control_off),

                  # PSM metrics
                  avg_n_matched = mean(n_matched),
                  avg_pct_dropped = mean(pct_dropped),

                  # IPW metrics
                  avg_weight_ratio = mean(weight_ratio),
                  avg_weight_cv = mean(weight_cv),
                  avg_ess = mean(ess),
                  avg_ess_pct = mean(ess_pct),
                  avg_pct_weights_gt_10 = mean(pct_weights_gt_10),
                  avg_pct_weights_gt_20 = mean(pct_weights_gt_20),
                  avg_pct_weights_gt_50 = mean(pct_weights_gt_50)
            )

      return(list(diagnostics = diag_df,
                  decision_table = decision_table,
                  plots = plots,
                  all_ps_data = all_ps_data))
}

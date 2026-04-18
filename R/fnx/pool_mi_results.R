

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
            B         <- var(estimates)       # Between-imputation variance

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

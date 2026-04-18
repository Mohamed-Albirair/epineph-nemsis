
# Manual pooling function for pattern-mixture models
pool_pattern_models <- function(model_list) {
      coefs <- sapply(model_list, coef)
      vars  <- sapply(model_list, function(x) diag(vcov(x)))

      # Rubin's rules
      Q_bar   <- rowMeans(coefs)
      U_bar   <- rowMeans(vars)
      B       <- rowSums((coefs - Q_bar)^2) / (length(model_list) - 1)
      tot_var <- U_bar + (1 + 1 / length(model_list)) * B

      data.frame(term      = names(coef(model_list[[1]])),
                 estimate  = Q_bar,
                 std.error = sqrt(tot_var),
                 statistic = Q_bar / sqrt(tot_var)) %>%
            mutate(p.value   = 2 * pnorm(-abs(statistic)),
                   conf.low  = estimate - 1.96 * sqrt(tot_var),
                   conf.high = estimate + 1.96 * sqrt(tot_var)) %>%
            mutate(across(.cols = c(estimate, conf.low, conf.high),
                          .fns  = ~ exp(.)))
}

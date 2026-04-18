
### Run Final Analysis on All Methods

### Method 1: Complete-case analysis (reference) ###############################

# Define your final model formula
rosc_formula   <- as.formula("rosc   ~ epineph + age + sex + rythm + ems + mechanism + bt + defib")
surv_formula   <- as.formula("surv   ~ epineph + age + sex + rythm + ems + mechanism + bt + defib")
miller_formula <- as.formula("miller ~ epineph + age + sex + rythm + ems + mechanism + bt + defib")
peters_formula <- as.formula("peters ~ epineph + age + sex + rythm + ems + mechanism + bt + defib")
witt_formula   <- as.formula("witt   ~ epineph + age + sex + rythm + ems + mechanism + bt + defib")

## ROSC
rosc_cca   <- glm(rosc_formula,
                  family = poisson(),
                  data   = epi_tca_sbst)
rosc_cca_res <- broom::tidy(rosc_cca, conf.int = TRUE, exponentiate = TRUE)

## Miller
miller_cca <- glm(miller_formula,
                  family = poisson(),
                  data   = epi_tca_sbst)
# miller_cca_res <- broom::tidy(miller_cca, conf.int = TRUE, exponentiate = TRUE)
miller_cca_res <- parameters::parameters(miller_cca, exponentiate = TRUE, ci_method = "wald") %>%
      as.data.frame() %>%
      select(term        = Parameter,
             coefficient = Coefficient,
             std.error   = SE,
             statistic   = z,
             p.value     = p,
             conf.low    = CI_low,
             conf.high   = CI_high)


## Another Streamlined Approach------------------------------------------------#

reg_mod <- epi_tca_sbst %>%
      pivot_longer(cols      = rosc:witt,
                   names_to  = "out",
                   values_to = "val") %>%
      mutate(out = factor(x      = out,
                          levels = c("rosc", "surv", "miller", "peters", "witt"))) %>%
      split(.$out) %>%
      map(.x = .,
          .f = ~ glm(formula = val ~ epineph + age + sex + rythm +
                           ems + mechanism + bt + defib,
                     family  = poisson(link = "log"),
                     data    = .x))


### Method 2: Standard Multiple Imputation #####################################

## ROSC
rosc_mi          <- map(.x = 1:20,
                        .f = ~ glm(rosc_formula,
                                   family = poisson(),
                                   data   = mice::complete(imputed_rosc, .x)))
class(rosc_mi)   <- "mira"
rosc_mi_pooled   <- mice::pool(rosc_mi)
rosc_mi_res      <- broom::tidy(rosc_mi_pooled, conf.int = TRUE, exponentiate = TRUE)

## Miller
miller_mi        <- map(.x = 1:20,
                        .f = ~ glm(miller_formula,
                                   family = poisson(),
                                   data   = mice::complete(imputed_miller, .x)))
class(miller_mi) <- "mira"
miller_mi_pooled <- mice::pool(miller_mi)
miller_mi_res    <- broom::tidy(miller_mi_pooled, conf.int = TRUE, exponentiate = TRUE)


### Method 3: Pattern-mixture approach #########################################

# This requires manual pooling for the combined imputations
rosc_pattern_models   <- vector("list", 10)
surv_pattern_models   <- vector("list", 10)
miller_pattern_models <- vector("list", 10)
peters_pattern_models <- vector("list", 10)
witt_pattern_models   <- vector("list", 10)

## ROSC loop
for(i in 1:10) {
      rosc_pattern_models[[i]]   <- glm(rosc_formula,
                                        family = poisson(),
                                        data   = comb_imputations_rosc[[i]])
}

## Miller loop
for(i in 1:10) {
      miller_pattern_models[[i]] <- glm(miller_formula,
                                        family = poisson(),
                                        data   = comb_imputations_miller[[i]])
}

rosc_pattern_results   <- pool_pattern_models(rosc_pattern_models)
miller_pattern_results <- pool_pattern_models(miller_pattern_models)


### Method 4: NMAR-sensitive imputation ########################################

## ROSC
rosc_sens_mod_ems <- vector("list", 20)
for(i in 1:20) {
      rosc_sens_mod_ems[[i]]          <- glm(rosc_formula,
                                             family = poisson(),
                                             data   = rosc_sens_imp_ems[[i]])
}

rosc_sens_mod_mech_blunt <- vector("list", 20)
for(i in 1:20) {
      rosc_sens_mod_mech_blunt[[i]]   <- glm(rosc_formula,
                                             family = poisson(),
                                             data   = rosc_sens_imp_mech_blunt[[i]])
}

rosc_sens_mod_mech_penet <- vector("list", 20)
for(i in 1:20) {
      rosc_sens_mod_mech_penet[[i]]   <- glm(rosc_formula,
                                             family = poisson(),
                                             data   = rosc_sens_imp_mech_penet[[i]])
}

rosc_sens_res   <- pool_pattern_models(c(rosc_sens_mod_ems,
                                         rosc_sens_mod_mech_blunt,
                                         rosc_sens_mod_mech_penet)) %>%
      mutate(across(.cols = c(estimate, conf.low, conf.high),
                    .fns  = ~ exp(.)))

## Miller----------------------------------------------------------------------#

miller_sens_mod_ems <- vector("list", 20)
for(i in 1:20) {
      miller_sens_mod_ems[[i]]        <- glm(miller_formula,
                                             family = poisson(),
                                             data   = miller_sens_imp_ems[[i]])
}

miller_sens_mod_mech_blunt <- vector("list", 20)
for(i in 1:20) {
      miller_sens_mod_mech_blunt[[i]] <- glm(miller_formula,
                                             family = poisson(),
                                             data   = miller_sens_imp_mech_blunt[[i]])
}

miller_sens_mod_mech_penet <- vector("list", 20)
for(i in 1:20) {
      miller_sens_mod_mech_penet[[i]] <- glm(miller_formula,
                                             family = poisson(),
                                             data   = miller_sens_imp_mech_penet[[i]])
}

miller_sens_res <- pool_pattern_models(c(miller_sens_mod_ems,
                                         miller_sens_mod_mech_blunt,
                                         miller_sens_mod_mech_penet)) %>%
      mutate(across(.cols = c(estimate, conf.low, conf.high),
                    .fns  = ~ exp(.)))


### Method 4: NMAR-sensitive imputation ########################################

## ROSC

rosc_sens_mod_ems <- vector("list", 20)
for(i in 1:20) {
      rosc_sens_mod_ems[[i]]          <- glm(rosc_formula,
                                             family = poisson(),
                                             data   = rosc_sens_imp_ems[[i]])
}

rosc_sens_mod_mech_blunt <- vector("list", 20)
for(i in 1:20) {
      rosc_sens_mod_mech_blunt[[i]]   <- glm(rosc_formula,
                                             family = poisson(),
                                             data   = rosc_sens_imp_mech_blunt[[i]])
}

rosc_sens_mod_mech_penet <- vector("list", 20)
for(i in 1:20) {
      rosc_sens_mod_mech_penet[[i]]   <- glm(rosc_formula,
                                             family = poisson(),
                                             data   = rosc_sens_imp_mech_penet[[i]])
}

rosc_sens_res   <- pool_pattern_models(c(rosc_sens_mod_ems,
                                         rosc_sens_mod_mech_blunt,
                                         rosc_sens_mod_mech_penet)) %>%
      mutate(across(.cols = c(estimate, conf.low, conf.high),
                    .fns  = ~ exp(.)))

## Miller----------------------------------------------------------------------#

miller_sens_mod_ems <- vector("list", 20)
for(i in 1:20) {
      miller_sens_mod_ems[[i]]        <- glm(miller_formula,
                                             family = poisson(),
                                             data   = miller_sens_imp_ems[[i]])
}

miller_sens_mod_mech_blunt <- vector("list", 20)
for(i in 1:20) {
      miller_sens_mod_mech_blunt[[i]] <- glm(miller_formula,
                                             family = poisson(),
                                             data   = miller_sens_imp_mech_blunt[[i]])
}

miller_sens_mod_mech_penet <- vector("list", 20)
for(i in 1:20) {
      miller_sens_mod_mech_penet[[i]] <- glm(miller_formula,
                                             family = poisson(),
                                             data   = miller_sens_imp_mech_penet[[i]])
}

miller_sens_res <- pool_pattern_models(c(miller_sens_mod_ems,
                                         miller_sens_mod_mech_blunt,
                                         miller_sens_mod_mech_penet)) %>%
      mutate(across(.cols = c(estimate, conf.low, conf.high),
                    .fns  = ~ exp(.)))



# save(list = c(
#
#       # ROSC
#       "rosc_cca_res",
#       "rosc_mi_res",
#       "rosc_pattern_results",
#
#       # Miller
#       "miller_cca_res",
#       "miller_mi_res",
#       "miller_pattern_results",
#       "miller_sens_res",
#
#      file = "../outputs/all_imputation_outcomes.RData")

# load("../outputs/all_imputation_outcomes.RData")


### Create Comparison Table ####################################################

## Combine all results for comparison

comparison_table <- bind_rows(

      # ROSC
      rosc_cca_res %>%
            mutate(outcome = "rosc",
                   method  = "Complete case",
                   .before = 1),
      rosc_mi_res %>%
            mutate(outcome = "rosc",
                   method = "Standard MI",
                   .before = 1),
      rosc_pattern_results %>%
            mutate(outcome = "rosc",
                   method = "Pattern-mixture",
                   .before = 1),
      surv_sens_res %>%
            mutate(outcome = "rosc",
                   method = "NMAR-sensitive",
                   .before = 1),

      # Miller
      miller_cca_res %>%
            mutate(outcome = "miller",
                   method  = "Complete case",
                   .before = 1),
      miller_mi_res %>%
            mutate(outcome = "miller",
                   method = "Standard MI",
                   .before = 1),
      miller_pattern_results %>%
            mutate(outcome = "miller",
                   method = "Pattern-mixture",
                   .before = 1),
      miller_sens_res %>%
            mutate(outcome = "miller",
                   method = "NMAR-sensitive",
                   .before = 1)
)


## Visaulization of Sensitivity

comparison_table %>%
      split(.$outcome) %>%

      map(.x = .,
          .f = ~ .x %>% filter(
                !str_detect(string = term,
                            pattern = regex(pattern = "rythm|intercept|drown",
                                            ignore_case = TRUE))) %>%
                filter(method != "NMAR-sensitive") %>%

                ggplot(aes(x   = term,
                           y   = estimate,
                           col = method)) +
                geom_point(position = position_dodge(width = 0.5)) +
                geom_errorbar(mapping  = aes(ymin = conf.low, ymax = conf.high),
                              width    = 0.2,
                              position = position_dodge(width = 0.5)) +
                facet_wrap(. ~ outcome, ncol = 1, scales = "free_x") +
                labs(title = "Comparison of coefficient estimates across missing data methods",
                     subtitle = "NMAR sensitivity analysis",
                     y = "Coefficient estimate",
                     x = NULL,
                     col = NULL) +
                theme_minimal() +
                coord_flip() +
                theme(legend.position = "bottom"))


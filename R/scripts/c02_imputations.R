
### Standard Multiple Imputation (Primary Analysis) ############################

# Basic Multiple Imputation--include ALL variables that might be related to missingness
imputed_rosc   <- mice::mice(epi_tca_sbst %>% select(!c("surv", "miller", "peters", "witt")),
                             m         = 20,    # Number of imputations
                             maxit     = 10,    # Iterations
                             seed      = 123,
                             printFlag = FALSE)

imputed_miller <- mice::mice(epi_tca_sbst %>% select(!c("rosc", "surv", "peters", "witt")),
                             m         = 20,    # Number of imputations
                             maxit     = 10,    # Iterations
                             seed      = 123,
                             printFlag = FALSE)

# save(imputed_rosc, imputed_miller,
#      file = "../outputs/impted_outcomes.RData")

load("../outputs/impted_outcomes.RData")

## Data Subset Imputation------------------------------------------------------#

imputed_miller_blunt <- mice::mice(
      epi_tca_sbst %>%
            filter(mechanism == "blunt") %>%
            select(!c("mechanism", "rosc", "surv", "peters", "witt")),
      m         = 20,    # Number of imputations
      maxit     = 10,    # Iterations
      seed      = 123,
      printFlag = FALSE
)

imputed_rosc_blunt   <- mice::mice(
      epi_tca_sbst %>%
            filter(mechanism == "blunt") %>%
            select(!c("mechanism","surv", "miller", "peters", "witt")),
      m         = 20,    # Number of imputations
      maxit     = 10,    # Iterations
      seed      = 123,
      printFlag = FALSE
)

imputed_miller_penet <- mice::mice(
      epi_tca_sbst %>%
            filter(mechanism == "penetrating") %>%
            select(!c("mechanism","rosc", "surv", "peters", "witt")),
      m         = 20,    # Number of imputations
      maxit     = 10,    # Iterations
      seed      = 123,
      printFlag = FALSE
)

imputed_rosc_penet   <- mice::mice(
      epi_tca_sbst %>%
            filter(mechanism == "penetrating") %>%
            select(!c("mechanism","surv", "miller", "peters", "witt")),
      m         = 20,    # Number of imputations
      maxit     = 10,    # Iterations
      seed      = 123,
      printFlag = FALSE
)

imputed_miller_peds <- mice::mice(
      epi_tca_sbst %>%
            select(!c("rosc", "surv", "peters", "witt")) %>%
            filter(age < 18),
      m         = 20,    # Number of imputations
      maxit     = 10,    # Iterations
      seed      = 123,
      printFlag = FALSE
)

imputed_rosc_peds   <- mice::mice(
      epi_tca_sbst %>%
            select(!c("surv", "miller", "peters", "witt")) %>%
            filter(age < 18),
      m         = 20,    # Number of imputations
      maxit     = 10,    # Iterations
      seed      = 123,
      printFlag = FALSE
)

imputed_miller_elder <- mice::mice(
      epi_tca_sbst %>%
            select(!c("rosc", "surv", "peters", "witt")) %>%
            filter(age >= 65),
      m         = 20,    # Number of imputations
      maxit     = 10,    # Iterations
      seed      = 123,
      printFlag = FALSE
)

imputed_rosc_elder   <- mice::mice(
      epi_tca_sbst %>%
            select(!c("surv", "miller", "peters", "witt")) %>%
            filter(age >= 65),
      m         = 20,    # Number of imputations
      maxit     = 10,    # Iterations
      seed      = 123,
      printFlag = FALSE
)

# save(imputed_miller_blunt, imputed_rosc_blunt,
#      imputed_miller_penet, imputed_rosc_penet,
#      imputed_miller_peds,  imputed_rosc_peds,
#      imputed_miller_elder, imputed_rosc_elder,
#      file = "../outputs/impted_subsets.RData")

# load("../outputs/impted_subsets.RData")


### Sensitivity Analsysis: Pattern-Mixture Approach ############################

## ROSC------------------------------------------------------------------------#

# Split data by outcome
data_rosc_1 <- epi_tca_sbst %>%
      filter(rosc == 1) %>%
      select(!c("surv", "miller", "peters", "witt"))
data_rosc_0 <- epi_tca_sbst %>%
      filter(rosc == 0) %>%
      select(!c("surv", "miller", "peters", "witt"))

# Impute separately with different assumptions
# For outcome=1 group (where missingness is higher)
imputed_rosc_1 <- mice::mice(data_rosc_1,
                             m         = 10,
                             maxit     = 10,
                             seed      = 123,
                             printFlag = FALSE)

# For outcome=0 group
imputed_rosc_0 <- mice::mice(data_rosc_0,
                             m         = 10,
                             maxit     = 10,
                             seed      = 123,
                             printFlag = FALSE)

# Combine the imputed datasets
# This is a bit manual but effective
comb_imputations_rosc <- vector("list", 10)

for(i in 1:10) {
      rosc_imp1 <- mice::complete(imputed_rosc_1, i)
      rosc_imp0 <- mice::complete(imputed_rosc_0, i)
      comb_imputations_rosc[[i]] <- bind_rows(rosc_imp1, rosc_imp0)
}


## Miller----------------------------------------------------------------------#

# Split data by outcome
data_miller_1 <- epi_tca_sbst %>%
      filter(miller == 1) %>%
      select(!c("rosc", "surv", "peters", "witt"))
data_miller_0 <- epi_tca_sbst %>%
      filter(miller == 0) %>%
      select(!c("rosc", "surv", "peters", "witt"))

# Impute separately with different assumptions
# For outcome=1 group (where missingness is higher)
imputed_miller_1 <- mice::mice(data_miller_1,
                               m         = 10,
                               maxit     = 10,
                               seed      = 123,
                               printFlag = FALSE)

# For outcome=0 group
imputed_miller_0 <- mice::mice(data_miller_0,
                               m         = 10,
                               maxit     = 10,
                               seed      = 123,
                               printFlag = FALSE)

# Combine the imputed datasets
# This is a bit manual but effective
comb_imputations_miller <- vector("list", 10)

for(i in 1:10) {
      miller_imp1 <- mice::complete(imputed_miller_1, i)
      miller_imp0 <- mice::complete(imputed_miller_0, i)
      comb_imputations_miller[[i]] <- bind_rows(miller_imp1, miller_imp0)
}


# save(comb_imputations_rosc,
#      comb_imputations_miller,
#      file = "../outputs/comb_imputations.RData")

# load("../outputs/comb_imputations.RData")



### More Sophisticated Sensitivity Analysis Using `mice` #######################

## Create sensitive imputations (more extreme scenario)

## ROSC------------------------------------------------------------------------#

rosc_sens_imp_ems          <- make_sensitive_imputation(impt_inpt = imputed_rosc,
                                                        outcm_var = "rosc",
                                                        trgt_var  = "ems")

rosc_sens_imp_mech_blunt   <- make_sensitive_imputation(impt_inpt = imputed_rosc,
                                                        outcm_var = "rosc",
                                                        trgt_var  = "mech",
                                                        mech_input = "blunt")

rosc_sens_imp_mech_penet   <- make_sensitive_imputation(impt_inpt = imputed_rosc,
                                                        outcm_var = "rosc",
                                                        trgt_var  = "mech",
                                                        mech_input = "penetrating")

## Miller----------------------------------------------------------------------#

miller_sens_imp_ems        <- make_sensitive_imputation(impt_inpt = imputed_miller,
                                                        outcm_var = "miller",
                                                        trgt_var  = "ems")

miller_sens_imp_mech_blunt <- make_sensitive_imputation(impt_inpt = imputed_miller,
                                                        outcm_var = "miller",
                                                        trgt_var  = "mech",
                                                        mech_input = "blunt")

miller_sens_imp_mech_penet <- make_sensitive_imputation(impt_inpt = imputed_miller,
                                                        outcm_var = "miller",
                                                        trgt_var  = "mech",
                                                        mech_input = "penetrating")



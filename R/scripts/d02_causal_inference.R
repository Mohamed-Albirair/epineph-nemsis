
# Causal Inference

### IPW ########################################################################

# Create a list of your mids objects
mids_list       <- list(miller = imputed_miller,
                        rosc   = imputed_rosc)

# Create a tibble with both mids objects and outcome names
analysis_data <- tibble(mids_obj     = mids_list,
                        outcome_name = names(mids_list))

# Use pmap to iterate over both
iptw_results <- pmap(analysis_data, function(mids_obj, outcome_name) {
      result <- perform_iptw_mi(mids_obj    = mids_obj,
                                outcome_var = outcome_name)

      result$outcome <- outcome_name
      return(result)
})

# Combine all results
bind_rows(iptw_results) %>%
      filter(term == "epineph") %>%
      select(outcome, estimate, lcl = conf.low, ucl = conf.high, p_val = p.value) %>%
      mutate(across(.cols = 2:4,
                    .fns  = ~ round(., 2))) %>%
      pander::pander()


# Blunt
miller_blunt_out <- perform_iptw_mi(imputed_miller_blunt,
                                    outcome_var = "miller",
                                    confounders = c("age", "sex", "rythm", "ems",
                                                    "bt", "defib")) %>%
      mutate(subst = "blunt", outcome = "miller")
rosc_blunt_out   <- perform_iptw_mi(imputed_rosc_blunt,
                                    outcome_var = "rosc",
                                    confounders = c("age", "sex", "rythm", "ems",
                                                    "bt", "defib")) %>%
      mutate(subst = "blunt", outcome = "rosc")

# Penetrating
miller_penet_out <- perform_iptw_mi(mids_obj    = imputed_miller_penet,
                                    outcome_var = "miller",
                                    confounders = c("age", "sex", "rythm", "ems",
                                                    "bt", "defib")) %>%
      mutate(subst = "penet", outcome = "miller")
rosc_penet_out   <- perform_iptw_mi(imputed_rosc_penet,
                                    outcome_var = "rosc",
                                    confounders = c("age", "sex", "rythm", "ems",
                                                    "bt", "defib")) %>%
      mutate(subst = "penet", outcome = "rosc")

# Peds
miller_peds_out  <- perform_iptw_mi(imputed_miller_peds, outcome_var = "miller") %>%
      mutate(subst = "peds", outcome = "miller")
rosc_peds_out    <- perform_iptw_mi(imputed_rosc_peds, outcome_var = "rosc") %>%
      mutate(subst = "peds", outcome = "rosc")

# Elderly
miller_elder_out <- perform_iptw_mi(imputed_miller_elder, outcome_var = "miller") %>%
      mutate(subst = "elder", outcome = "miller")
rosc_elder_out   <- perform_iptw_mi(imputed_rosc_elder, outcome_var = "rosc") %>%
      mutate(subst = "elder", outcome = "rosc")

# Combine all results
bind_rows(mget(ls()[grepl("_out", ls()) & !grepl("psm", ls())])) %>%
      filter(term == "epineph") %>%
      select(subst, outcome, estimate, lcl = conf.low, ucl = conf.high, p_val = p.value) %>%
      mutate(subst = factor(subst, levels = c("blunt", "penet", "peds", "elder"))) %>%
      arrange(subst) %>%
      mutate(across(.cols = 3:5,
                    .fns  = ~ round(., 2))) %>%
      pander::pander()


### PSM ########################################################################

# Use pmap to iterate over both
psm_results <- pmap(analysis_data, function(mids_obj, outcome_name) {
      result <- perform_psm_mi(mids_obj = mids_obj,
                               outcome_var = outcome_name)

      result$outcome <- outcome_name
      return(result)
})

# Combine all results
bind_rows(psm_results) %>%
      filter(term == "epineph") %>%
      select(outcome, estimate, lcl = conf.low, ucl = conf.high, p_val = p.value) %>%
      mutate(across(.cols = 2:4,
                    .fns  = ~ round(., 2))) %>%
      pander::pander()

# Blunt
miller_blunt_psm_out <- perform_psm_mi(imputed_miller_blunt,
                                       outcome_var = "miller",
                                       confounders = c("age", "sex", "rythm", "ems",
                                                       "bt", "defib")) %>%
      mutate(subst = "blunt", outcome = "miller")
rosc_blunt_psm_out   <- perform_psm_mi(imputed_rosc_blunt,
                                       outcome_var = "rosc",
                                       confounders = c("age", "sex", "rythm", "ems",
                                                       "bt", "defib")) %>%
      mutate(subst = "blunt", outcome = "rosc")

# Penetrating
miller_penet_psm_out <- perform_psm_mi(imputed_miller_penet,
                                       outcome_var = "miller",
                                       confounders = c("age", "sex", "rythm", "ems",
                                                       "bt", "defib")) %>%
      mutate(subst = "penet", outcome = "miller")
rosc_penet_psm_out   <- perform_psm_mi(imputed_rosc_penet,
                                       outcome_var = "rosc",
                                       confounders = c("age", "sex", "rythm", "ems",
                                                       "bt", "defib")) %>%
      mutate(subst = "penet", outcome = "rosc")

# Peds
miller_peds_psm_out  <- perform_psm_mi(imputed_miller_peds,
                                       outcome_var = "miller") %>%
      mutate(subst = "peds", outcome = "miller")
rosc_peds_psm_out    <- perform_psm_mi(imputed_rosc_peds, outcome_var = "rosc") %>%
      mutate(subst = "peds", outcome = "rosc")

# Elderly
miller_elder_psm_out <- perform_psm_mi(imputed_miller_elder, outcome_var = "miller") %>%
      mutate(subst = "elder", outcome = "miller")
rosc_elder_psm_out   <- perform_psm_mi(imputed_rosc_elder, outcome_var = "rosc") %>%
      mutate(subst = "elder", outcome = "rosc")

# Combine all results
bind_rows(mget(ls()[grepl("_psm_out", ls())])) %>%
      filter(term == "epineph") %>%
      select(subst, outcome, estimate, lcl = conf.low, ucl = conf.high, p_val = p.value) %>%
      mutate(subst = factor(subst, levels = c("blunt", "penet", "peds", "elder"))) %>%
      arrange(subst) %>%
      mutate(across(.cols = 3:5,
                    .fns  = ~ round(., 2))) %>%
      pander::pander()


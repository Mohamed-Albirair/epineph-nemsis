#' @title Descriptive Statistics
#' @author Mohamed Albirair, MBBS, MPH, PhD


### Table 1 ####################################################################

## Epinephrine frequency overall-----------------------------------------------#

tbl1_c1 <- epi_tca %>%
      mutate(epiany_minusEpizero = ifelse(epiany_minusEpizero == 1, "Received Epinephrine", "Not received epinephrine")) %>%
      gtsummary::tbl_summary(
            include = c(USCensusRegion, Urbanicity),
            by      = epiany_minusEpizero,
            percent = "row",
            missing_text = "(Missing)"
      ) %>%
      gtsummary::modify_header(label ~ "**Variable**") %>%
      # gtsummary::add_n() %>%
      gtsummary::modify_column_hide(columns = stat_1)  # Hide the second group

## Epinephrine frequency by trauma mechansim-----------------------------------#

tbl1_c2 <- epi_tca %>%
      filter(mechanismsummary %in% c("blunt", "penetrating")) %>%
      mutate(mechanismsummary = as.character(mechanismsummary),
             epiany_minusEpizero = ifelse(epiany_minusEpizero == 0, "Not received epinephrine", "Received epinephrine")) %>%
      gtsummary::tbl_strata(mechanismsummary,
                            strata = ,
                            .tbl_fun = ~ gtsummary::tbl_summary(
                                  data    = .x,
                                  include = c(USCensusRegion, Urbanicity),
                                  by      = epiany_minusEpizero,
                                  percent = "row",
                                  missing_text = "(Missing)"
                            ) %>% gtsummary::modify_header(label ~ "**Variable**")
      ) %>%
      gtsummary::modify_column_hide(columns = c(stat_1_1, stat_1_2))


## Epinephrine frequency among <18 YOs-----------------------------------------#

tbl1_c3 <- epi_tca %>%
      # mutate(age18 = ifelse(ageinyear < 18, "<18", "18+")) %>%
      mutate(peds = ifelse(peds == 1, "<18", "18+"),
             epiany_minusEpizero = ifelse(epiany_minusEpizero == 0, "Not received epinephrine", "Received epinephrine")) %>%
      gtsummary::tbl_strata(
            strata = peds,
            .tbl_fun = ~ gtsummary::tbl_summary(
                  data    = .x,
                  include = c(USCensusRegion, Urbanicity),
                  by      = epiany_minusEpizero,
                  percent = "row",
                  missing_text = "(Missing)"
            ) %>% gtsummary::modify_header(label ~ "**Variable**")
      ) %>%
      gtsummary::modify_column_hide(columns = c(stat_1_1, stat_1_2, stat_2_1))

## Epinephrine frequency among 65+ YOs-----------------------------------------#

tbl1_c4 <- epi_tca %>%
      mutate(age65 = ifelse(age65 == 1, "65+", "<65"),
             epiany_minusEpizero = ifelse(epiany_minusEpizero == 0, "Not received epinephrine", "Received epinephrine")) %>%
      gtsummary::tbl_strata(
            strata = age65,
            .tbl_fun = ~ gtsummary::tbl_summary(
                  data    = .x,
                  include = c(USCensusRegion, Urbanicity),
                  by      = epiany_minusEpizero,
                  percent = "row",
                  missing_text = "(Missing)"
            ) %>% gtsummary::modify_header(label ~ "**Variable**")
      ) %>%
      gtsummary::modify_column_hide(columns = c(stat_1_1, stat_1_2, stat_2_2))

tbl1 <- gtsummary::tbl_merge(list(tbl1_c1, tbl1_c2, tbl1_c3, tbl1_c4))
lab_vec <- tbl1$table_styling$header %>% filter(!hide) %>% select(label) %>% pull()
lab_vec[2] <- str_replace_all(lab_vec[2], "\\*\\*(.*?)\\*\\*", "**Overall**")
lab_vec[3] <- str_replace_all(lab_vec[3], "\\*\\*(.*?)\\*\\*", "**Blunt**")
lab_vec[4] <- str_replace_all(lab_vec[4], "\\*\\*(.*?)\\*\\*", "**Penetrating**")
lab_vec[5] <- str_replace_all(lab_vec[5], "\\*\\*(.*?)\\*\\*", "**Age <18**")
lab_vec[6] <- str_replace_all(lab_vec[6], "\\*\\*(.*?)\\*\\*", "**Age 65+**")


gtsummary::tbl_merge(list(tbl1_c1, tbl1_c2, tbl1_c3, tbl1_c4)) %>%
      gtsummary::modify_header(
            stat_2_1   ~ lab_vec[2],
            stat_2_1_2 ~ lab_vec[3],
            stat_2_2_2 ~ lab_vec[4],
            stat_2_2_3 ~ lab_vec[5],
            stat_2_1_4 ~ lab_vec[6]
      ) %>%
      gtsummary::remove_spanning_header() #%>%
# gtsummary::as_gt() %>%
# gt::gtsave(., "../outputs/table_1.png")



### Table 2 ####################################################################

rhythms <- sort(unique(as.character(epi_tca$rhythmonarrival_eArrest_17_1)))

epi_tca %>%

      # Modify variables
      mutate(epiany_minusEpizero = ifelse(
            test = epiany_minusEpizero == 1,
            yes  = "Received Epinephrine",
            no   = "Not received epinephrine"
      )) %>%
      mutate(age_cat = case_when(peds == 1  ~ "<18",
                                 age65 == 1 ~ "65+",
                                 TRUE       ~ "other")) %>%
      mutate(ePatient_13 = as.character(ePatient_13)) %>%
      filter(ePatient_13 %in% c("male", "female", NA)) %>%

      mutate(racesummary = as.character(racesummary)) %>%
      filter(!racesummary %in% c("NA", "Not recorded", "Not reporting")) %>%

      mutate(rythm_arrive = as.character(rhythmonarrival_eArrest_17_1)) %>%
      mutate(rythm_arrive = case_when(rythm_arrive == "PEA" ~
                                            "Pulseless electrical activity",
                                      rythm_arrive %in% rhythms[startsWith(rhythms, "sinus")] ~
                                            "Any sinus rythm",
                                      rythm_arrive == "asystole" ~
                                            "Asystole",
                                      rythm_arrive %in% c("afib", "atrial flutter") ~
                                            "Atrical fibrillation or flutter",
                                      TRUE ~ "Unknown / not recorded / artifact")) %>%

      mutate(rythm_arrest = as.character(eArrest_11)) %>%
      mutate(rythm_arrest = case_when(rythm_arrest == "PEA" ~
                                            "Pulseless electrical activity",
                                      rythm_arrest %in% rhythms[startsWith(rhythms, "sinus")] ~
                                            "Any sinus rythm",
                                      rythm_arrest == "asystole" ~
                                            "Asystole",
                                      rythm_arrest %in% c("afib", "atrial flutter") ~
                                            "Atrical fibrillation or flutter",
                                      TRUE ~ "Unknown / not recorded / artifact")) %>%


      gtsummary::tbl_summary(
            include = c(age_cat,
                        ePatient_13,      # Sex
                        racesummary,      # Race
                        # mass casualty incidence,
                        mechanismsummary, # Injury mechanism
                        EMSSceneTimeMin,  # EMS scene time in minutes
                        intubation,       # Endotracheal intubation performed
                        defibrillation,   # Defibrillation performed
                        bloodtransfusion, # Blood transfusion administered
                        rythm_arrive,
                        rythm_arrest        # Initial arrest heart rhythm
                        # eArrest_05,       # CPR performed prior to EMS arrival
                        # any_SBP,          # ever had SBP >60 mmHg during EMS event
                        # any_HR           # ever had HR >40 mmHg during EMS event
            ),
            label = list(age_cat          ~ "Age groups",
                         ePatient_13      ~ "Sex",
                         racesummary      ~ "Race",
                         mechanismsummary ~ "Injury mechanism",
                         EMSSceneTimeMin  ~ "EMS scene time in minutes",
                         intubation       ~ "Endotracheal intubation performed",
                         defibrillation   ~ "Defibrillation performed",
                         bloodtransfusion ~ "Blood transfusion administered",
                         rythm_arrive     ~ "Heart rhythm on EMS arrival",
                         rythm_arrest     ~ "First monitored arrest rhythm"),
            by      = epiany_minusEpizero,
            percent = "column",
            missing_text = "(Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
      ) %>%
      gtsummary::modify_header(label ~ "**Variable**") %>%
      gtsummary::add_overall() #%>%
# gtsummary::add_n() %>%
# gtsummary::modify_column_hide(columns = stat_1)

# gtsummary::remove_spanning_header() #%>%
# gtsummary::as_gt() %>%
# gt::gtsave(., "../outputs/table_2.png")



### Table 3 ####################################################################

tab3_a <- epi_tca %>%

      # Modify variables
      mutate(epiany_minusEpizero = ifelse(
            test = epiany_minusEpizero == 1,
            yes  = "Received Epinephrine",
            no   = "Not received epinephrine"
      )) %>%

      mutate(surv   = factor(survivedEMS_basedoneArrest_18,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             miller = factor(Miller0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             peters = factor(Peters0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             witt   = factor(Witt0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive"))) %>%


      gtsummary::tbl_summary(
            include = c(surv,
                        miller,
                        peters,
                        witt,
                        ROSCsummary),
            label = list(surv        ~ "Reported survival",
                         miller      ~ "Miller",
                         peters      ~ "Peters",
                         witt        ~ "Witt",
                         ROSCsummary ~ "Return of spontaneous circulation"),
            by      = epiany_minusEpizero,
            percent = "column",
            missing_text = "(Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
      ) %>%
      gtsummary::modify_header(label ~ "**Variable**") %>%
      # gtsummary::add_overall() #%>%
      # gtsummary::add_n()

      # https://www.danieldsjoberg.com/gtsummary/reference/remove_row_type.html
      gtsummary::remove_row_type(
            x           = .,
            variables   = c(surv, miller, peters, witt),
            typ         = "level",
            level_value = "Dead")

## Blunt mechanism-------------------------------------------------------------#

tab3_b <- epi_tca %>%
      filter(mechanismsummary == "blunt") %>%

      # Modify variables
      mutate(epiany_minusEpizero = ifelse(
            test = epiany_minusEpizero == 1,
            yes  = "Received Epinephrine",
            no   = "Not received epinephrine"
      )) %>%

      mutate(surv   = factor(survivedEMS_basedoneArrest_18,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             miller = factor(Miller0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             peters = factor(Peters0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             witt   = factor(Witt0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive"))) %>%


      gtsummary::tbl_summary(
            include = c(surv,
                        miller,
                        peters,
                        witt,
                        ROSCsummary),
            label = list(surv        ~ "Reported survival",
                         miller      ~ "Miller",
                         peters      ~ "Peters",
                         witt        ~ "Witt",
                         ROSCsummary ~ "Return of spontaneous circulation"),
            by      = epiany_minusEpizero,
            percent = "column",
            missing_text = "(Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
      ) %>%
      gtsummary::modify_header(label ~ "**Variable**") %>%
      # gtsummary::add_overall() #%>%
      # gtsummary::add_n()

      # https://www.danieldsjoberg.com/gtsummary/reference/remove_row_type.html
      gtsummary::remove_row_type(
            x           = .,
            variables   = c(surv, miller, peters, witt),
            typ         = "level",
            level_value = "Dead")


## Penetrating mechanism-------------------------------------------------------#

tab3_c <- epi_tca %>%
      filter(mechanismsummary == "penetrating") %>%

      # Modify variables
      mutate(epiany_minusEpizero = ifelse(
            test = epiany_minusEpizero == 1,
            yes  = "Received Epinephrine",
            no   = "Not received epinephrine"
      )) %>%

      mutate(surv   = factor(survivedEMS_basedoneArrest_18,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             miller = factor(Miller0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             peters = factor(Peters0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             witt   = factor(Witt0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive"))) %>%


      gtsummary::tbl_summary(
            include = c(surv,
                        miller,
                        peters,
                        witt,
                        ROSCsummary),
            label = list(surv        ~ "Reported survival",
                         miller      ~ "Miller",
                         peters      ~ "Peters",
                         witt        ~ "Witt",
                         ROSCsummary ~ "Return of spontaneous circulation"),
            by      = epiany_minusEpizero,
            percent = "column",
            missing_text = "(Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
      ) %>%
      gtsummary::modify_header(label ~ "**Variable**") %>%
      # gtsummary::add_overall() #%>%
      # gtsummary::add_n()

      # https://www.danieldsjoberg.com/gtsummary/reference/remove_row_type.html
      gtsummary::remove_row_type(
            x           = .,
            variables   = c(surv, miller, peters, witt),
            typ         = "level",
            level_value = "Dead")


## Peds------------------------------------------------------------------------#

tab3_d <- epi_tca %>%
      filter(peds == 1) %>%

      # Modify variables
      mutate(epiany_minusEpizero = ifelse(
            test = epiany_minusEpizero == 1,
            yes  = "Received Epinephrine",
            no   = "Not received epinephrine"
      )) %>%

      mutate(surv   = factor(survivedEMS_basedoneArrest_18,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             miller = factor(Miller0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             peters = factor(Peters0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             witt   = factor(Witt0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive"))) %>%


      gtsummary::tbl_summary(
            include = c(surv,
                        miller,
                        peters,
                        witt,
                        ROSCsummary),
            label = list(surv        ~ "Reported survival",
                         miller      ~ "Miller",
                         peters      ~ "Peters",
                         witt        ~ "Witt",
                         ROSCsummary ~ "Return of spontaneous circulation"),
            by      = epiany_minusEpizero,
            percent = "column",
            missing_text = "(Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
      ) %>%
      gtsummary::modify_header(label ~ "**Variable**") %>%
      # gtsummary::add_overall() #%>%
      # gtsummary::add_n()

      # https://www.danieldsjoberg.com/gtsummary/reference/remove_row_type.html
      gtsummary::remove_row_type(
            x           = .,
            variables   = c(surv, miller, peters, witt),
            typ         = "level",
            level_value = "Dead")


## Elderly---------------------------------------------------------------------#

tab3_e <- epi_tca %>%
      filter(age65 == 1) %>%

      # Modify variables
      mutate(epiany_minusEpizero = ifelse(
            test = epiany_minusEpizero == 1,
            yes  = "Received Epinephrine",
            no   = "Not received epinephrine"
      )) %>%

      mutate(surv   = factor(survivedEMS_basedoneArrest_18,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             miller = factor(Miller0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             peters = factor(Peters0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             witt   = factor(Witt0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive"))) %>%


      gtsummary::tbl_summary(
            include = c(surv,
                        miller,
                        peters,
                        witt,
                        ROSCsummary),
            label = list(surv        ~ "Reported survival",
                         miller      ~ "Miller",
                         peters      ~ "Peters",
                         witt        ~ "Witt",
                         ROSCsummary ~ "Return of spontaneous circulation"),
            by      = epiany_minusEpizero,
            percent = "column",
            missing_text = "(Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
      ) %>%
      gtsummary::modify_header(label ~ "**Variable**") %>%
      # gtsummary::add_overall() #%>%
      # gtsummary::add_n()

      # https://www.danieldsjoberg.com/gtsummary/reference/remove_row_type.html
      gtsummary::remove_row_type(
            x           = .,
            variables   = c(surv, miller, peters, witt),
            typ         = "level",
            level_value = "Dead")


# tab3$table_styling$spanning_header

gtsummary::tbl_merge(list(tab3_a, tab3_b, tab3_c, tab3_d, tab3_e)) %>%
      gtsummary::modify_spanning_header(
            matches("^(var_type|stat_[1|2])_1$")  ~ "**Overall**",
            matches("^(var_type|stat_[1|2])_2$")  ~ "**Blunt**",
            matches("^(var_type|stat_[1|2])_3$")  ~ "**Penetrating**",
            matches("^(var_type|stat_[1|2])_4$")  ~ "**Age <18**",
            matches("^(var_type|stat_[1|2])_5$")  ~ "**Age 65+**"
      ) #%>%

# gtsummary::as_gt() %>%
# gt::gtsave(., "../outputs/table_3.html")



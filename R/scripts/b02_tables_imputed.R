
# Tables--Imputed

## Table 1

epi_tca_imp <- purrr::map_dfr(1:imputed_miller$m, function(i) {

      comp_miller <- mice::complete(imputed_miller, action = i)
      comp_rosc   <- mice::complete(imputed_rosc,   action = i)

      epi_tca %>%
            select(-c(ageinyear, ePatient_13, rhythmonarrival_simple,
                      EMSSceneTimeMin, mechanismsummary, bloodtransfusion,
                      defibrillation, epiany_minusEpizero, Miller0dead1alive,
                      ROSCsummary)) %>%
            bind_cols(
                  comp_miller %>%
                        rename(ageinyear              = age,
                               ePatient_13            = sex,
                               rhythmonarrival_simple = rythm,
                               EMSSceneTimeMin        = ems,
                               mechanismsummary       = mechanism,
                               bloodtransfusion       = bt,
                               defibrillation         = defib,
                               epiany_minusEpizero    = epineph,
                               Miller0dead1alive      = miller),
                  comp_rosc %>% select(ROSCsummary = rosc)) %>%
            mutate(imp = i)
})


# Epinephrine frequency overall------------------------------------------------#
imp_tbl1_c1 <- epi_tca_imp %>%
      mutate(epiany_minusEpizero = ifelse(test = epiany_minusEpizero == 1,
                                          yes  = "Received Epinephrine",
                                          no   = "Not received epinephrine")) %>%
      gtsummary::tbl_summary(
            include = c(USCensusRegion, Urbanicity),
            by      = epiany_minusEpizero,
            percent = "row",
            missing_text = "(Missing)"
      ) %>%

      # https://stackoverflow.com/questions/78766859/modify-cells-in-tbl-summary-table-by-dividing-with-constant
      gtsummary::modify_table_body(
            fun = ~ .x %>% mutate(
                  across(.cols = gtsummary::all_stat_cols(),
                         .fns  = ~ stringr::str_replace(
                               string      = .,
                               pattern     = "^\\d+(?:,\\d+)*",
                               replacement =  function(z) {
                                     scales::label_comma()(do.call(function(x, y) x / y,
                                                                   list(parse_number(z), m)))
                               })))) %>%

      gtsummary::modify_header(label ~ "**Variable**",
                               # This will only work with 2 spaces before "\n"!!
                               gtsummary::all_stat_cols() ~ "**{level}**  \nN = {scales::label_comma()(n / m)}") %>%
      # gtsummary::add_n() %>%
      gtsummary::modify_column_hide(columns = stat_1)


# Epinephrine frequency by trauma mechansim------------------------------------#
imp_tbl1_c2 <- epi_tca_imp %>%
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
                            ) %>%
                                  gtsummary::modify_header(label ~ "**Variable**",
                                                           # This will only work with 2 spaces before "\n"!!
                                                           gtsummary::all_stat_cols() ~ "**{level}**  \nN = {scales::label_comma()(n / m)}")
      ) %>%

      # https://stackoverflow.com/questions/78766859/modify-cells-in-tbl-summary-table-by-dividing-with-constant
      gtsummary::modify_table_body(
            fun = ~ .x %>% mutate(
                  across(.cols = gtsummary::all_stat_cols(),
                         .fns  = ~ stringr::str_replace(
                               string      = .,
                               pattern     = "^\\d+(?:,\\d+)*",
                               replacement =  function(z) {
                                     scales::label_comma()(do.call(function(x, y) x / y,
                                                                   list(parse_number(z), m)))
                               })))) %>%

      gtsummary::modify_column_hide(columns = c(stat_1_1, stat_1_2))


# Epinephrine frequency among <18 YOs------------------------------------------#
imp_tbl1_c3 <- epi_tca_imp %>%
      # mutate(age18 = ifelse(ageinyear < 18, "<18", "18+")) %>%
      # mutate(peds = as.numeric(ageinyear < 18)) %>%
      mutate(peds = ifelse(peds == 1, "<18", "18+"),
             epiany_minusEpizero = ifelse(epiany_minusEpizero == 0,
                                          "Not received epinephrine",
                                          "Received epinephrine")) %>%
      gtsummary::tbl_strata(peds,
                            strata = ,
                            .tbl_fun = ~ gtsummary::tbl_summary(
                                  data    = .x,
                                  include = c(USCensusRegion, Urbanicity),
                                  by      = epiany_minusEpizero,
                                  percent = "row",
                                  # statistic    = list(gtsummary::all_categorical() ~ "{n} ({p}%)"),
                                  missing_text = "(Missing)"
                            ) %>% gtsummary::modify_header(label ~ "**Variable**",
                                                           # This will only work with 2 spaces before "\n"!!
                                                           gtsummary::all_stat_cols() ~ "**{level}**  \nN = {scales::label_comma()(n / m)}")
      ) %>%
      # https://stackoverflow.com/questions/78766859/modify-cells-in-tbl-summary-table-by-dividing-with-constant
      gtsummary::modify_table_body(
            fun = ~ .x %>% mutate(
                  across(.cols = gtsummary::all_stat_cols(),
                         .fns  = ~ stringr::str_replace(
                               string      = .,
                               pattern     = "^\\d+(?:,\\d+)*",
                               replacement =  function(z) {
                                     scales::label_comma()(do.call(function(x, y) x / y,
                                                                   list(parse_number(z), m)))
                               })))) %>%
      gtsummary::modify_column_hide(columns = c(stat_1_1, stat_1_2, stat_2_1))

# Epinephrine frequency among 65+ YOs------------------------------------------#
imp_tbl1_c4 <- epi_tca_imp %>%
      # mutate(peds = as.numeric(ageinyear >= 65)) %>%
      mutate(age65 = ifelse(age65 == 1, "65+", "<65"),
             epiany_minusEpizero = ifelse(epiany_minusEpizero == 0,
                                          "Not received epinephrine",
                                          "Received epinephrine")) %>%
      gtsummary::tbl_strata(
            strata = age65,
            .tbl_fun = ~ gtsummary::tbl_summary(
                  data    = .x,
                  include = c(USCensusRegion, Urbanicity),
                  by      = epiany_minusEpizero,
                  percent = "row",
                  missing_text = "(Missing)"
            ) %>% gtsummary::modify_header(label ~ "**Variable**",
                                           # This will only work with 2 spaces before "\n"!!
                                           gtsummary::all_stat_cols() ~ "**{level}**  \nN = {scales::label_comma()(n / m)}")
      ) %>%
      # https://stackoverflow.com/questions/78766859/modify-cells-in-tbl-summary-table-by-dividing-with-constant
      gtsummary::modify_table_body(
            fun = ~ .x %>% mutate(
                  across(.cols = gtsummary::all_stat_cols(),
                         .fns  = ~ stringr::str_replace(
                               string      = .,
                               pattern     = "^\\d+(?:,\\d+)*",
                               replacement =  function(z) {
                                     scales::label_comma()(do.call(function(x, y) x / y,
                                                                   list(parse_number(z), m)))
                               })))) %>%
      gtsummary::modify_column_hide(columns = c(stat_1_1, stat_1_2, stat_2_2))


imp_tbl1 <- gtsummary::tbl_merge(list(imp_tbl1_c1, imp_tbl1_c2, imp_tbl1_c3, imp_tbl1_c4))
lab_vec <- imp_tbl1$table_styling$header %>% filter(!hide) %>% select(label) %>% pull()
lab_vec[2] <- str_replace_all(lab_vec[2], "\\*\\*(.*?)\\*\\*", "**Overall**")
lab_vec[3] <- str_replace_all(lab_vec[3], "\\*\\*(.*?)\\*\\*", "**Blunt**")
lab_vec[4] <- str_replace_all(lab_vec[4], "\\*\\*(.*?)\\*\\*", "**Penetrating**")
lab_vec[5] <- str_replace_all(lab_vec[5], "\\*\\*(.*?)\\*\\*", "**Age <18**")
lab_vec[6] <- str_replace_all(lab_vec[6], "\\*\\*(.*?)\\*\\*", "**Age 65+**")


# tbl1 <-
gtsummary::tbl_merge(list(imp_tbl1_c1, imp_tbl1_c2, imp_tbl1_c3, imp_tbl1_c4)) %>%
      # gtsummary::modify_spanning_header(
      #       # matches("_1$")   ~ "**Overall**",      # Columns ending with _1
      #       matches("_1_2$") ~ "**Blunt**",        # Columns ending with _2
      #       matches("_2_2$") ~ "**Penetrating**",  # Columns ending with _3
      #       matches("_2_3$") ~ "**Age <18**",       # Columns ending with _4
      #       matches("^(var_type|stat_[1|2])_1_4$")  ~ "**Age 65+**",
      #       matches("^(var_type|stat_[1|2])_1$")  ~ "**Overall**"
      # ) %>%
      gtsummary::modify_header(
            stat_2_1   ~ lab_vec[2],
            stat_2_1_2 ~ lab_vec[3],
            stat_2_2_2 ~ lab_vec[4],
            stat_2_2_3 ~ lab_vec[5],
            stat_2_1_4 ~ lab_vec[6]
      ) %>%
      gtsummary::remove_spanning_header() %>%
      gtsummary::as_gt() %>%
      gt::gtsave(., "R/outputs/imp_table_1.png")


### Table 2 ####################################################################


rhythms <- sort(unique(as.character(epi_tca$rhythmonarrival_eArrest_17_1)))

epi_tca_imp %>%

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

      gtsummary::add_overall() %>%

      # https://stackoverflow.com/questions/78766859/modify-cells-in-tbl-summary-table-by-dividing-with-constant
      gtsummary::modify_table_body(
            fun = ~ .x %>% mutate(
                  across(.cols = gtsummary::all_stat_cols(),
                         .fns  = ~ stringr::str_replace(
                               string      = .,
                               pattern     = "^\\d+(?:,\\d+)*",
                               replacement =  function(z) {
                                     scales::label_comma(accuracy = 1)(do.call(function(x, y) x / y,
                                                                               list(parse_number(z), m)))
                               })))) %>%

      gtsummary::modify_header(label ~ "**Variable**",
                               # This will only work with 2 spaces before "\n"!!
                               gtsummary::all_stat_cols() ~ "**{level}**  \nN = {scales::label_comma(accuracy = 1)(n / m)}") %>%
      # gtsummary::add_n() %>%
      # gtsummary::modify_column_hide(columns = stat_1)

      # gtsummary::remove_spanning_header() #%>%
      gtsummary::as_gt() %>%
      gt::gtsave(., "R/outputs/imp_table_2.png")


## Table 3 #####################################################################

imp_tab3_a <- epi_tca_imp %>%

      # Modify variables
      mutate(epiany_minusEpizero = ifelse(
            test = epiany_minusEpizero == 1,
            yes  = "Received Epinephrine",
            no   = "Not received epinephrine"
      )) %>%

      mutate(miller = factor(Miller0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             rosc   = factor(ROSCsummary,
                             levels = 0:1,
                             labels = c("No", "Yes"))) %>%


      gtsummary::tbl_summary(
            include = c(miller, rosc),
            label = list(miller ~ "Miller",
                         rosc   ~ "Return of spontaneous circulation"),
            type = list(rosc ~ "categorical"),
            by      = epiany_minusEpizero,
            percent = "column",
            missing_text = "(Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
      ) %>%

      # https://stackoverflow.com/questions/78766859/modify-cells-in-tbl-summary-table-by-dividing-with-constant
      gtsummary::modify_table_body(
            fun = ~ .x %>% mutate(
                  across(.cols = gtsummary::all_stat_cols(),
                         .fns  = ~ stringr::str_replace(
                               string      = .,
                               pattern     = "^\\d+(?:,\\d+)*",
                               replacement =  function(z) {
                                     scales::label_comma()(do.call(function(x, y) x / y,
                                                                   list(parse_number(z), m)))
                               })))) %>%

      gtsummary::modify_header(label ~ "**Variable**",
                               # This will only work with 2 spaces before "\n"!!
                               gtsummary::all_stat_cols() ~ "**{level}**  \nN = {scales::label_comma()(n / m)}") %>%
      # gtsummary::add_overall() #%>%
      # gtsummary::add_n()

      # https://www.danieldsjoberg.com/gtsummary/reference/remove_row_type.html
      gtsummary::remove_row_type(
            x           = .,
            variables   = miller,
            typ         = "level",
            level_value = "Dead") %>%
      gtsummary::remove_row_type(
            x           = .,
            variables   = rosc,
            typ         = "level",
            level_value = "No")


# Blunt mechanism--------------------------------------------------------------#
imp_tab3_b <- epi_tca_imp %>%
      filter(mechanismsummary == "blunt") %>%

      # Modify variables
      mutate(epiany_minusEpizero = ifelse(
            test = epiany_minusEpizero == 1,
            yes  = "Received Epinephrine",
            no   = "Not received epinephrine"
      )) %>%

      mutate(miller = factor(Miller0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             rosc   = factor(ROSCsummary,
                             levels = 0:1,
                             labels = c("No", "Yes"))) %>%

      gtsummary::tbl_summary(
            include = c(miller, rosc),
            label = list(miller ~ "Miller",
                         rosc   ~ "Return of spontaneous circulation"),
            type = list(rosc ~ "categorical"),
            by      = epiany_minusEpizero,
            percent = "column",
            missing_text = "(Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
      ) %>%

      gtsummary::modify_header(label ~ "**Variable**",
                               # This will only work with 2 spaces before "\n"!!
                               gtsummary::all_stat_cols() ~ "**{level}**  \nN = {scales::label_comma()(n / m)}") %>%
      # gtsummary::add_overall() #%>%
      # gtsummary::add_n()

      # https://stackoverflow.com/questions/78766859/modify-cells-in-tbl-summary-table-by-dividing-with-constant
      gtsummary::modify_table_body(
            fun = ~ .x %>% mutate(
                  across(.cols = gtsummary::all_stat_cols(),
                         .fns  = ~ stringr::str_replace(
                               string      = .,
                               pattern     = "^\\d+(?:,\\d+)*",
                               replacement =  function(z) {
                                     scales::label_comma()(do.call(function(x, y) x / y,
                                                                   list(parse_number(z), m)))
                               })))) %>%

      # https://www.danieldsjoberg.com/gtsummary/reference/remove_row_type.html
      gtsummary::remove_row_type(
            x           = .,
            variables   = miller,
            typ         = "level",
            level_value = "Dead") %>%
      gtsummary::remove_row_type(
            x           = .,
            variables   = rosc,
            typ         = "level",
            level_value = "No")


# Penetrating mechanism--------------------------------------------------------#
imp_tab3_c <- epi_tca_imp %>%
      filter(mechanismsummary == "penetrating") %>%

      # Modify variables
      mutate(epiany_minusEpizero = ifelse(
            test = epiany_minusEpizero == 1,
            yes  = "Received Epinephrine",
            no   = "Not received epinephrine"
      )) %>%

      mutate(miller = factor(Miller0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             rosc   = factor(ROSCsummary,
                             levels = 0:1,
                             labels = c("No", "Yes"))) %>%


      gtsummary::tbl_summary(
            include = c(miller, rosc),
            label = list(miller ~ "Miller",
                         rosc   ~ "Return of spontaneous circulation"),
            type = list(rosc ~ "categorical"),
            by      = epiany_minusEpizero,
            percent = "column",
            missing_text = "(Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
      ) %>%
      gtsummary::modify_header(label ~ "**Variable**",
                               # This will only work with 2 spaces before "\n"!!
                               gtsummary::all_stat_cols() ~ "**{level}**  \nN = {scales::label_comma()(n / m)}") %>%
      # gtsummary::add_overall() #%>%
      # gtsummary::add_n()

      # https://stackoverflow.com/questions/78766859/modify-cells-in-tbl-summary-table-by-dividing-with-constant
      gtsummary::modify_table_body(
            fun = ~ .x %>% mutate(
                  across(.cols = gtsummary::all_stat_cols(),
                         .fns  = ~ stringr::str_replace(
                               string      = .,
                               pattern     = "^\\d+(?:,\\d+)*",
                               replacement =  function(z) {
                                     scales::label_comma()(do.call(function(x, y) x / y,
                                                                   list(parse_number(z), m)))
                               })))) %>%

      # https://www.danieldsjoberg.com/gtsummary/reference/remove_row_type.html
      gtsummary::remove_row_type(
            x           = .,
            variables   = miller,
            typ         = "level",
            level_value = "Dead") %>%
      gtsummary::remove_row_type(
            x           = .,
            variables   = rosc,
            typ         = "level",
            level_value = "No")


# Peds-------------------------------------------------------------------------#
imp_tab3_d <- epi_tca_imp %>%
      filter(peds == 1) %>%

      # Modify variables
      mutate(epiany_minusEpizero = ifelse(
            test = epiany_minusEpizero == 1,
            yes  = "Received Epinephrine",
            no   = "Not received epinephrine"
      )) %>%

      mutate(miller = factor(Miller0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             rosc   = factor(ROSCsummary,
                             levels = 0:1,
                             labels = c("No", "Yes"))) %>%


      gtsummary::tbl_summary(
            include = c(miller, rosc),
            label = list(miller ~ "Miller",
                         rosc   ~ "Return of spontaneous circulation"),
            type = list(rosc ~ "categorical"),
            by      = epiany_minusEpizero,
            percent = "column",
            missing_text = "(Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
      ) %>%
      gtsummary::modify_header(label ~ "**Variable**",
                               # This will only work with 2 spaces before "\n"!!
                               gtsummary::all_stat_cols() ~ "**{level}**  \nN = {scales::label_comma()(n / m)}") %>%
      # gtsummary::add_overall() #%>%
      # gtsummary::add_n()

      # https://stackoverflow.com/questions/78766859/modify-cells-in-tbl-summary-table-by-dividing-with-constant
      gtsummary::modify_table_body(
            fun = ~ .x %>% mutate(
                  across(.cols = gtsummary::all_stat_cols(),
                         .fns  = ~ stringr::str_replace(
                               string      = .,
                               pattern     = "^\\d+(?:,\\d+)*",
                               replacement =  function(z) {
                                     scales::label_comma()(do.call(function(x, y) x / y,
                                                                   list(parse_number(z), m)))
                               })))) %>%

      # https://www.danieldsjoberg.com/gtsummary/reference/remove_row_type.html
      gtsummary::remove_row_type(
            x           = .,
            variables   = miller,
            typ         = "level",
            level_value = "Dead") %>%
      gtsummary::remove_row_type(
            x           = .,
            variables   = rosc,
            typ         = "level",
            level_value = "No")


# Elderly----------------------------------------------------------------------#
imp_tab3_e <- epi_tca_imp %>%
      filter(age65 == 1) %>%

      # Modify variables
      mutate(epiany_minusEpizero = ifelse(
            test = epiany_minusEpizero == 1,
            yes  = "Received Epinephrine",
            no   = "Not received epinephrine"
      )) %>%

      mutate(miller = factor(Miller0dead1alive,
                             levels = 0:1,
                             labels = c("Dead", "Alive")),
             rosc   = factor(ROSCsummary,
                             levels = 0:1,
                             labels = c("No", "Yes"))) %>%


      gtsummary::tbl_summary(
            include = c(miller, rosc),
            label = list(miller ~ "Miller",
                         rosc   ~ "Return of spontaneous circulation"),
            type = list(rosc ~ "categorical"),
            by      = epiany_minusEpizero,
            percent = "column",
            missing_text = "(Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
      ) %>%
      gtsummary::modify_header(label ~ "**Variable**",
                               # This will only work with 2 spaces before "\n"!!
                               gtsummary::all_stat_cols() ~ "**{level}**  \nN = {scales::label_comma()(n / m)}") %>%
      # gtsummary::add_overall() #%>%
      # gtsummary::add_n()

      # https://stackoverflow.com/questions/78766859/modify-cells-in-tbl-summary-table-by-dividing-with-constant
      gtsummary::modify_table_body(
            fun = ~ .x %>% mutate(
                  across(.cols = gtsummary::all_stat_cols(),
                         .fns  = ~ stringr::str_replace(
                               string      = .,
                               pattern     = "^\\d+(?:,\\d+)*",
                               replacement =  function(z) {
                                     scales::label_comma()(do.call(function(x, y) x / y,
                                                                   list(parse_number(z), m)))
                               })))) %>%

      # https://www.danieldsjoberg.com/gtsummary/reference/remove_row_type.html
      gtsummary::remove_row_type(
            x           = .,
            variables   = miller,
            typ         = "level",
            level_value = "Dead") %>%
      gtsummary::remove_row_type(
            x           = .,
            variables   = rosc,
            typ         = "level",
            level_value = "No")


# tab3$table_styling$spanning_header

# gtsummary::tbl_merge(list(imp_tab3_a, imp_tab3_b, imp_tab3_c, imp_tab3_d, imp_tab3_e)) %>%
#       gtsummary::modify_spanning_header(
#             matches("^(var_type|stat_[1|2])_1$")  ~ "**Overall**",
#             matches("^(var_type|stat_[1|2])_2$")  ~ "**Blunt**",
#             matches("^(var_type|stat_[1|2])_3$")  ~ "**Penetrating**",
#             matches("^(var_type|stat_[1|2])_4$")  ~ "**Age <18**",
#             matches("^(var_type|stat_[1|2])_5$")  ~ "**Age 65+**"
#       ) %>%
#
#       gtsummary::as_gt() %>%
#       gt::gtsave(., "../outputs/imp_table_3.png", vwidth = 2000)


gtsummary::tbl_stack(
      tbls = list(imp_tab3_a, imp_tab3_b, imp_tab3_c, imp_tab3_d, imp_tab3_e),
      group_header = c("**Overall**", "**Blunt**", "**Penetrating**", "**Age <18**", "**Age 65+**")) %>%
      gtsummary::as_gt() %>%
      gt::gtsave(., "../outputs/imp_table_3_long.png")


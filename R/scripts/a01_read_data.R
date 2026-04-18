

source("R/library.R")

epi_tca_raw <- haven::read_dta(file = here::here(s$data_dir, s$data_loc)) %>%
      filter(!eDisposition_12 %in% c(
            4212001,  # agency assist
            4212003,  # public assist
            4212005,  # unit assist
            4212007,  # canceled PTA
            4212009,  # cancelled on scene, no pt contact
            4212011,  # cancelled on scene, no pt found
            4212013,  # DEAD AT SCENE NO RESUSCITATION ATTEMPTED w transport
            4212015,  # DEAD AT SCENE NO RESUSCITATION ATTEMPTED without transport
            4212021,  # pt evaluated, no treatment/transport required
            4212025,  # pt refused eval/care without transport
            4212031,  # pt treated, transferred care to another EMS unit
            4212035,  # pt treated and transported by law enforcement
            4212037,  # pt treated and transported by POV
            4212039,  # standby - no services or support
            4212041,  # standby - safety fire or operational support
            4212043   # transport non-patient organs
      ))

epi_tca <- epi_tca_raw %>%
      mutate(across(.cols = where(fn = is.character),
                    .fns  = ~ na_if(., ""))) %>%
      mutate(across(.cols = where(fn = function(x) any(class(x) == "haven_labelled")),
                    .fns  = ~ haven::as_factor(.)))

epi_tca_sbst <- epi_tca %>%
      select(age       = ageinyear,
             sex       = ePatient_13,
             rythm     = rhythmonarrival_simple,
             ems       = EMSSceneTimeMin,
             mechanism = mechanismsummary,
             bt        = bloodtransfusion,
             defib     = defibrillation,
             epineph   = epiany_minusEpizero, # primary predictor

             # Outcome
             rosc      = ROSCsummary,
             surv      = survivedEMS_basedoneArrest_18,
             miller    = Miller0dead1alive,
             peters    = Peters0dead1alive,
             witt      = Witt0dead1alive)




### Missingness Analysis #######################################################

## Quantify Missingness

source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/misc/report_missing.R")

epi_tca_sbst %>% report_missing %>%
      pander::pander()


## Explore Missingness Patterns------------------------------------------------#

epi_tca_sbst %>%
      mice::md.pattern(.,
                       rotate.names = TRUE,
                       plot = TRUE)


## ROSC

coef(summary(glm(ems ~ rosc + age + sex + rythm + mechanism + bt + defib + epineph,
                 family = binomial(),
                 data = epi_tca_sbst %>%
                       mutate(ems = !is.na(ems)))))[2, 4] < 0.05

coef(summary(glm(mechanism ~ rosc + age + sex + rythm + ems + bt + defib + epineph,
                 family = binomial(),
                 data = epi_tca_sbst %>%
                       mutate(mechanism = !is.na(mechanism)))))[2, 4] < 0.05


## Surv

coef(summary(glm(ems ~ surv + age + sex + rythm + mechanism + bt + defib + epineph,
                 family = binomial(),
                 data = epi_tca_sbst %>%
                       mutate(ems = !is.na(ems)))))[2, 4] < 0.05

coef(summary(glm(mechanism ~ surv + age + sex + rythm + ems + bt + defib + epineph,
                 family = binomial(),
                 data = epi_tca_sbst %>%
                       mutate(mechanism = !is.na(mechanism)))))[2, 4] < 0.05


## Miller

coef(summary(glm(ems ~ miller + age + sex + rythm + mechanism + bt + defib + epineph,
                 family = binomial(),
                 data = epi_tca_sbst %>%
                       mutate(ems = !is.na(ems)))))[2, 4] < 0.05

coef(summary(glm(mechanism ~ miller + age + sex + rythm + ems + bt + defib + epineph,
                 family = binomial(),
                 data = epi_tca_sbst %>%
                       mutate(mechanism = !is.na(mechanism)))))[2, 4] < 0.05


## Peters

coef(summary(glm(ems ~ peters + age + sex + rythm + mechanism + bt + defib + epineph,
                 family = binomial(),
                 data = epi_tca_sbst %>%
                       mutate(ems = !is.na(ems)))))[2, 4] < 0.05

coef(summary(glm(mechanism ~ peters + age + sex + rythm + ems + bt + defib + epineph,
                 family = binomial(),
                 data = epi_tca_sbst %>%
                       mutate(mechanism = !is.na(mechanism)))))[2, 4] < 0.05


## Witt

coef(summary(glm(ems ~ witt + age + sex + rythm + mechanism + bt + defib + epineph,
                 family = binomial(),
                 data = epi_tca_sbst %>%
                       mutate(ems = !is.na(ems)))))[2, 4] < 0.05

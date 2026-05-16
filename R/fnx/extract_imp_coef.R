

extract_imp_coef <- function(formula_inpt,
                             imp_obj) {

      rosc_mi          <- map(.x = 1:20,
                              .f = ~ glm(formula_inpt,
                                         family = poisson(),
                                         data   = mice::complete(imp_obj, .x)))
      class(rosc_mi)   <- "mira"
      rosc_mi_pooled   <- mice::pool(rosc_mi)
      rosc_mi_res      <- broom::tidy(rosc_mi_pooled, conf.int = TRUE, exponentiate = TRUE)

      print(paste0(round(rosc_mi_res[2, 2], 2),
                   " (",
                   round(rosc_mi_res[2, 6], 2),
                   " - ",
                   round(rosc_mi_res[2, 7], 2),
                   ")"))

      # return(rosc_mi_res)
}

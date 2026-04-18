
# Let's assume EMSSceneTimeMin might be higher for missing cases in outcome=1 group
# Adjust the imputation to add 10 minutes for outcome=1 missing cases
make_sensitive_imputation <- function(impt_inpt,
                                      outcm_var,
                                      trgt_var   = c("ems", "mechanism"),
                                      ems_shift  = 1.5,
                                      mech_input = NULL) {

      # Validate inputs
      trgt_var <- match.arg(trgt_var, c("ems", "mechanism"))

      if (trgt_var == "ems") {
            mech_input <- NULL
      } else if (trgt_var == "mechanism") {
            if (is.null(mech_input)) stop("No input specified for sensitive mechanism imputation")
            mech_input <- match.arg(mech_input, c("blunt", "penetrating"))
      }

      sensitive_imps <- vector("list", impt_inpt$m)

      for(i in 1:impt_inpt$m) {
            imputn <- mice::complete(impt_inpt, i)

            # Get original data from mice object (has original missing pattern)
            orig_data <- impt_inpt$data

            if (trgt_var == "ems") {
                  # Identify originally missing values in the target variable for outcome=1
                  miss_indx <- which(is.na(orig_data[[trgt_var]]) &
                                           orig_data[[outcm_var]] == 1)

                  if (length(miss_indx) > 0) {
                        # Shift the imputed values for these cases
                        imputn[miss_indx, trgt_var] <- imputn[miss_indx, trgt_var] * ems_shift

                  }


            } else if (trgt_var == "mechanism") {
                  # Identify ALL originally missing values in mechanism (regardless of outcome)
                  miss_indx <- which(is.na(orig_data[[trgt_var]]))

                  if(length(miss_indx) > 0) {
                        # Set all missing mechanism to specified scenario
                        imputn[miss_indx, trgt_var] <- mech_input

                  }

            }

            sensitive_imps[[i]] <- imputn
      }

      return(sensitive_imps)
}

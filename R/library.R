#------------------------------------------------------------------------------#
# Epinepherine and Post-Trauma Survival
#
# Author: Mohamed Albirair, MBBS, MPH, PhD (credit: Pedro Nascimento de Lima)
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#


# Load libraries
library(tidyverse)

## load settings
# s <- yaml::read_yaml("settings_lm.yml")
s <- yaml::read_yaml(here::here("settings.yml"))

# Home folder

s$home_dir <- here::here()

# R code
if (!exists("r_root")) {
      r_root       <- here::here("R")
}

s$data_dir    <- here::here(s$home_dir, "data")
s$fnx_dir     <- here::here(r_root, "fnx")
s$scripts_dir <- here::here(r_root, "scripts")
s$outputs_dir <- here::here(r_root, "outputs")


# Source functions
invisible(sapply(X   = paste0(list.files(path       = s$fnx_dir,
                                         pattern    = "*.R",
                                         full.names = TRUE)),
                 FUN = source, echo = F))


# s$cisnet_crc_col   <- c("#66CD00", "#E9967A", "gray")
s$output_col <- c(opt_a = "#CCAD8F",
                  opt_b = "#DE6E4B",
                  opt_c = "#7C9885",
                  opt_d = "#28666E")

s$col_gradient <- c("#C55A38", "#DE6E4B", "#E48D69", "#EBAC87", "#F1CBA4", "#F7EAC2",
                    # "#C5F9D7", "#9ED4BD", "#77B0A3", "#4F8B88", "#28666E", "#1B494F")
                    # "#C5F9D7", "#A5E4BE", "#85D0A6", "#65BB8E", "#396A50", "#28666E", "#1B494F",

                    "#C5F9D7", "#A8E6CF", "#8BCCB8", "#6EB2A2", "#51988C",
                    "#347E76", "#1B645F", "#1B494F")

## Load functions/scripts from URLs
#-----check if connected to the internet:
# https://stackoverflow.com/questions/5076593/how-to-determine-if-you-have-an-internet-connection-in-r
if (curl::has_internet()) {
      # `not it`
      source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/misc/notin.R")

      # ggplot theme_caviz
      source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/viz_ggplot/theme_caviz.R")

      # Report missing
      # source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/misc/report_missing.R")
      source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/misc/report_missing.R")

      # Convert probability to rate
      source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/epi/prob_to_rate.R")

      # Convert rate to probability
      source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/epi/rate_to_prob.R")

      # Explore an .RData file
      source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/misc/explore_rdata_obj.R")

      # Save/Update an .RData file
      source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/misc/save_rdata.R")

} else {
      # `not it`
      source(z$notin)

      # ggplot theme_caviz
      source(z$caviz_theme)

      # Report missingness
      source(z$report_missing)

      # Convert probability to rate
      source(z$prob_to_rate)

      # Convert rate to probability
      source(z$rate_to_prob)

      # Explore an .RData file
      source(z$explore_rdata)

      # Save/update an .RData file
      source(z$save_rdata_file)
}

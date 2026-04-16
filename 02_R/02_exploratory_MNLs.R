# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# Exploratory MNLs
#
# Oliver Becker (oliver@pksb.de)
# Apr 17 2026
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr)


# 1) Load and prepare choice data -----------------------------------------

# Sampled subsets
choice_sampled <- read_csv("01_Data/choice_sampled_dist_stratified_Apr16.csv")

# Inpute median for NA values
choice_sampled <- choice_sampled %>%
  mutate(across(where(is.numeric),
                ~ replace(., is.na(.), median(., na.rm = TRUE))))

# 2) Select and transform variables ---------------------------------------

# Some log-transforms
choice_sampled <- choice_sampled %>%
  mutate(
    log_distance = log(distance_km + 1),
    log_area     = log(area_ha + 1),
    log_parking  = log(parking_count + 1)
  )

# Select explantory variables
vars <- c("log_distance", "log_area", "log_parking", "mean_canopy", "water_near_lar", 
          "broadleaf_share")


# 3) Estimate MNL -------------------------------------------------------

# Load custom MNL package from GitHub
if (!requireNamespace("mnltools", quietly = TRUE)) remotes::install_github("Olihno/mnltools")
library(mnltools) # ls("package:mnltools"); ?mnl_fit; ?mnl_full

model <- mnl_full(
  Data = choice_sampled,
  X    = vars,
  choiceVar = "choice",
  idVar     = "rid",
  starting.values = rep(0, length(vars))
)
output_formatted <- summary(model); rownames(output_formatted$estimate) <- vars 
output_formatted

choice_sampled %>%
  group_by(rid) %>%
  summarise(n_choice = sum(choice)) %>%
  filter(n_choice != 1)

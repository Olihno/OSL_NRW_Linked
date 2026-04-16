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
    log_area     = log(area_ha + 1),
    log_parking  = log(parking_count + 1)
  )

# Select explantory variables
vars <- c("distance_km", "log_area", "log_parking", "mean_canopy", "water_near_lar", 
          "broadleaf_share")


# 3) Estimate MNL with sampling correction --------------------------------

# Load custom MNL package from GitHub
if (!requireNamespace("mnltools", quietly = TRUE)) remotes::install_github("Olihno/mnltools")
library(mnltools) # ls("package:mnltools"); ?mnl_fit; ?mnl_full

# Starting values
start_vals <- rep(0, length(vars))

# Set starting values for sampling correction (SC) term at 1
fixed_idx <- which("SC"==vars)
start_vals[fixed_idx] <- 1

# Estimate MNL
model <- mnl_full(
  Data = choice_sampled,
  X    = vars,
  choiceVar = "choice",
  idVar     = "rid",
  starting.values = start_vals
)
output_formatted <- summary(model); rownames(output_formatted$estimate) <- vars 
output_formatted


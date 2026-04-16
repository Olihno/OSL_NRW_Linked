# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# Link survey data to forest database, compute distance to all sites, 
# and apply stratified importance sampling by disatnce to full choice set
#
# Oliver Becker (oliver@pksb.de)
# Apr 15 2026
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, sf, purrr)


# 1) Load and prepare data ------------------------------------------------

# Forest patches
fp <- read_sf("01_Data/fp_nrw_new.geojson") %>%
  st_transform(25832)

# Respondent data
df <- read_csv("01_Data/Coordinates_visit_residence_NRW_forest_march26.csv") %>%
  filter(!is.na(latlng_wood_sq_1_1))

# Sf objects from coordinates of forest and home markers
df_forest <- df %>%
  st_as_sf(coords = c("latlng_wood_sq_1_2", "latlng_wood_sq_1_1"), crs = 4326) %>% 
  st_transform(25832)

df_home <- df %>%
  st_as_sf(coords = c("latlng_residence_1_2", "latlng_residence_1_1"), crs = 4326) %>% 
  st_transform(25832)


# 2) Sampling setup -------------------------------------------------------

dist_breaks <- c(0, 5, 15, 30, 60, Inf)   # km
N.alt <- 100

dist_levels <- levels(cut(dist_breaks[-length(dist_breaks)], dist_breaks, right = FALSE))
Jtilde_target <- rep(N.alt / length(dist_levels), length(dist_levels))
names(Jtilde_target) <- dist_levels

sampled_list <- vector("list", nrow(df))
failed_rid <- c()

# 3) Loop over respondents ------------------------------------------------

pb <- txtProgressBar(min = 0, max = nrow(df), style = 3) # Progress bar

for (i in 1:nrow(df)) {
  
  rid_i <- df$rid[i]           # current ID
  
  forest_i <- df_forest[i, ]   # current forest marker
  home_i   <- df_home[i, ]     # current home marker
  
  # chosen patch: first containing polygon, otherwise nearest within 50 m
  inside_i <- st_within(forest_i, fp)[[1]]
  
  chosen_sf_id <- NA_integer_
  
  # Identify visited site
  if (length(inside_i) > 0) {
    chosen_sf_id <- fp$sf_id[inside_i[1]]
  } else {
    nn <- st_nearest_feature(forest_i, fp) # Nearest feature
    d_match <- as.numeric(st_distance(forest_i, fp[nn, ], by_element = TRUE)) # Distance to nearest
    if (d_match <= 50) chosen_sf_id <- fp$sf_id[nn] # Assign if distance < 50m
  }
  
  # Record unsuccesful choice assignments
  if (is.na(chosen_sf_id)) {
    failed_rid <- c(failed_rid, rid_i)
    next
  }
  
  # distance from home to all patches
  dist_km <- as.numeric(st_distance(home_i, fp)) / 1000
  
  # respondent-specific full choice data
  df_i <- fp %>%
    st_drop_geometry() %>%
    mutate(
      rid = rid_i,
      choice = as.integer(sf_id == chosen_sf_id),
      distance_km = dist_km,
      dist_bin = cut(distance_km, dist_breaks, right = FALSE)
    ) %>%
    select(rid, sf_id, choice, distance_km, dist_bin, everything())
  
  # chosen stratum
  i_str <- as.character(df_i$dist_bin[df_i$choice == 1])
  
  # stratum sizes and target draws
  J_rn <- table(df_i$dist_bin) # number of available sites per stratum
  Jtilde <- pmin(Jtilde_target[names(J_rn)], as.numeric(J_rn)) # cut target by availability
  names(Jtilde) <- names(J_rn) # rename necessary if one bin not available
  
  # sampled set with chosen alternative forced in
  idx_keep <- unlist(lapply(names(J_rn), function(s) {
    idx_s <- which(as.character(df_i$dist_bin) == s)
    Jt <- min(Jtilde[s], length(idx_s))
    
    if (s == i_str) {
      i_row <- which(df_i$choice == 1)
      pool <- setdiff(idx_s, i_row)
      c(i_row, if (Jt > 1) sample(pool, min(Jt - 1, length(pool)), replace = FALSE) else integer(0))
    } else {
      if (Jt > 0) sample(idx_s, Jt, replace = FALSE) else integer(0)
    }
  }))
  
  df_sub <- df_i[sort(unique(idx_keep)), ]
  
  # McFadden correction
  q <- ifelse(
    df_sub$choice == 1, 1,
    ifelse(
      as.character(df_sub$dist_bin) == i_str,
      (Jtilde[i_str] - 1) / (J_rn[i_str] - 1),
      Jtilde[as.character(df_sub$dist_bin)] / J_rn[as.character(df_sub$dist_bin)]
    )
  )
  
  df_sub <- df_sub %>%
    mutate(
      SC = -log(q),
      age = df$age[i],
      sex = df$sex[i],
      income_group = df$income_group[i]
    )
  
  sampled_list[[i]] <- df_sub # Write individual subset to list
  setTxtProgressBar(pb, i)    # Update progress
}


# 4) Bind output ----------------------------------------------------------

choice_sampled <- bind_rows(sampled_list)
failed_rid <- tibble(rid = failed_rid)

write_csv(choice_sampled, "01_Data/choice_sampled_dist_stratified.csv")
write_csv(failed_rid, "01_Data/failed_choice_matches.csv")


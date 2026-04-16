# Forest Recreation Demand (NRW)

This project develops a linked model of forest recreation demand in North Rhine-Westphalia.

## Structure

- `01_Data/` → input data (not included)
- `02_R/` → R scripts (`01_prep_sampling.R` → data preparation and sampling, `02_exploratory_MNLs.R`, ...)

## Data

To run the code, place the following files in `01_Data/` (this folder is not included in the repository due to file size and data privacy considerations):

- `fp_nrw_new.geojson` (forest database)
- `Coordinates_visit_residence_NRW_forest_march26.csv` (respondent data)

## Notes

- The full choice set is very large (~15,000 sites), so we use stratified importance sampling (Ben-Akiva and Lerman, 1985) by distance.
- Chosen sites are identified if site marker from online mapping tool falls inside forest polygon or if distance to nearest forest is less than 50 m.
- Observations for which the chosen site cannot be identified based on these criteria (31.7%) are stored in `failed_choice_matches_Apr16.csv` and dropped automatically.

## Usage

Run the scripts in `02_R/` — they build the choice data and apply sampling.

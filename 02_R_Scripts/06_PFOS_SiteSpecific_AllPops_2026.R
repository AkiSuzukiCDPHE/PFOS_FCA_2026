# R SCRIPT FOR MERCURY SITE-SPECIFIC UPDATES TO COMBINE ALL POPULATIONS AND EXPORT ADVISORIES

# This R Script will combine the GP, WCBA, and Child site-specific data into one dataframe
# and rename, reorder, and delete extraneous variables.

# Merge the data  and transform to long####

library(dplyr)
# merge all three pops
FCAs_2026 <-  Advisories_GP |> left_join(
  Advisories_WCBA,
  by = c(
    "Waterbody",
    "Species_Code",
    "Species",
    "Average_Result",
    "Unit",
    "Num_Obs"
  )
) |> left_join(
  Advisories_Children,
  by = c(
    "Waterbody",
    "Species_Code",
    "Species",
    "Analyte",
    "Average_Result",
    "Unit",
    "Num_Obs"
  )
)




library(tidyr)
library(dplyr)

# Pivot longer
FCAs_2026_Long <- FCAs_2026 %>%
  pivot_longer(
    # Do not pivot Waterbody through Num_Obs
    cols = !(Waterbody:Num_Obs),
    
    # Use names_pattern instead of names_sep
    # This captures the Group first, then the Variable name, regardless of underscores
    names_to = c("Population", ".value"),
    names_pattern = "(GP|WCBA|Children)_(.*)"
  )




# Final cleaning steps for final site-specific output ####

# 
# # Filter out species that do not meet sample size requirements and have not been selected for manual review.
# # This sample size will change if the power analysis is rerun and produces a different result.
# FCAs_2026_Long_1 <- FCAs_2026_Long %>%
#   filter(!(Num_Obs<11))


# Subset to df with all new, updated, or potentially lifted advisories
FCAs_2026_Long_1 <- FCAs_2026_Long |>  filter(Rec %in% c(
  "Same as Statewide - Consider removing SS advisory with TAC",
  "Adopt SS advisory (More stringent than statewide)",
  "Adopt updated (Less Stringent than existing SS)",
  "Issue a new site-specific FCA",
  "Consider removing SS advisory with TAC",
  "Review manually"
))


# Upload the CPW Regional Waterbodies
CPWRegions <- read_excel("01_Raw_Data/Lakes_with_CPWAreaRegion_County.xlsx")

# Merge the regional waterbodies with the advisories
PFOS_SS_FCAs_Final_CPW_Regions <- merge(FCAs_2026_Long_1, CPWRegions[, c("Waterbody","REGION")], by = "Waterbody", all.x = TRUE)

# Make sure all rows are distinct
PFOS_SS_FCAs_Final_CPW_Regions1 <- PFOS_SS_FCAs_Final_CPW_Regions %>%
  distinct(Waterbody, Species, Average_Result, Population, .keep_all = TRUE) |> rename (CPW_Region = REGION)

# Rename columns to make them more legible and reorder variables
PFOS_SS_FCAs_Final_CPW_Regions2 <-PFOS_SS_FCAs_Final_CPW_Regions1 %>% rename(
  c(
    `Sample N` = Num_Obs,
    `Proposed (meals per month)` = MealsPerMonth,
    `Existing site-specific (meals per month)` = Current_SS_Per_Month,
    `Existing site-specific status` = SS_Status,
    `Statewide (meals per month)` = Statewide_Per_Month,
    `Statewide status` = State_Status,
    Recommendation = Rec))  |>  mutate(Size= "Any") |> relocate(Size, .after=Commonly_Consumed)


# Exporting the final CENSORED dataframe
# Change file paths


library("writexl")
write_xlsx(PFOS_SS_FCAs_Final_CPW_Regions2,"04_Output/2026_PFOS_SS_Final_FCAs.xlsx")



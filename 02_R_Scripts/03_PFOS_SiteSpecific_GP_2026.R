#SITE SPECIFIC ADVISORIES FOR PFOS FOR THE GENERAL POPULATION

# 1: Calculating new FCAs ####

# Import xlsx files of this years entire PFAS dataset (master dataset)

library(readxl)
library(dplyr)
library(tidyr)

# Correct the file path by using forward slashes or double backslashes.
PFAS_Clean_Original <- read_excel("03_Clean_Data/PFOS_CleanedMaster_2026.xlsx")


# Filter for PFOS
PFOS_Only_Clean_Original <- PFAS_Clean_Original |>  filter(Analyte == "PFOS")


# Filter for waterbodies that have new data in this update.
# Always change year for each update
# Filter the dataset
PFOS_Clean <- PFOS_Only_Clean_Original %>%
  group_by(Waterbody, Species) %>%
  # Keeps the whole group if 'any' row within it has a year matching 2025
  filter(any(Sample_Year %in% c(2025, "2025"))) %>%
  ungroup()



# Censor data by year if necessary (skip this step for now because PFOS data is all <5 years old)


# Calculate the average concentration (result) for each species by waterbody

# The group_by code means that the subsequent calculations will be applied separately for each
# unique combination of Waterbody and Species_Codes.
# The Num_obs=n()) line calculates the number of observations within each group of waterbody and species
# and creates a new variable called Num_Obs to store these counts i.e. it calculates the sample size for the ss
# advisory.

PFOS_SS <- PFOS_Clean %>%
  group_by(Waterbody, Species_Code) %>%
  mutate(Average_Result = mean(Result), Num_Obs = n())


# Obtain the unique rows based on the specified columns from the PFOS_SS data frame.
# The distinct function keeps the distinct rows based on the Waterbody column while preserving all
# other columns specified in the .keep_all argument.
# This operation ensures that duplicate rows based on Waterbody are removed while preserving the values of other variables.

PFOS_SS2 = distinct(PFOS_SS, Waterbody, .keep_all = TRUE) %>%
  select(
    "Waterbody",
    "Species_Code",
    "Species",
    "Commonly_Consumed",
    "Analyte",
    "Average_Result",
    "Unit",
    "Num_Obs"
  )

# Assign meal frequency recommendations for site-specific advisories
# UPDATED WITH PFOS FCLGS IN NG/G

PFOS_SS3 = PFOS_SS2 %>%
  mutate(
    GP_MealsPerMonth = case_when(
      Average_Result > 0 & Average_Result <= .4 ~ 24,
      Average_Result > 0.4 &
        Average_Result <= 0.5 ~ 20,
      Average_Result > 0.5 &
        Average_Result <= 0.7 ~ 16,
      Average_Result > 0.7 &
        Average_Result <= 0.9 ~ 12,
      Average_Result > 0.9 &
        Average_Result <= 1.3 ~ 8,
      Average_Result > 1.3 &
        Average_Result <= 2.7 ~ 4,
      Average_Result > 2.7 &
        Average_Result <= 3.6 ~ 3,
      Average_Result > 3.6 &
        Average_Result <= 5.3 ~ 2,
      Average_Result > 5.3 &
        Average_Result <= 10.7 ~ 1,
      Average_Result > 10.7 &
        Average_Result <= 21.4 ~ .5,
      Average_Result > 21.4 &
        Average_Result <= 42.8 ~ .25,
      TRUE ~ 0
    )
  )



# 2: Comparing to existing SS and Statewide ####



# Merge the data frame with the existing advisories list in order to evaluate which advisories are
# new, updated, or removed in future advisories

# Upload the existing advisories dataset
# This will change every year but always needs to include all existing advisories for both PFOS and Hg.
# Upload new version!!!
Existing_SSAdvisories = read_excel("01_Raw_Data/Existing_Advisories_2026.xlsx", sheet =
                                     1)


# Filter the data frame for the GP
ExistingSS <- Existing_SSAdvisories %>% filter(Population == "General population") |> rename(GP_Current_SS = Current_SS,
                                                                                             GP_Current_SS_Per_Month = Current_SS_Per_Month)




# JOIN SS: Creating new variables in the existing SS advisories dataset and joining to the updates

# Step 1: Clean the reference dataset to create the Yes/No flag
advisory_lookup <- ExistingSS %>%
  mutate(
    # If it says "Unrestricted", it's NOT site-specific. Otherwise, it is.
    GP_SS_Status = ifelse(GP_Current_SS == "Unrestricted", "No", "Yes")
  ) %>%
  # Keep only the columns needed for matching and the new flag
  select(Waterbody, Species, GP_Current_SS_Per_Month, GP_SS_Status)

# Step 2: Join the flag back to your original dataset
PFOS_SS4 <- PFOS_SS3 %>%
  left_join(advisory_lookup, by = c("Waterbody", "Species")) %>%
  mutate(
    # Handle cases where a waterbody/species combo wasn't in the reference data at all
    GP_SS_Status = ifelse(is.na(GP_SS_Status), "No", GP_SS_Status)
  )




# JOIN STATEWIDE

# Upload the statewide advisories dataset
# This only changes every 5-10 years - we will likely not update until 2034
PFOS_Statewide = read_excel("01_Raw_Data/Existing_Advisories_2026.xlsx", sheet =
                              2)


# Transpose to long and add variables
PFOS_Statewide_long <- PFOS_Statewide |> pivot_longer(
  cols = c(
    `General population`,
    `People who may become pregnant`,
    `Children`
  ) ,
  names_to = "Population",
  values_to = "Statewide_Character"
)

# Create a numeric statewide advisory column using the first character oof the character column
PFOS_Statewide_long2 <- PFOS_Statewide_long |>  mutate(
  GP_Statewide_Per_Month = case_when(
    Statewide_Character == "1 meal/week" ~ 4,
    Statewide_Character ==
      "2 meals/week" ~ 8,
    Statewide_Character ==
      "1 meal/month" ~ 1,
    Statewide_Character ==
      "2 meals/month" ~ 2,
    Statewide_Character ==
      "3 meals/month" ~ 3,
    Statewide_Character ==
      "6 meals/year" ~ 0.5,
    Statewide_Character ==
      "unrestricted" ~ NA
  )
)

# Filter for the gen pop
PFOS_Statewide_long3 <- PFOS_Statewide_long2 %>% filter(Population == "General population")


# Step 1: Clean the reference dataset to create the Yes/No flag for a statewide advisory
advisory_lookup_statewide <- PFOS_Statewide_long3 %>%
  mutate(
    # If it is NA it does not have a statewide advisory. Otherwise, it does.
    GP_State_Status = ifelse(is.na(GP_Statewide_Per_Month), "No", "Yes")
  ) %>%
  # Keep only the columns needed for matching and the new flag
  select(Species, GP_Statewide_Per_Month, GP_State_Status)

# Step 2: Join the flag back to your original dataset
PFOS_SS5 <- PFOS_SS4 %>%
  left_join(advisory_lookup_statewide, by = c("Species")) %>%
  mutate(
    # Handle cases where a waterbody/species combo wasn't in the reference data at all
    GP_State_Status = ifelse(is.na(GP_State_Status), "No", GP_State_Status)
  )



# Calculate advisory comparisons


Advisories_GP <- PFOS_SS5 %>%
  mutate(
    GP_Rec = case_when(
      # --- BRANCH 1: Existing Site-Specific Advisory Exists ---
      
      # The updated FCA is less stringent than the existing FCA and > 8 meals per month
      GP_SS_Status == "Yes" &
        (GP_MealsPerMonth > 8) ~ "Consider removing SS advisory with TAC",
      
      
      # The updated FCA is more stringent than the existing FCA
      GP_SS_Status == "Yes" &
        (GP_MealsPerMonth <= 8) &
        (GP_MealsPerMonth < GP_Current_SS_Per_Month) ~ "Adopt updated SS (More Stringent than existing SS)",
      
      # The updated FCA is less stringent than the existing FCA, still <= 8 meals per month, and there is no state wide advisory
      GP_SS_Status == "Yes" &
        (GP_MealsPerMonth <= 8) &
        (GP_MealsPerMonth > GP_Current_SS_Per_Month) &
        GP_State_Status == "No" ~ "Adopt updated (Less Stringent than existing SS)",
      
      # The updated FCA is less stringent than the existing FCA, still <= 8 meals per month, and < the existing state wide advisory
      GP_SS_Status == "Yes" &
        (GP_MealsPerMonth <= 8) &
        (GP_MealsPerMonth > GP_Current_SS_Per_Month) &
        GP_State_Status == "Yes" &
        (GP_MealsPerMonth < GP_Statewide_Per_Month) ~ "Adopt updated (Less Stringent than existing SS)",
      
      # The updated FCA is less stringent than the existing FCA, still <= 8 meals per month, and > the existing state wide advisory
      GP_SS_Status == "Yes" &
        (GP_MealsPerMonth <= 8) &
        (GP_MealsPerMonth > GP_Current_SS_Per_Month) &
        GP_State_Status == "Yes" &
        (GP_MealsPerMonth >= GP_Statewide_Per_Month) ~ "Same as Statewide - Consider removing SS advisory with TAC",
      
      # The updated FCA is the same as the existing FCA
      GP_SS_Status == "Yes" &
        (GP_MealsPerMonth <= 8) &
        (GP_MealsPerMonth == GP_Current_SS_Per_Month) ~ "Retain existing SS (No change)",
      
      # Thupdated
      
      
      # --- BRANCH 2: No Site Advisory, No Statewide Advisory ---
      
      # No site-specific and no statewide advisory but a meal rec <= 8
      GP_SS_Status == "No" &
        GP_State_Status == "No" &
        (GP_MealsPerMonth <= 8) ~ "Issue a new site-specific FCA",
      
      # No site-specific and no statewide advisory and a meal rec > 8
      GP_SS_Status == "No" &
        GP_State_Status == "No" &
        (GP_MealsPerMonth > 8)  ~ "No advisory (No statewide & no SS)",
      
      
      
      # --- BRANCH 3: No Site Advisory, But Has Statewide Advisory ---
      
      # Updated FCA is is more stringent than the statewide FCA
      GP_SS_Status == "No" &
        GP_State_Status == "Yes" &
        (GP_MealsPerMonth <= 8) &
        (GP_MealsPerMonth < GP_Statewide_Per_Month) ~ "Adopt SS advisory (More stringent than statewide)",
      
      # Updated FCA is <=8 but it is less stringent (or equal to) the existing statewide advisory
      GP_SS_Status == "No" &
        GP_State_Status == "Yes" &
        (GP_MealsPerMonth <= 8) &
        (GP_MealsPerMonth >= GP_Statewide_Per_Month) ~ "No action (Defer to statewide advisory)",
      
      # Updated FCA is >8 meals per month i.e., above the threshold for an advisory
      GP_SS_Status == "No" &
        GP_State_Status == "Yes" &
        (GP_MealsPerMonth > 8) ~ "No action (Defer to statewide advisory)",
      
      # Catch-all safety net for missing data or typos
      TRUE ~ "Review manually"
    )
  )

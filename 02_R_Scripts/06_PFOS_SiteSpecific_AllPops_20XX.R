# R SCRIPT FOR PFOS SITE-SPECIFIC ADVISORIES TO JOIN ALL POPULATION DATAFRAMES AND EXPORT ADVISORIES

# This R Script will combine the GP, WCBA, and Child site-specific data frames into one
# dataframe and rename, reorder, and delete extraneous variables.

# After 5 years (2029) this script will need to be edited to include a process for censoring data by time and combining data
# to meet minimum sample size requirements.

library(dplyr)

# Merge data from all three population's dataframe's

# Change variables to reflect PFOS dataset's variable names
PFOS_merged_all_1 <- merge(merge(PFOS_GP_SS_Merged, PFOS_WCBA_SS_Merged,
                                 by = c("Waterbody", "Species_Codes", "Species", "Analyte", "Average_Result", "Num_Obs", "Commonly_Consumed"), all = TRUE),
                           PFOS_Children_SS_Merged, by = c("Waterbody", "Species_Codes", "Species", "Analyte", "Average_Result", "Num_Obs", "Commonly_Consumed"), all = TRUE)



# Rename variables to be easily understood in output
PFOS_merged_all_2 <- PFOS_merged_all_1 %>% 
  rename(
    # Use once there is an existing PFOS SS
    # GP_Existing_SW = In_Existing_Advisory.x,
    # WCBA_Existing_SW = In_Existing_Advisory.y,
    # Child_Existing_SW = In_Existing_Advisory,
    # 
    # GP_Existing_SS = MealsMonth_ExistingSS.x, 
    # WCBA_Existing_SS = MealsMonth_ExistingSS.y,
    # Child_Existing_SS= MealsMonth_ExistingSS,  
    # 
    # GP_Status_Existing_SS = Existing_SS_Advisory.x, 
    # WCBA_Status_Existing_SS = Existing_SS_Advisory.y,
    # Child_Status_Existing_SS= Existing_SS_Advisory, 
    # Population_GP = Population.x, 
    # Population_WCBA = Population.y, 
    # Population_Child = Population, 
    
    GP_SS_Status = New_SS.x, 
    WCBA_SS_Status = New_SS.y,
    Child_SS_Status = New_SS,
    
    GP_Meals_SS = GP_MealsPerMonthSS, 
    WCBA_Meals_SS = WCBA_MealsPerMonthSS, 
    Child_Meals_SS = Children_MealsPerMonthSS,
    
    GP_Meals_SW = GP_MealsPerMonthSW, 
    WCBA_Meals_SW = WCBA_MealsPerMonthSW, 
    Child_Meals_SW = Children_MealsPerMonthSW,
    
  )

# Add units
PFOS_merged_all_2$Units = "ng/g" 


# Reordering variables
# Customize for variables in PFOS dataset
PFOS_merged_all_3 <- PFOS_merged_all_2 %>%
  select(
    Waterbody, Species_Codes, Old_Species_Codes, Species, Average_Result, Units, Num_Obs, Analyte, Commonly_Consumed, Length_Inches,
    
    GP_Meals_SS, GP_SS_Status, GP_Meals_SW,  Population_GP, GP_Status_Existing_SS, GP_Existing_SS,
    
    WCBA_Meals_SS, WCBA_SS_Status, WCBA_Meals_SW, Population_WCBA, WCBA_Status_Existing_SS, WCBA_Existing_SS,
    
    Child_Meals_SS, Child_SS_Status, Child_Meals_SW, Population_Child, Child_Status_Existing_SS, Child_Existing_SS,
  )


# Deleting variables
PFOS_merged_all_4 <- subset(PFOS_merged_all_3, select = -c(GP_Existing_SW, WCBA_Existing_SW, Child_Existing_SW) )


# 2025 onwards subset data to only include water bodies with new data from the past year's sampling effort.


# For 2024 update - This script subsets to a dataframe where GP and/or WCBA and/or Children have new site-specific advisories.
# For PFOS, unlike Hg, only need rows with new advisories because there are no existing advisories to be lifted/updated.
# In 2025 we will need to review the mercury process in the Hg template if there are any updates to existing PFOS advisories (we resample any of the 2023 lakes)
PFOS_SS_New_FINAL_20XX <- PFOS_merged_all_4[PFOS_merged_all_4$GP_SS_Status == "Yes" 
                                         | PFOS_merged_all_4$WCBA_SS_Status == "Yes" 
                                         | PFOS_merged_all_4$Child_SS_Status == "Yes", ]


# Export the dataset and manually review w/TAC to decide whether to issue any proposed advisories that do not meet sample size requirements.

# Exporting the final dataframe
# install.packages("writexl")
# library("writexl")
# write_xlsx(PFOS_SS_New_FINAL_20XX,"INSERT FILEPATH")








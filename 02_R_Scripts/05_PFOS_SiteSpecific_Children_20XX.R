#SITE SPECIFIC ADVISORIES FOR PFOS FOR Children - 2024

library(dplyr)


# Create a new data frame from the data frame created in the data cleaning R file for the PFOS data
PFOS_Clean <-PFOS_Clean_Original

# Rename variables if necessary
PFOS_Clean <- PFOS_Clean %>% rename (Analyte= COMPOUND)

# Censor data by year if necessary (skip this step because PFOS data is all <5 years old)


# Calculate the average concentration (result) for each species by waterbody
# The group_by code means that the subsequent calculations will be applied separately for each
# unique combination of Waterbody and Species_Codes.
# The Num_obs=n()) line calculates the number of observations within each group of waterbody and species
# and creates a new variable called Num_Obs to store these counts i.e. it calculates the sample size for the ss
# advisory.


PFOS_SS <- PFOS_Clean %>%
  group_by(Waterbody,Species_Codes) %>%
  mutate(Average_Result = mean(Result),
         Num_Obs = n())

# Obtain the unique rows based on the specified columns from the PFOS_SS data frame.
# The distinct function keeps the distinct rows based on the Waterbody column while preserving all
# other columns specified in the .keep_all argument.
# This operation ensures that duplicate rows based on Waterbody are removed while preserving the values of other variables.

PFOS_SS= distinct(PFOS_SS, Waterbody, .keep_all = TRUE) %>%
  select("Waterbody","Species_Codes","Species", "Analyte","Average_Result", "Num_Obs")


# Assign meal frequency recommendations for site-specific advisories
# UPDATED WITH PFOS FCLGS IN NG/G

PFOS_SS2=PFOS_SS %>%
  mutate(Children_MealsPerMonth=case_when(Average_Result>0 & Average_Result <=0.15 ~24,
                                      Average_Result>0.15 & Average_Result <=0.18 ~20,
                                      Average_Result>0.18 & Average_Result <=0.22 ~16,
                                      Average_Result>0.22 & Average_Result <=0.3 ~12,
                                      Average_Result>0.3 & Average_Result <=.45 ~8,
                                      Average_Result>.45 & Average_Result <=0.89 ~4,
                                      Average_Result>0.89 & Average_Result <=1.19 ~3,
                                      Average_Result>1.19 & Average_Result <=1.78 ~2,
                                      Average_Result>1.78 & Average_Result <=3.56 ~1,
                                      Average_Result>3.56 & Average_Result <=7.12 ~.5,
                                      Average_Result>7.12 & Average_Result <=14.25 ~.25,
                                      TRUE~0))


# Is there an existing site-specific advisory for PFOS at any of the waterbodies?
# NOin 2024 because it was the first year of site-specific advisories.
# 2025 updates --> insert code to compare proposed advisories to existing advisories.
# Refer to the Hg FCA template to copy example of how to compare existing advisories to updated advisories.


# Check if species is listed on the statewide advisory for Mercury

# Upload the existing statewide advisories data set.
library(readxl)
Hg_Statewide_Final_2024=read_excel("X:\\Shared drives\\_CDPHE TEEO TARA\\PFAS 🔥\\Data Integration and Assessment\\Fish\\FCAs\\PFOS FCAs\\Annual FCA Updates\\PFOS_FCA_2024\\04_Output\\Statewide_Hg_Advisory\\Hg_Statewide_Final_2024.xlsx")


# Merge the data frame PFOS_SS2 with the statewide Hg advisory data frame.

  # The select function is used to choose only the columns Species_Codes and Children_MealsPerMonth
  # from Hg_Statewide_Final_2024.
  
  # The left_join function is used to perform a left join between the PFOS dataframe PFOS_SS2 and the
  # result of the select operation.
  
  # The by = "Species_Codes" argument specifies the column used for the join, which is Species_Codes.
  # Rows from PFOS_SS2 that have matching values in the Species_Codes column with
  # Hg_Statewide_Final_2024 will have corresponding values from Hg_Statewide_Final_2024 included. Rows without a
  # match will have NA values for the columns from Hg_Statewide_Final_2024.


PFOS_SS3 <- left_join(PFOS_SS2, Hg_Statewide_Final_2024 %>% select(Species_Codes, Children_MealsPerMonth, Commonly_Consumed),
                      by = "Species_Codes")


# Renaming the variables for Children_MealsPerMonth in the new merged dataframe

PFOS_SS4 <- PFOS_SS3  %>%
  rename(Children_MealsPerMonthSS = Children_MealsPerMonth.x, Children_MealsPerMonthSW = Children_MealsPerMonth.y)


# Filtering the data frame to subset species that are on the statewide advisory (<= 8 meals per month)
# and species that are not on the statewide advisory (> 8 meals per month)

PFOS_SS5a <- PFOS_SS4 %>% filter(Children_MealsPerMonthSW <=8) # Species that have a statewide advisory

PFOS_SS5b <- PFOS_SS4 %>% filter(Children_MealsPerMonthSW > 8) # Species that DO NOT have a statewide advisory


# FOR SPECIES WITH A STATEWIDE ADVISORY (<= 8 meals per month)

# Create a new variable to check if the site-specific advisory for a species is more stringent than the statewide advisory.

# The group_by function groups the data by the variable Species_Codes.The subsequent operations will
# be applied separately within each group
# The value of this variable is determined based on whether the corresponding value of Children_MealsPerMonthSS is
# less than the corresponding value of Children_MealsPerMonthSW within each group defined by Species_Codes. 
# If true, the value of the variable New_SS is "Yes"; otherwise, it's "No".

PFOS_SS6a <- PFOS_SS5a %>%           
  group_by(Species_Codes) %>%
  mutate(New_SS = case_when(
    Children_MealsPerMonthSS < Children_MealsPerMonthSW ~ "Yes",
    TRUE ~ "No"
  )) 


# FOR SPECIES WITHOUT A STATEWIDE ADVISORY

# Use a new variable called New_SS to determine whether species will have a new site-specific
# advisory based on whether species meal recs  are <= 8 meals/month

PFOS_SS6b <- PFOS_SS5b %>%
  mutate(New_SS = case_when(
    Children_MealsPerMonthSS <= 8 ~ "Yes",
    TRUE ~ "No"
  ))


# Merge all final data frames into one data frame that has a variable New_SS indicating species at waterbodies
# that require issuing a site-specific advisory for PFOS

# There should be two data frames merged:

# (1) Data frame for species with an existing statewide advisory.
# PFOS_SS6a

# (2) Data frame for species without a statewide advisory.
# PFOS_SS6b


PFOS_Children_SS_Merged <- bind_rows(PFOS_SS6a, PFOS_SS6b)




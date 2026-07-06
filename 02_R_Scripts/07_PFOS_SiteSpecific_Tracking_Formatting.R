# Cleaned samples for website
# Every year do this with only new data from the past year's sampling effort.


# Import xlsx files of PFOS data 

install.packages("readxl")
library(readxl)
library(dplyr)

options(scipen = 999)

# Correct the file path by using forward slashes or double backslashes
PFOSData_Raw <- read_excel("X:/Shared drives/_CDPHE TEEO TARA/PFAS 🔥/Data Integration and Assessment/Fish/FCAs/PFOS FCAs/Annual FCA Updates/PFOS_FCA_2024/01_Raw_Data/PFOS_2023Data_Validated.xlsx")

# Remove rejected samples
PFOSData_Raw <- PFOSData_Raw %>% filter(VAL_Qual != "R")

colnames(PFOSData_Raw)

# Selected only needed variables
PFOSData_Raw <- PFOSData_Raw %>% select (Species,COMPOUND, VAL_Result, UNIT, ANALYSIS_DATE,  Waterody, Length_mm, Length_Inches )

# Create a new variable for result in mg/kg and create new variable unit = mg/kg

PFOSData_Raw$Result <- PFOSData_Raw$VAL_Result *0.001
PFOSData_Raw$Units <- "mg/kg"


# Remove the old units variable "UNIT" and old VAL_Results variable
PFOSData_Formatted <- PFOSData_Raw %>% select(- UNIT)
PFOSData_Formatted <- PFOSData_Formatted %>% select(- VAL_Result)

# Rename variables to match other results formatting for the website

PFOSData_Formatted <- PFOSData_Formatted %>% rename (Common_Name =Species, Analyte1=COMPOUND, `Submit Date` =ANALYSIS_DATE, Waterbody1=Waterody, `Total Length (mm)`=Length_mm, `Total Length (in)`=Length_Inches)
PFOS_FishSamples_Formatted_2024Update <- PFOSData_Formatted %>% select (Common_Name,	Analyte1,	Result,	Units,	`Submit Date`,	Waterbody1,	`Total Length (mm)`,	`Total Length (in)`)



# Exporting the final dataframe
# install.packages("writexl")

library("writexl")
write_xlsx(PFOS_FishSamples_Formatted_2024Update,"X:\\Shared drives\\_CDPHE TEEO TARA\\PFAS 🔥\\Data Integration and Assessment\\Fish\\FCAs\\PFOS FCAs\\Annual FCA Updates\\PFOS_FCA_2024\\04_Output\\SiteSpecific_PFOS_2024_VALIDATED\\PFOS_FishSamples_Formatted_2024Update.xlsx")




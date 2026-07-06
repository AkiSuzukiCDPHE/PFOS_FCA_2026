#Final DF "PFOSData_Clean"


# Import xlsx files of PFOS data 

install.packages("readxl")
library(readxl)
library(dplyr)

# Correct the file path by using forward slashes or double backslashes
PFOSData_Raw <- read_excel("X:/Shared drives/_CDPHE TEEO TARA/PFAS 🔥/Data Integration and Assessment/Fish/FCAs/PFOS FCAs/Annual FCA Updates/PFOS_FCA_2024/01_Raw_Data/PFOS_2023Data_Validated.xlsx")


# Add underscore to all column names
library(dplyr)
library(stringr)

# Function to add underscore only to multi-word variable names
add_underscore <- function(name) {
  if (str_detect(name, " ")) {
    str_replace_all(name, " ", "_")
  } else {
    name
  }
}

# Apply the function to all column names
PFOSData_Raw <- PFOSData_Raw %>%
  rename_with(~ sapply(., add_underscore))

# Display the modified column names to verify
colnames(PFOSData_Raw)

# Run frequency tables to see values per variable
table(PFOSData_Raw$Species)
table(PFOSData_Raw$Waterbody)
table(PFOSData_Raw$COMPOUND)

# Filter for PFOS
PFOSData_Raw1 <- PFOSData_Raw %>% 
  filter(COMPOUND == "PFOS")


# Select only necessary columns
PFOSData_Raw2 <- PFOSData_Raw1 %>% select (Waterbody, Species, COMPOUND, CAS_NO, VAL_Result, VAL_Qual, MDL, SAMPLE_NO, Sampling_date, UNIT)%>% filter (UNIT != "ng/g")


# Remove rejected samples
PFOSData_Raw2 <- PFOSData_Raw2 %>% filter (VAL_Qual != "R")

# Replace values that are UJ flagged with the MDL, other wise keep values as is. J flagged just means below the reporting limit estimated values that can be kept as is.
PFOSData_Raw2 <- PFOSData_Raw2 %>% mutate (VAL_Result = ifelse(VAL_Qual == "UJ", MDL, VAL_Result))

# Format sample date as date
class(PFOSData_Raw2$Sampling_date)
PFOSData_Raw2$Sampling_date <- as.Date(PFOSData_Raw2$Sampling_date, format = "%m/%d/%Y")

# Assign species codes to species
PFOS_Clean <- PFOSData_Raw2 %>%
  mutate(Species_Codes = case_when(Species == "Smallmouth Bass"~ "SMB",
                                   Species ==  'Walleye'~ "WAL",
                                   Species == "Black Bullhead" ~ "BBH",
                                   Species == 'Saugeye' ~ "SAG",
                                   Species == "Yellow Perch" ~ "YPE",
                                   Species == "Rainbow Trout" ~ "RBT",
                                   Species == "White Crappie" ~ "WCR",
                                   Species == "Spotted Bass" ~ "SPB"))


# Remove observations where Waterbody is NA
PFOS_Clean <- PFOS_Clean %>% filter(!is.na(Waterbody)) %>% rename (Result=VAL_Result)

class(PFOS_Clean$Result)

# This is the dataset you need to merge with the Master Cleaned dataset from the previous year

# Add in merge steps and export

# Export here


library("writexl")
write_xlsx(PFOS_Clean,"X:/Shared drives/_CDPHE TEEO TARA/PFAS 🔥/Data Integration and Assessment/Fish/FCAs/PFOS FCAs/Annual FCA Updates/PFOS_FCA_2024/03_Clean_Data/PFOS_Clean_2023_VALIDATED.xlsx")



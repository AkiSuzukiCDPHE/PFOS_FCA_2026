# R SCRIPT FOR CLEANING THE NEW PFOS DATA & MERGING WITH EXISTING DATA

# Importing new data ####

# Importing xlsx files of raw data from the past year's sampling efforts
library(readxl)
library(dplyr)

# Locate the directory
getwd()

# Import the new data
PFOSData <- read_excel("01_Raw_Data/2025_FCA_PFOS_Validated.xlsx")


# Cleaning the new data ####

# Remove samples where tissue type = "O" or "E" (keep M & plug only).
PFOSData_1 <- PFOSData |>  filter(Tissue_Type %in% c('M', 'Plug'))

# Run a frequency table to make sure filtering for M and Plug worked
table(PFOSData_1$Tissue_Type)

# Recreate the values for species based on species code
# This catches any typos or issues in the raw data if the name was incorrect or if they lab labeled big and small fish sizes.
#NPK & NPKB = Northern pike"
#WALB & WAL = "Walleye"
#SMBB & SMB = "Smallmouth bass"
#LMBB & LMB = "Largemouth bass"
#AGB & SAG = Saugeye
#NAT = "Cutthroat"
#SRN = "Cutthroat"
#RGN = "Cutthroat"
#CRN = "Cutthroat"
#RXN = "Rainbow trout"


library(dplyr)
PFOSData_2 <- PFOSData_1 %>%
  mutate(
    Species = case_when(
      Species_Code == "SMB" ~ "Smallmouth Bass",
      Species_Code == "SMBB" ~ 'Smallmouth Bass',
      Species_Code == "LMB" ~ 'Largemouth Bass',
      Species_Code == "LMBB" ~ 'Largemouth Bass',
      Species_Code == "TGM" ~ 'Tiger Muskie',
      Species_Code == "NPK" ~ 'Northern Pike',
      Species_Code == "NPKB" ~ 'Northern Pike',
      Species_Code == "WAL" ~ 'Walleye',
      Species_Code == "WALB" ~ 'Walleye',
      Species_Code == "BCR" ~ "Black Crappie",
      Species_Code == "BGL" ~ "Bluegill",
      Species_Code == "BBH" ~ "Black Bullhead",
      Species_Code == "CPP" ~ 'Common Carp',
      Species_Code == "CCF" ~ 'Channel Catfish',
      Species_Code == "MAC" ~ "Lake Trout(Mackinaw)",
      Species_Code == "SAG" ~ 'Saugeye',
      Species_Code == "SAGB" ~ "Saugeye",
      Species_Code == "SPL" ~ "Splake",
      Species_Code == "SBS" ~ "Striped Bass",
      Species_Code == "SNF" ~ "Green Sunfish",
      Species_Code == "WBA" ~ "White Bass",
      Species_Code == "SXW" ~ "Wipe",
      Species_Code == "WHS" ~ "White Sucker",
      Species_Code == "YPE" ~ "Yellow Perch",
      Species_Code == "RXC" ~ "Rainbow Trout x Cutthroat",
      Species_Code == "RXN" ~ "Rainbow Trout",
      Species_Code == "SRN" ~ "Cutthroat",
      Species_Code == "RGN" ~ "Cuttrhoat",
      Species_Code == "BRK" ~ "Brook Trout",
      Species_Code == "RBT" ~ "Rainbow Trout",
      Species_Code == "KOK" ~ "Kokanee",
      Species_Code == "LOC" ~ "Brown Trout",
      Species_Code == "DRM" ~ "Freshwater Drum",
      Species_Code == "PKS" ~ "Pumpkinseed",
      Species_Code == "SGR" ~ "Sauger",
      Species_Code == "WCR" ~ "White Crappie",
      Species_Code == "NAT" ~ "Cutthroat",
      Species_Code == "CRN" ~ "Cutthroat",
      Species_Code == "LGS" ~ "Longnose Sucker",
      Species_Code == "GRA" ~ "Arctic Grayling",
      Species_Code == "SQF" ~ "Colorado Pikeminnow",
      Species_Code == "CFI" ~ "Crayfish",
      Species_Code == "FMS" ~ "Flannelmouth Sucker",
      Species_Code == "GSD" ~ "Gizzard Shed",
      Species_Code == "GSF" ~ "Golden Shiner maybe (GSH)",
      Species_Code == "HBG" ~ "Hybrid Bluegill",
      Species_Code == "SPB" ~ "Spotted Bass",
      Species_Code == "TRT" ~ "Trout - unspecified",
      Species_Code == "SXX" ~ "White bass x wiper backcross",
      TRUE ~ Species
    )
  )



# Rename water bodies in the new data set where there is a discrepancy with the old master data set
# (manually check with previous years clean master dataset and the fish data template)
# This should not be common as the lab uses the data template.

# Run a frequency table to view waterbodies
table(PFOSData_2$Waterbody)

# Rename waterbodies if needed
# PFOSData_2 <- PFOSData_1 %>% mutate(Waterbody = case_when(Waterbody == "Jumbo Annex" ~ "Jumbo Lake"),
#           TRUE ~ Waterbody)


# Lump subspecies together in the species code
PFOSData_3 = PFOSData_2 %>%
  mutate(
    Species_Code = case_when(
      Species_Code == "SMBB" ~ "SMB",
      Species_Code == "LMBB" ~ 'LMB',
      Species_Code == "NPKB" ~ 'NPK',
      Species_Code == "WALB" ~ 'WAL',
      Species_Code == "SAGB" ~ "SAG",
      Species_Code == "RXN" ~ "RBT",
      Species_Code == "SRN" ~ "NAT",
      Species_Code == "RGN" ~ "NAT",
      Species_Code == "CRN" ~ "NAT",
      TRUE ~ Species_Code
    )
  )




# Replace non-detect values with the MDL
PFOSData_4 <- PFOSData_3 %>% mutate(MDL = as.character(MDL)) |>
  mutate(Result = case_when(Qualifier %in% c("U", "BDL") ~ MDL, TRUE ~ (Result)))



# Merge with Master Dataset ####

# Import the master dataset from the previous year (already cleaned). This will be in the 03_Clean_Data folder
PFOSData_Master <- read_excel("03_Clean_Data/PFOS_CleanedMaster_2024.xlsx")


# check variable names in new data and master dataset to make sure they are the same
colnames(PFOSData_Master)
colnames(PFOSData_4)


# Rename variables with long names or names that are not the same as the master dataset you will eventually merge the new data with.
# PFOSData <- PFOSData %>% rename ("Result" ="PFOS (mg/kg)", "Waterbody" = "Waterbody1")


# Reconcile different data types in the master vs new dataset
PFOSData_Master$`Length_mm` <- as.numeric(PFOSData_Master$`Length_mm`)
PFOSData_Master$`Weight (g)` <- as.numeric(PFOSData_Master$`Weight_g`)
PFOSData_Master$Length_Inches <- as.numeric(PFOSData_Master$Length_Inches)
PFOSData_4$Result <- as.numeric(PFOSData_4$Result)
PFOSData_4$MDL <- as.numeric(PFOSData_4$MDL)


# Merge master dataset with new cleaned dataframe
# Combine dataframes by appending rows

PFOSData_Merged <- bind_rows(PFOSData_Master, PFOSData_4, .id = "Source")

colnames(PFOSData_Merged)

# Deselect the columns you do NOT want to keep
PFOSData_Merged1 <-  PFOSData_Merged |> select(
  -c(
    `CAS Number`,
    `Percent Recovered`,
    `Composite ID`,
    Analysis_Date,
    Lab_Method,
    Report_Date,
    `Lab Comment`,
    Dissolved_Oxygen,
    `Weight_g`,
    pH,
    Conductivity,
    Prep_Date,
    Water_Temperature,
    `Submit_Date`:`Report_Date`
  )
)


library(lubridate)
# Filter out crayfish and trout unspecified because these Species_Code should not be included in the cleaned data
# Trout-unspecified is not a Species_Code and Crayfish is not a fish.
PFOSData_Merged2 <- subset(PFOSData_Merged1,
                           !Species %in% c("Crayfish", "Trout - unspecified")) |> mutate(Sample_Year =
                                                                                           year(Sample_Date))


# Create new column denoting whether a Species_Code is in the existing statewide advisory.
# Double check these are still true each year using the FCA dashboard's General advisory

PFOSData_Merged3 <- PFOSData_Merged2 %>%
  mutate(Statewide_Advisory = case_when(
    Species %in% c(
      "Arctic Grayling",
      "Colorado Pikeminnow",
      "Rainbow Trout x Cutthroat",
      "Spotted Bass",
      "White bass x Wiper backcross",
      "Pumpkinseed",
      "Hybrid Bluegill"
    )
    ~ "No",
    TRUE ~ "Yes"
  ))



# Create a new variable for whether a fish Species_Code is commonly consumed. This is unlikely to chnage often.
PFOSData_Merged4 <- PFOSData_Merged3 %>%
  mutate(Commonly_Consumed = case_when(
    Species_Code %in% c("SQF", "CFI", "FMS", "GSD", "GSF") ~ "No",
    TRUE ~ "Yes"
  ))



# Relocate columns
PFOSData_Clean <- PFOSData_Merged4 %>%
  relocate(Sample_Year, .after = Sample_Date)

# This is your cleaned data frame you will use in subsequent scripts.
PFOSData_Clean

# Export the data frame as a cleaned master dataset ####
# Title with the following format: “PFOS_CleanedMaster_CurrentYear”


# Export to the 03_Clean_Data folder in the project. Change the output year each time. Also save to the drive in the PFOS Data By Year folder
library("writexl")
write_xlsx(PFOSData_Clean,
           ("03_Clean_Data/PFOS_CleanedMaster_2026.xlsx"))

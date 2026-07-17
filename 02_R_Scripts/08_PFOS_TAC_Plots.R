
library(ggplot2)
library(dplyr)
library(readxl)

# Importing the final dataset with TAC recommendations####

# Locate the directory
getwd()

# Import the data
PFOS_Clean_All_Data<- read_excel("03_Clean_Data/PFOS_CleanedMaster_2026.xlsx") |> filter(Analyte =="PFOS")


# TAC Plots Children ####


# Filter for the waterbodies and species with recommendations for children that need to be discussed at the TAC
# Filter for the censored data unless the discussion includes data with combined data
PFOS_SS_TAC_Children <- PFOS_Clean_All_Data %>% 
  filter((Waterbody == "Rocky Mountain Lake" &
            Species  %in% c("Black Crappie", "Largemouth Bass")) |
           (Waterbody == "Pueblo Reservoir" &
              Species == "Spotted Bass"
           )
  )

# Calculate the average concentration (result) for each species by waterbody.
PFOS_SS_TAC_Children2 <- PFOS_SS_TAC_Children %>%
  group_by(Waterbody, Species) %>%
  mutate(Average_Result = mean(Result), Num_Obs = n())



# Create a combined X-axis column with a newline character (\n)
PFOS_SS_TAC_Children2 <- PFOS_SS_TAC_Children2%>%
  mutate(x_label = paste(Species, Num_Obs, Waterbody, sep = "\n"))



ggplot(PFOS_SS_TAC_Children2, aes(x = x_label)) +
  # 1. Plot individual concentration points (blue circles)
  geom_point(aes(y = Result), color = "#808080", size = 3, alpha = 0.7) +
  
  # 2. Plot the average results (green diamonds)
  geom_point(aes(y = Average_Result), color = "#2a2a2a", shape = 18, size = 5) +
  
  # 3. Add horizontal threshold lines
  
  geom_hline(yintercept = 14.25, color = "#4BACC6", linewidth = 1) + # 0.25 meals/month
  geom_hline(yintercept = 7.12, color = "#e40b0b", linewidth = 1) + # 0.5 meal/month
  geom_hline(yintercept = 3.56, color = "#fd9e02", linewidth = 1) + # 1 meal per month
  # geom_hline(yintercept = 1.78, color = "#ffb703", linewidth = 1) + # 2 meal per month
  # geom_hline(yintercept = 1.19, color = "#126782", linewidth = 1) + # 3 meal per month
  
  
  
  # 4. Add threshold text labels on the left side
  annotate("text", x = 0.5, y = 16, label = "DO NOT EAT",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 10, label = "0.25 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 5.5, label = "0.5 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 2, label = ">=1 meal/month",  hjust = 0, size = 3.5) +
  # annotate("text", x = 0.5, y = 1.5, label = "2 meals per month",  hjust = 0, size = 3.5) +
  # annotate("text", x = 0.5, y = .05, label = ">=3 meals per month",  hjust = 0, size = 3.5) +
  
  
  
  # 5. Formatting axes and limits
  scale_y_continuous(limits = c(0, 54), breaks = seq(0, 54, by = 10)) +
  labs(x = NULL, y = NULL) + # Removes default axis titles to match your clean look
  
  # 6. Styling the theme to match a clean grid
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "#E0E0E0"),
    panel.grid.major.y = element_line(color = "#E0E0E0"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, color = "black", vjust = 0.5),
    axis.text.y = element_text(size = 11, color = "black")
  )




# TAC Plots Women ####


# Filter for the waterbodies and species with recommendations for Women that need to be discussed at the TAC
# Filter for the censored data unless the discussion includes data with combined data
PFOS_SS_TAC_Women <- PFOS_Clean_All_Data  |>
  filter((Waterbody == "Rocky Mountain Lake" &
            Species  %in% c("Black Crappie", "Largemouth Bass")) |
           (Waterbody == "Pueblo Reservoir" &
              Species == "Spotted Bass"
           )
  )

# Calculate the average concentration (result) for each species by waterbody.
PFOS_SS_TAC_Women2 <- PFOS_SS_TAC_Women %>%
  group_by(Waterbody, Species) %>%
  mutate(Average_Result = mean(Result), Num_Obs = n())



# Create a combined X-axis column with a newline character (\n)
PFOS_SS_TAC_Women2 <- PFOS_SS_TAC_Women2%>%
  mutate(x_label = paste(Species, Num_Obs, Waterbody, sep = "\n"))



ggplot(PFOS_SS_TAC_Women2, aes(x = x_label)) +
  
  
  
  # 3. Add horizontal threshold lines
  
  # geom_hline(yintercept = 42.8, color = "#4BACC6", linewidth = 1) + # 0.25 meals/month
  geom_hline(yintercept = 21.4, color = "#e40b0b", linewidth = 1) + # 0.5 meal/month
  geom_hline(yintercept = 10.7, color = "#fd9e02", linewidth = 1) + # 1 meal per month
  geom_hline(yintercept = 5.3, color = "#ffb703", linewidth = 1) + # 2 meal per month
  geom_hline(yintercept = 3.6, color = "#126782", linewidth = 1) + # 3 meal per month
  geom_hline(yintercept = 2.7, color = "#219ebc", linewidth = 1) + # 4 meal per month
  geom_hline(yintercept = 1.3, color = "#8ecae6", linewidth = 1) + # 8 meal per month
  
  
  # 1. Plot individual concentration points (blue circles)
  geom_point(aes(y = Result), color = "#808080", size = 3, alpha = 0.7) +
  
  # 2. Plot the average results (green diamonds)
  geom_point(aes(y = Average_Result), color = "#2a2a2a", shape = 18, size = 5) +
  

  
  
  # 4. Add threshold text labels on the left side
  # annotate("text", x = 0.5, y = 1.20, label = "DO NOT EAT",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 30, label = "0.25 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 15, label = "0.5 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 8, label = "1 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 4.6, label = "2 meals per month",  hjust = 0, size = 3.5) +
  # annotate("text", x = 0.5, y = 3, label = "3 meals per month",  hjust = 0, size = 3.5) +
  # annotate("text", x = 0.5, y = 2, label = "4 meals per month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = .08, label = ">= 8 meals per month",  hjust = 0, size = 3.5) +
  
  
  # 5. Formatting axes and limits
  scale_y_continuous(limits = c(0, 53.5), breaks = seq(0, 53.5, by = 10)) +
  labs(x = NULL, y = NULL) + # Removes default axis titles to match your clean look
  
  # 6. Styling the theme to match a clean grid
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "#E0E0E0"),
    panel.grid.major.y = element_line(color = "#E0E0E0"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, color = "black", vjust = 0.5),
    axis.text.y = element_text(size = 11, color = "black")
  )




# TAC Plots General Population ####


# Filter for the waterbodies and species with recommendations for Women that need to be discussed at the TAC
# Filter for the censored data unless the discussion includes data with combined data
PFOS_SS_TAC_GP <- PFOS_Clean_All_Data |>
  filter((Waterbody == "Rocky Mountain Lake" &
            Species  %in% c("Black Crappie", "Largemouth Bass")) |
           (Waterbody == "Pueblo Reservoir" &
              Species == "Spotted Bass"
           )
  )

# Calculate the average concentration (result) for each species by waterbody.
PFOS_SS_TAC_GP2 <- PFOS_SS_TAC_GP %>%
  group_by(Waterbody, Species) %>%
  mutate(Average_Result = mean(Result), Num_Obs = n())



# Create a combined X-axis column with a newline character (\n)
PFOS_SS_TAC_GP2 <- PFOS_SS_TAC_GP2%>%
  mutate(x_label = paste(Species, Num_Obs, Waterbody, sep = "\n"))



ggplot(PFOS_SS_TAC_GP2, aes(x = x_label)) +
  
  # 3. Add horizontal threshold lines
  
  # geom_hline(yintercept = 42.8, color = "#4BACC6", linewidth = 1) + # 0.25 meals/month
  geom_hline(yintercept = 21.4, color = "#e40b0b", linewidth = 1) + # 0.5 meal/month
  geom_hline(yintercept = 10.7, color = "#fd9e02", linewidth = 1) + # 1 meal per month
  geom_hline(yintercept = 5.3, color = "#ffb703", linewidth = 1) + # 2 meal per month
  geom_hline(yintercept = 3.6, color = "#126782", linewidth = 1) + # 3 meal per month
  geom_hline(yintercept = 2.7, color = "#219ebc", linewidth = 1) + # 4 meal per month
  geom_hline(yintercept = 1.3, color = "#8ecae6", linewidth = 1) + # 8 meal per month
  
  
  # 1. Plot individual concentration points (blue circles)
  geom_point(aes(y = Result), color = "#808080", size = 3, alpha = 0.7) +
  
  # 2. Plot the average results (green diamonds)
  geom_point(aes(y = Average_Result), color = "#2a2a2a", shape = 18, size = 5) +
  
  
  
  
  # 4. Add threshold text labels on the left side
  # annotate("text", x = 0.5, y = 1.20, label = "DO NOT EAT",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 30, label = "0.25 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 15, label = "0.5 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 8, label = "1 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 4.6, label = "2 meals per month",  hjust = 0, size = 3.5) +
  # annotate("text", x = 0.5, y = 3, label = "3 meals per month",  hjust = 0, size = 3.5) +
  # annotate("text", x = 0.5, y = 2, label = "4 meals per month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = .08, label = ">= 8 meals per month",  hjust = 0, size = 3.5) +
  
  
  # 5. Formatting axes and limits
  scale_y_continuous(limits = c(0, 53.5), breaks = seq(0, 53.5, by = 10)) +
  labs(x = NULL, y = NULL) + # Removes default axis titles to match your clean look
  
  # 6. Styling the theme to match a clean grid
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "#E0E0E0"),
    panel.grid.major.y = element_line(color = "#E0E0E0"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, color = "black", vjust = 0.5),
    axis.text.y = element_text(size = 11, color = "black")
  )



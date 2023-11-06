# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Read csvs
data <- read_csv("actual-cfd-generation-and-avoided-ghg-emissions.csv") # low carbon contract comapny 
cpi <- read.csv("series-181023.csv") # ONS
cpi_filtered <- cpi[-c(1:473),]

# Filter data for the specific CfD ID
solar83 <- data[data$CfD_ID == "AAA-LIG-176", ]
solar83 <- solar83 %>%
  filter(!(Settlement_Date == ymd("2016-06-30")) & # Exclude June 30th, 2016
           !(month(Settlement_Date) == 10 & year(Settlement_Date) == 2023)) # Exclude October 2023

# Ensure CPI values are numeric
cpi_filtered$CPI.INDEX.00..ALL.ITEMS.2015.100 <- as.numeric(cpi_filtered$CPI.INDEX.00..ALL.ITEMS.2015.100)

# Create a lookup vector for CPI values with the Title as names
cpi_lookup <- setNames(cpi_filtered$CPI.INDEX.00..ALL.ITEMS.2015.100, cpi_filtered$Title)

# Format the Settlement_Date column to match the CPI Title format
solar83$Month_Year <- format(solar83$Settlement_Date, "%Y %b")
solar83$Month_Year <- toupper(solar83$Month_Year)

# Check for any Month_Year in solar83 that doesn't have a match in cpi_lookup
non_matching <- setdiff(solar83$Month_Year, names(cpi_lookup))

# Print non-matching Month_Year values if there are any
if(length(non_matching) > 0) {
  print("Non-matching Month_Year values:")
  print(non_matching)
}

# Match CPI values to the solar83 dataset using Month_Year
solar83$CPI_for_date <- cpi_lookup[solar83$Month_Year]

# Calculate the adjustment factor using CPI for 2012
base_cpi <- mean(as.numeric(cpi[474:485,2]))
solar83$adjustment_factor <- ifelse(is.na(solar83$CPI_for_date), NA, base_cpi / solar83$CPI_for_date)

# Apply the adjustment factor to the Strike Price and Reference Price
solar83$Strike_Price_GBP_Per_MWh <- ifelse(is.na(solar83$adjustment_factor), NA, solar83$Strike_Price_GBP_Per_MWh * solar83$adjustment_factor)
solar83$Market_Reference_Price_GBP_Per_MWh <- ifelse(is.na(solar83$adjustment_factor), NA, solar83$Market_Reference_Price_GBP_Per_MWh * solar83$adjustment_factor)

# Create the new tibble solar83a with the adjusted payments
solar83a <- solar83

# Assuming "Settlement_Date" is in a datetime format ("<dttm>")
# Extract the date portion of the column
solar83$DateOnly <- as.Date(solar83$Settlement_Date)

# Divide through by 2012 strike price
names <- c("Strike_Price_GBP_Per_MWh")
solar2a <- solar83a
for(name in names){
  solar2a[[name]] <- solar83a[[name]] / 79.23
}

# solar with a strike price of 65
solar65a <- solar2a
for(name in names){
  solar65a[[name]] <- solar2a[[name]] * 65
}

# Calculate the difference between the strike price (65 in this case) and the market reference price
# Then multiply by the power generated to get the new payment amounts
solar65a$CFD_Payments_GBP <- (solar65a$Strike_Price_GBP_Per_MWh - solar65a$Market_Reference_Price_GBP_Per_MWh) * solar65a$CFD_Generation_MWh

# Define the treasury discount rate
total <- sum(solar65a$CFD_Payments_GBP)

# Define the cutoff date for the pre-pandemic period
pre_pandemic_cutoff <- as.Date("2020-01-01")

# Filter the dataset for the pre-pandemic period
solar_pre_pandemic <- solar65a %>%
  filter(Settlement_Date < pre_pandemic_cutoff)
pre_pandemic <- sum(solar_pre_pandemic$CFD_Payments_GBP)

#Payments from the low-carbon contract company
results <- data.frame("total" = total, "pre pandemic" = pre_pandemic)
results

# plot graph
plot_solar65a <- ggplot(solar65a, aes(x = Settlement_Date, y = CFD_Payments_GBP)) +
  geom_line(color = "#E3120B", size = 1.2) + # Bold red line for payments
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) + # Dotted black zero line
  labs(title = "£65 strike price (AR1 Solar reference)",
       x = "Date",
       y = "Adjusted Payments (GBP)") +
  theme_minimal(base_size = 14) + # Clean minimalistic theme with larger base font size
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16), # Smaller bold title
        axis.title.y = element_text(face = "bold", size = 14), # Bold Y axis title
        axis.text = element_text(color = "black"), # Black axis text for clarity
        axis.line = element_line(color = "black"), # Black axis lines
        panel.grid.major = element_line(color = "grey80"), # Lighter grid lines
        panel.grid.minor = element_blank(), # No minor grid lines
        panel.background = element_rect(fill = "white", color = NA), # White background, no border
        plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt")) + # Adjust plot margins
  annotate("text", x = max(solar65a$Settlement_Date, na.rm = TRUE), y = min(solar65a$Adjusted_CFD_Payments_GBP, na.rm = TRUE), 
           label = "Source: LCCC / Adjusted to 2012 CPI", 
           hjust = 1, vjust = -1, color = "grey50", size = 3.5) # Source annotation at the bottom right
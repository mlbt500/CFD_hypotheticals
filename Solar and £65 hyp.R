# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)

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

# Apply the adjustment factor to the CFD_Payments_GBP column
solar83$Adjusted_CFD_Payments_GBP <- ifelse(is.na(solar83$adjustment_factor), NA, solar83$CFD_Payments_GBP * solar83$adjustment_factor)

# Create the new tibble solar83a with the adjusted payments
solar83a <- solar83

# Define the pandemic start date
pandemic_start_date <- as.Date("2020-03-01")

# Assuming "Settlement_Date" is in a datetime format ("<dttm>")
# Extract the date portion of the column
solar83$DateOnly <- as.Date(solar83$Settlement_Date)

# Filter the data frame based on the date condition
# Make sure to replace 'DateOnly' with the actual date column name in 'solar83a'
pre_pandemic_data <- filter(solar83a, Settlement_Date < pandemic_start_date)

# Divide through by 2012 strike price
names <- c("Adjusted_CFD_Payments_GBP", "Strike_Price_GBP_Per_MWh", "Market_Reference_Price_GBP_Per_MWh")
solar2a <- solar83a
for(name in names){
  solar2a[[name]] <- solar83a[[name]] / 79.23
}

# solar with a strike price of 65
solar65a <- solar2a
for(name in names){
  solar65a[[name]] <- solar2a[[name]] * 65
}

# solar with a strike price of 75
solar75a <- solar2a
for(name in names){
  solar75a[[name]] <- solar2a[[name]] * 75
}

# Define the treasury discount rate
treasury_rate <- 0.035

# Define the last payment date
last_payment_date <- as.Date("2023-09-30")

# Calculate the NPV
npv <- solar65a %>%
  mutate(
    # Calculate the number of days from the Settlement_Date to the last payment date
    Days = as.numeric(difftime(last_payment_date, Settlement_Date, units = "days")),
    # Convert days to years for the discount rate calculation
    Years = Days / 365.25,
    # Discount all payments up to the date of the last payment
    # Payments on and after the last payment date are not discounted
    Discounted_Payment = if_else(Settlement_Date >= last_payment_date, 
                                 Adjusted_CFD_Payments_GBP, 
                                 Adjusted_CFD_Payments_GBP / ((1 + treasury_rate) ^ Years))
  ) %>%
  # Sum up all the discounted payments to get the NPV
  summarise(NPV = sum(Discounted_Payment, na.rm = TRUE)) # Use na.rm = TRUE to remove NAs

# Output the NPV
print(npv)

# Create the plot with John Burn-Murdoch styling for solar65a
plot_solar65a <- ggplot(solar65a, aes(x = Settlement_Date, y = Adjusted_CFD_Payments_GBP)) +
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

# Display the plot
print(plot_solar65a)
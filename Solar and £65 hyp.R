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
base_cpi <- mean(as.numeric(cpi[475:485,2]))
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

# Assuming 'pandemic_start_date' and 'solar83a' are already defined

# Filter the data frame based on the date condition
# Make sure to replace 'DateOnly' with the actual date column name in 'solar83a'
pre_pandemic_data <- filter(solar83a, Settlement_Date < pandemic_start_date)

# Divide through by 2012 strike price
names <- c("CFD_Payments_GBP", "Strike_Price_GBP_Per_MWh", "Market_Reference_Price_GBP_Per_MWh")
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

# Define the discount rate
annual_discount_rate <- 0.035

# Define the base date for discounting (July 2016)
base_date <- as.Date("2016-07-01")

# Function to calculate the present value of a payment
calculate_pv <- function(payment, date, base_date, rate) {
  # Calculate the time in years between the payment date and the base date
  time_in_years <- as.numeric(difftime(date, base_date, units = "days")) / 365.25
  
  # Calculate the present value
  pv <- payment / ((1 + rate) ** time_in_years)
  
  return(pv)
}

# Sum of discounted payments for each data frame
sum_pv_solar65a <- sum(sapply(1:nrow(solar65a), function(i) calculate_pv(solar65a$CFD_Payments_GBP[i], solar65a$Settlement_Date[i], base_date, annual_discount_rate)))
sum_pv_solar75a <- sum(sapply(1:nrow(solar75a), function(i) calculate_pv(solar75a$CFD_Payments_GBP[i], solar75a$Settlement_Date[i], base_date, annual_discount_rate)))
sum_pv_solar83a <- sum(sapply(1:nrow(solar83a), function(i) calculate_pv(solar83a$CFD_Payments_GBP[i], solar83a$Settlement_Date[i], base_date, annual_discount_rate)))

# Output the results
sum_pv_solar65a
sum_pv_solar75a
sum_pv_solar83a

# Function to calculate the total discounted payment for a given period
calculate_discounted_total <- function(data_frame, start_date, end_date, discount_rate, base_date) {
  # Filter the data for the given period
  period_data <- subset(data_frame, Settlement_Date >= start_date & Settlement_Date < end_date)
  
  # Calculate the present value of each payment
  pv_payments <- sapply(1:nrow(period_data), function(i) {
    calculate_pv(period_data$CFD_Payments_GBP[i], period_data$Settlement_Date[i], base_date, discount_rate)
  })
  
  # Sum the present values
  total_discounted_payment <- sum(pv_payments, na.rm = TRUE)
  
  return(total_discounted_payment)
}

# Calculate the total discounted payments before and after the pandemic for each strike rate
total_before_solar65a <- calculate_discounted_total(solar65a, base_date, pandemic_start_date, annual_discount_rate, base_date)
total_after_solar65a <- calculate_discounted_total(solar65a, pandemic_start_date, Sys.Date(), annual_discount_rate, base_date)

total_before_solar75a <- calculate_discounted_total(solar75a, base_date, pandemic_start_date, annual_discount_rate, base_date)
total_after_solar75a <- calculate_discounted_total(solar75a, pandemic_start_date, Sys.Date(), annual_discount_rate, base_date)

total_before_solar83a <- calculate_discounted_total(solar83a, base_date, pandemic_start_date, annual_discount_rate, base_date)
total_after_solar83a <- calculate_discounted_total(solar83a, pandemic_start_date, Sys.Date(), annual_discount_rate, base_date)

# Output the results
list(
  before_pandemic = list(
    solar65a = total_before_solar65a,
    solar75a = total_before_solar75a,
    solar83a = total_before_solar83a
  ),
  after_pandemic = list(
    solar65a = total_after_solar65a,
    solar75a = total_after_solar75a,
    solar83a = total_after_solar83a
  )
)
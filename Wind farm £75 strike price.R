
# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)

# Read csvs
data <- read_csv("actual-cfd-generation-and-avoided-ghg-emissions.csv") # low carbon contract company 
cpi <- read.csv("series-181023.csv") # ONS
cpi_filtered <- cpi[-c(1:527),] # Skipping the first 527 rows which might be headers or unrelated data

# Filter data for the specific CfD ID
triton_knoll <- data[data$CfD_ID == "AR2-TKN-103", ]
triton_knoll <- triton_knoll[-1,] # Assuming the first row is headers or unrelated

# Ensure CPI values are numeric
cpi_filtered$CPI.INDEX.00..ALL.ITEMS.2015.100 <- as.numeric(cpi_filtered$CPI.INDEX.00..ALL.ITEMS.2015.100)

# Create a lookup vector for CPI values with the Title as names
cpi_lookup <- setNames(cpi_filtered$CPI.INDEX.00..ALL.ITEMS.2015.100, cpi_filtered$Title)

# Format the Settlement_Date column to match the CPI Title format
triton_knoll$Month_Year <- format(triton_knoll$Settlement_Date, "%Y %b")
triton_knoll$Month_Year <- toupper(triton_knoll$Month_Year)

# Check for any Month_Year in triton_knoll that doesn't have a match in cpi_lookup
non_matching <- setdiff(triton_knoll$Month_Year, names(cpi_lookup))

# Print non-matching Month_Year values if there are any
if(length(non_matching) > 0) {
  print("Non-matching Month_Year values:")
  print(non_matching)
}

# Match CPI values to the triton_knoll dataset using Month_Year
triton_knoll$CPI_for_date <- cpi_lookup[triton_knoll$Month_Year]

# Calculate the adjustment factor using CPI for July 2016 as the base
base_cpi <- cpi_lookup["2016 JUL"]
triton_knoll$adjustment_factor <- ifelse(is.na(triton_knoll$CPI_for_date), NA, base_cpi / triton_knoll$CPI_for_date)

# Apply the adjustment factor to the CFD_Payments_GBP column
triton_knoll$Adjusted_CFD_Payments_GBP <- ifelse(is.na(triton_knoll$adjustment_factor), NA, triton_knoll$CFD_Payments_GBP * triton_knoll$adjustment_factor)

# Assuming the rest of the code is intended for further analysis and adjustments

# Define the pandemic start date
pandemic_start_date <- as.Date("2020-03-01")

# Assuming "Settlement_Date" is in a datetime format ("<dttm>")
# Extract the date portion of the column
triton_knoll$DateOnly <- as.Date(triton_knoll$Settlement_Date)

# Filter the data frame based on the date condition
pre_pandemic_data <- filter(triton_knoll, DateOnly < pandemic_start_date)

# Define the annual discount rate
annual_discount_rate <- 0.035

# Define the base date for discounting (first payment date)
base_date <- min(triton_knoll$Settlement_Date)

# Function to calculate the present value of a payment
calculate_pv <- function(payment, date, base_date, rate) {
  # Calculate the time in years between the payment date and the base date
  time_in_years <- as.numeric(difftime(date, base_date, units = "days")) / 365.25
  
  # Calculate the present value
  pv <- payment / ((1 + rate) ** time_in_years)
  
  return(pv)
}

# Apply the calculate_pv function to each payment to find its present value
triton_knoll$PV_CFD_Payments_GBP <- mapply(calculate_pv, triton_knoll$Adjusted_CFD_Payments_GBP, triton_knoll$Settlement_Date, MoreArgs = list(base_date = base_date, rate = annual_discount_rate))

# Sum of discounted payments
sum_pv_triton_knoll <- sum(triton_knoll$PV_CFD_Payments_GBP, na.rm = TRUE)

# Output the results
sum_pv_triton_knoll

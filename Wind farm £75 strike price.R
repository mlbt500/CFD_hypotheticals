
# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Read csvs
data <- read_csv("actual-cfd-generation-and-avoided-ghg-emissions.csv") # low carbon contract company 
cpi <- read.csv("series-181023.csv") # ONS
cpi_filtered <- cpi[-c(1:473),]

# Filter data for the specific CfD ID
triton_knoll <- data[data$CfD_ID == "AR2-TKN-103", ]
triton_knoll <- triton_knoll %>%
  filter(!(month(Settlement_Date) == 5 & year(Settlement_Date) == 2021),
         !(month(Settlement_Date) == 10 & year(Settlement_Date) == 2023)) # Exclude October 2023

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

# Calculate the adjustment factor using CPI for 2012
base_cpi <- mean(as.numeric(cpi[474:485,2]))
triton_knoll$adjustment_factor <- ifelse(is.na(triton_knoll$CPI_for_date), NA, base_cpi / triton_knoll$CPI_for_date)

# Apply the adjustment factor to the CFD_Payments_GBP column
triton_knoll$Adjusted_CFD_Payments_GBP <- ifelse(is.na(triton_knoll$adjustment_factor), NA, triton_knoll$CFD_Payments_GBP * triton_knoll$adjustment_factor)

# Assuming the rest of the code is intended for further analysis and adjustments

# Define the pandemic start date
pandemic_start_date <- as.Date("2020-03-01")

# Assuming "Settlement_Date" is in a datetime format ("<dttm>")
# Extract the date portion of the column
triton_knoll$DateOnly <- as.Date(triton_knoll$Settlement_Date)

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

# Ensure the DateOnly column is of Date class
triton_knoll$DateOnly <- as.Date(triton_knoll$DateOnly)

# Create the plot with John Burn-Murdoch styling and a smaller title
plot <- ggplot(triton_knoll, aes(x = DateOnly, y = Adjusted_CFD_Payments_GBP)) +
  geom_line(color = "#E3120B", size = 1.2) + # Bold red line for payments
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) + # Dotted black zero line
  labs(title = "Triton Knoll £75 Strike Price",
       x = "Date",
       y = "Adjusted Payments (GBP)") +
  theme_minimal(base_size = 14) + # Clean minimalistic theme with larger base font size
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 18), # Smaller bold title
        axis.title.y = element_text(face = "bold", size = 14), # Bold Y axis title
        axis.text = element_text(color = "black"), # Black axis text for clarity
        axis.line = element_line(color = "black"), # Black axis lines
        panel.grid.major = element_line(color = "grey80"), # Lighter grid lines
        panel.grid.minor = element_blank(), # No minor grid lines
        panel.background = element_rect(fill = "white", color = NA), # White background, no border
        plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt")) + # Adjust plot margins
  annotate("text", x = max(triton_knoll$DateOnly, na.rm = TRUE), y = min(triton_knoll$Adjusted_CFD_Payments_GBP, na.rm = TRUE), 
           label = "Source: LCCC / Adjusted to 2012 CPI", 
           hjust = 1, vjust = -1, color = "grey50", size = 3.5) # Source annotation at the bottom right
plot
# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)

# Fetch data from the URL
url <- "https://www.lowcarboncontracts.uk/data-portal/dataset/8a82caf8-3ec2-45a2-b1bd-581411d61cde/resource/ab7ea978-2f03-4ee4-99ef-f9cdf398b849/download/actual-cfd-generation-and-avoided-ghg-emissions.csv"
data <- read_csv(url)

# Filter data for the specific CfD ID
solar83 <- data[data$CfD_ID == "AAA-LIG-176", ]

# Calculate the hypothetical CFD Payments for £65 Strike Price
solar83$CFD_Payments_65 <- (65 - solar83$Market_Reference_Price_GBP_Per_MWh) * solar83$CFD_Generation_MWh

# Define the pandemic start date
pandemic_start_date <- as.Date("2020-03-01")

# Assuming "Settlement_Date" is in a datetime format ("<dttm>")
# Extract the date portion of the column
solar83$DateOnly <- as.Date(solar83$Settlement_Date)

# Filter the data frame based on the date condition
pre_pandemic_data <- filter(solar83, DateOnly < pandemic_start_date)

# Remove the temporary "DateOnly" column if not needed
solar83 <- select(solar83, -DateOnly)

# Compute the total savings/costs for both scenarios before the pandemic
pre_pandemic_data <- filter(solar83, Settlement_Date < pandemic_start_date)
actual_total_payment_pre_pandemic <- sum(pre_pandemic_data$CFD_Payments_GBP, na.rm = TRUE)
hypothetical_65_total_payment_pre_pandemic <- sum(pre_pandemic_data$CFD_Payments_65, na.rm = TRUE)

# Create the footnote text
actual_footnote <- paste("* Total over the contract's lifetime:", 
                         ifelse(actual_total_payment < 0, 
                                paste("\nSaved consumers", round(abs(actual_total_payment), 2), "GBP"), 
                                paste("\nCost consumers", round(actual_total_payment, 2), "GBP")),
                         "\n** From contract start to pre-pandemic:", 
                         ifelse(actual_total_payment_pre_pandemic < 0, 
                                paste("\nSaved consumers", round(abs(actual_total_payment_pre_pandemic), 2), "GBP"), 
                                paste("\nCost consumers", round(actual_total_payment_pre_pandemic, 2), "GBP")))

hypothetical_65_footnote <- paste("* Total over the contract's lifetime:", 
                                  ifelse(hypothetical_65_total_payment < 0, 
                                         paste("\nWould have saved consumers", round(abs(hypothetical_65_total_payment), 2), "GBP"), 
                                         paste("\nWould have cost consumers", round(hypothetical_65_total_payment, 2), "GBP")),
                                  "\n** From contract start to pre-pandemic:", 
                                  ifelse(hypothetical_65_total_payment_pre_pandemic < 0, 
                                         paste("\nWould have saved consumers", round(abs(hypothetical_65_total_payment_pre_pandemic), 2), "GBP"), 
                                         paste("\nWould have cost consumers", round(hypothetical_65_total_payment_pre_pandemic, 2), "GBP")))

# Set up the layout to have one row and two columns
par(mfrow = c(1, 2))

# Increase the bottom margin using mar parameter
par(mar = c(12, 4, 4, 2) + 0.1)


plot(solar83$Settlement_Date, solar83$CFD_Payments_GBP, type="l", 
     xlab="Date", ylab="CFD Payments (GBP)", main="CFD Payments for £83 Solar PV Over Time")
abline(h=0, col="red", lwd=1.5)
mtext(side=1, line=8, adj=0, actual_footnote)

# Hypothetical CFD Payments for £65
plot(solar83$Settlement_Date, solar83$CFD_Payments_65, type="l", 
     xlab="Date", ylab="CFD Payments (GBP)", main="Hypothetical CFD Payments for £65 Strike Price")
abline(h=0, col="red", lwd=1.5)
mtext(side=1, line=8, adj=0, hypothetical_65_footnote) 
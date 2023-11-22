library(ggplot2)
library(lubridate)

natural_gas <- read.csv("historical-prices-11-08-2023.csv")
natural_gas
# https://www.barchart.com/futures/quotes/NFF02/interactive-chart

# Assuming your data frame 'natural_gas' is already created and has the correct format
# Convert the 'Exp.Date' column to Date format assuming the dates are in 'dd/mm/yyyy' format
natural_gas$Exp.Date <- as.Date(natural_gas$Exp.Date, format = "%d/%m/%Y")

#cpi https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/d7bt/mm23
cpi <- read.csv("cpi.csv")
#remove rows that are not in month format
cpi_filtered <- cpi[-c(1:185),]
# Ensure CPI values are numeric
cpi_filtered$CPI.INDEX.00..ALL.ITEMS.2015.100 <- as.numeric(cpi_filtered$CPI.INDEX.00..ALL.ITEMS.2015.100)

# Create a lookup vector for CPI values with the Title as names
cpi_lookup <- setNames(cpi_filtered$CPI.INDEX.00..ALL.ITEMS.2015.100, cpi_filtered$Title)

# Format 'Exp.Date' to "Year Mon" format and convert to uppercase
natural_gas$Year_Mon <- toupper(format(natural_gas$Exp.Date, "%Y %b"))

# Check for any Month_Year that doesn't have a match in cpi_lookup
non_matching <- setdiff(natural_gas$Month_Year, names(cpi_lookup))

# Match CPI values to the triton_knoll dataset using Month_Year
natural_gas$CPI_for_date <- cpi_lookup[natural_gas$Year_Mon]

#Adjustment factor
natural_gas <- natural_gas[1:321,]
natural_gas$adjustment_factor <- ifelse(is.na(natural_gas$CPI_for_date), NA, natural_gas[321,12] / natural_gas$CPI_for_date)
natural_gas$Last_inflation_adjusted <- natural_gas$Last * natural_gas$adjustment_factor

# Create the plot with matched styling
plot <- ggplot(natural_gas, aes(x = Exp.Date, y = Last)) +
  geom_line(color = "#E3120B", size = 1.2) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) + 
  labs(title = "UK Natural Gas Future Prices Over Time",
       x = "Expiration Date",
       y = "Price (GBP/thm nominal)") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20), # Title with bold and size 16
        axis.title.x = element_text(face = "bold", size = 20), # X axis title with bold and size 12
        axis.title.y = element_text(face = "bold", size = 20), # Y axis title with bold and size 12
        axis.text.x = element_text(size = 10), # X axis text with size 10
        axis.text.y = element_text(size = 10), # Y axis text with size 10
        axis.line = element_line(color = "black"), # Black axis lines
        panel.grid.major = element_line(color = "grey80"), # Lighter grid lines
        panel.grid.minor = element_blank(), # No minor grid lines
        panel.background = element_rect(fill = "white", color = NA), # White background, no border
        plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt")) + # Adjust plot margins
  annotate("text", x = max(natural_gas$Exp.Date, na.rm = TRUE), 
           y = min(natural_gas$Last, na.rm = TRUE), 
           label = "Source: barchart.com", 
           hjust = 1, vjust = 1, color = "grey50", size = 3.5) # Source annotation at the bottom right, with size 3.5

# To display the plot in R, simply print 'plot' or use it in an RMarkdown chunk.
plot

plot <- ggplot(natural_gas, aes(x = Exp.Date, y = Last_inflation_adjusted)) +
  geom_line(color = "#E3120B", size = 1.2) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) + 
  labs(title = "UK Natural Gas Future Prices Over Time",
       x = "Expiration Date",
       y = "Price (GBP/thm adjusted by CPI)") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20), # Title with bold and size 16
        axis.title.x = element_text(face = "bold", size = 20), # X axis title with bold and size 12
        axis.title.y = element_text(face = "bold", size = 20), # Y axis title with bold and size 12
        axis.text.x = element_text(size = 10), # X axis text with size 10
        axis.text.y = element_text(size = 10), # Y axis text with size 10
        axis.line = element_line(color = "black"), # Black axis lines
        panel.grid.major = element_line(color = "grey80"), # Lighter grid lines
        panel.grid.minor = element_blank(), # No minor grid lines
        panel.background = element_rect(fill = "white", color = NA), # White background, no border
        plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt")) + # Adjust plot margins
  annotate("text", x = max(natural_gas$Exp.Date, na.rm = TRUE), 
           y = min(natural_gas$Last_inflation_adjusted, na.rm = TRUE), 
           label = "Source: barchart.com", 
           hjust = 1, vjust = 1, color = "grey50", size = 3.5) # Source annotation at the bottom right, with size 3.5

# To display the plot in R, simply print 'plot' or use it in an RMarkdown chunk.
plot

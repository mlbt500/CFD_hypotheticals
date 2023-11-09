natural_gas <- read.csv("historical-prices-11-08-2023.csv")
natural_gas

library(ggplot2)

# Assuming your data frame 'natural_gas' is already created and has the correct format
# Convert the 'Exp.Date' column to Date format assuming the dates are in 'dd/mm/yyyy' format
natural_gas$Exp.Date <- as.Date(natural_gas$Exp.Date, format = "%d/%m/%Y")

# Create the plot with matched styling
plot <- ggplot(natural_gas, aes(x = Exp.Date, y = Last)) +
  geom_line(color = "#E3120B", size = 1.2) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) + 
  labs(title = "UK Natural Gas Future Prices Over Time",
       x = "Expiration Date",
       y = "Price (GBP/thm)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30), # Title with bold and size 16
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

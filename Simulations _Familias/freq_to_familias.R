# Load required library
library(readxl)

# Read the Excel file
data_origin <- read_excel("C:/Users/Miranda Córdova/Desktop/Miranda_dnaview/Validación de datos/kinship/Concentrado_intentos/Nist_freq/freq/1036-Revised-Allele-Freqs-PopStats-July-19-2017.xlsx",  sheet ='1036 revised all, n=1036')
# Define the path to the output text file 
output_file <- "C:/Users/Miranda Córdova/Desktop/NIST_freq_all.txt"

# Remove the third row from the data
data <- data_origin[-c(0:2), ]
data <-data[1:98,]
# Extract marker names from the third row
marker_names <- as.character(data[1, ])


# Open the text file for writing
fileConn <- file(output_file, "w")

# Loop through each combination of "Allele" column and subsequent columns
for (i in 2:(length(marker_names) - 1)) {
  # Extract columns "Allele" and the current column
  columns <- data[, c(1, i)]
  
  # Remove incomplete cases
  columns <- columns[complete.cases(columns), ]
  
  
  
  # Write the data to the text file
  write.table(columns, fileConn, row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE, sep = "\t")
}

# Close the text file connection
close(fileConn)


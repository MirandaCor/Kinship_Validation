# ============================================================
# Function: Process and clean profile data
# ============================================================
process_filtered_data <- function(data) {
  # Remove unnecessary columns
  data <- data[, !names(data) %in% c("FOLIO", "COMMENT")]
  
  # Split the 'AMEL/SEXO' column into two separate columns
  data <- data %>%
    mutate(`AMEL 1` = substr(`AMEL/SEXO`, 1, 1),   # First character 
           `AMEL 2` = substr(`AMEL/SEXO`, 2, 2))   # Second character 
  
  # Drop the original 'AMEL/SEXO' column
  data <- select(data, -`AMEL/SEXO`)
  
  # Replace underscores in column names with spaces
  colnames(data) <- gsub("_", " ", colnames(data))
  
  # Remove rows where any cell equals "0", "1", or a blank space
  data <- data %>%
    filter(!rowSums(sapply(., function(x) x %in% c("0", "1", " "))))
  
  return(data)
}


# ============================================================
# Load input data
# ============================================================
ident_pi <- read_excel(
  "C:/Users/Miranda Córdova/Desktop/Miranda_dnaview/Validación de datos/kinship/global-16markers_sin ceros/LR _BR_11600/otravez_filtrado_sindiscordantes/pi/filtrados_pi_children.xlsx"
) %>% select(FAM_Sample_info, VIC_Sample_info, Value)

ident_nopi <- read_excel(
  "C:/Users/Miranda Córdova/Desktop/Miranda_dnaview/Validación de datos/kinship/global-16markers_sin ceros/LR _BR_11600/otravez_filtrado_sindiscordantes/pi/filtrados_ultimated_nonrelated.xlsx"
) %>% select(FAM_Sample_info, VIC_Sample_info, Value)

# Load profiles data
fam_data <- read_excel(
  "C:/Users/Miranda Córdova/Desktop/Miranda_dnaview/Validación de datos/Datos/fam_05-21_sinceros_sindiscordane.xlsx"
)

# Output folder
output_folder <- "C:/Users/Miranda Córdova/Desktop/Miranda_dnaview/Validación de datos/kinship/global-16markers_sin ceros/LR _BR_11600/otravez_filtrado_sindiscordantes/porporciones/pi"

# Define proportions for true (pi) and false (nopi) cases
proportions <- list(
  c(1, 749), c(3, 747), c(7, 743), c(15, 735), 
  c(30, 720), c(60, 690), c(97, 653), c(175, 575), c(375, 375)
)

# Create output folder if it doesn’t exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# ============================================================
# Create Excel workbook for results
# ============================================================
wb <- createWorkbook()

# ============================================================
# Iterate over each proportion
# ============================================================
for (prop in proportions) {
  n_pi <- prop[1]
  n_nopi <- prop[2]
  
  # Sample data according to the proportions
  sample_pi <- ident_pi %>% sample_n(size = n_pi)
  sample_nopi <- ident_nopi %>% sample_n(size = n_nopi)
  
  # Combine sampled family and victim IDs
  combined_samples <- data.frame(
    stack = c(sample_pi$FAM_Sample_info, sample_pi$VIC_Sample_info,
              sample_nopi$FAM_Sample_info, sample_nopi$VIC_Sample_info)
  )
  
  # Filter profile data using the sampled IDs
  processed_data <- fam_data[fam_data$`SAMPLE INFO` %in% combined_samples$stack, ]
  
  # (Optional) Clean data with process_filtered_data()
  # processed_data <- process_filtered_data(processed_data)
  
  # Save processed data to a .txt file
  file_name <- paste0("real_ident_pi_proportion_", n_pi, "_", n_nopi, ".txt")
  write.table(
    processed_data, file.path(output_folder, file_name), 
    sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE
  )
  
  # Add sampled data to the Excel workbook in separate sheets
  sheet_name_pi <- paste0("pi_", n_pi)
  addWorksheet(wb, sheet_name_pi)
  writeData(wb, sheet_name_pi, sample_pi)
  
  sheet_name_nopi <- paste0("nopi_", n_nopi)
  addWorksheet(wb, sheet_name_nopi)
  writeData(wb, sheet_name_nopi, sample_nopi)
}

# ============================================================
# Save final workbook
# ============================================================
saveWorkbook(wb, file.path(output_folder, "results_proportions.xlsx"), overwrite = TRUE)

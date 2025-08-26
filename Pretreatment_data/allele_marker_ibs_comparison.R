# -------------------------------------------------------------------
# Load profile data (exclude unnecessary columns)
# -------------------------------------------------------------------
profile <- read.xlsx(
  "C:/Users/Miranda Córdova/Desktop/Miranda_dnaview/copia de material 1julio24/material trabajo/raw_profiles_clave_dnaview_reales/fam_05-21_sinceros_sindiscordane.xlsx"
)[, c(2, 4:33)]

# Remove marker DYS391 (not needed)
profile <- profile %>% select(-contains("DYS391"))

# Create a workbook to store results
wb_results <- createWorkbook()

# -------------------------------------------------------------------
# Function: Compare samples and count number of matching markers
# -------------------------------------------------------------------
compare_samples <- function(sample1, sample2, profile) {
  # Filter rows corresponding to the given samples
  row1 <- profile %>% filter(grepl(sample1, SAMPLE.INFO, fixed = TRUE))
  row2 <- profile %>% filter(grepl(sample2, SAMPLE.INFO, fixed = TRUE))
  
  match_count <- 0
  
  if (nrow(row1) > 0 && nrow(row2) > 0) {
    # Iterate through marker pairs (allele 1, allele 2)
    for (i in seq(2, ncol(profile) - 1, by = 2)) {
      if ((!is.na(row1[[1, i]]) && !is.na(row2[[1, i]]) && row1[[1, i]] == row2[[1, i]]) ||
          (!is.na(row1[[1, i]]) && !is.na(row2[[1, i + 1]]) && row1[[1, i]] == row2[[1, i + 1]]) ||
          (!is.na(row1[[1, i + 1]]) && !is.na(row2[[1, i]]) && row1[[1, i + 1]] == row2[[1, i]]) ||
          (!is.na(row1[[1, i + 1]]) && !is.na(row2[[1, i + 1]]) && row1[[1, i + 1]] == row2[[1, i + 1]])) {
        match_count <- match_count + 1
      }
    }
  }
  
  return(match_count)
}

# -------------------------------------------------------------------
# Function: Compare samples and count allele sharing (0, 1, or 2 alleles)
# -------------------------------------------------------------------
compare_samples_allele <- function(sample1, sample2, profile) {
  row1 <- profile %>% filter(grepl(sample1, SAMPLE.INFO, fixed = TRUE))
  row2 <- profile %>% filter(grepl(sample2, SAMPLE.INFO, fixed = TRUE))
  
  count_0 <- 0
  count_1 <- 0
  count_2 <- 0
  
  if (nrow(row1) > 0 && nrow(row2) > 0) {
    for (i in seq(2, ncol(profile) - 1, by = 2)) {
      shared_values <- 0
      
      # Check each allele combination
      if (!is.na(row1[[1, i]]) && !is.na(row2[[1, i]]) && row1[[1, i]] == row2[[1, i]]) shared_values <- shared_values + 1
      if (!is.na(row1[[1, i]]) && !is.na(row2[[1, i + 1]]) && row1[[1, i]] == row2[[1, i + 1]]) shared_values <- shared_values + 1
      if (!is.na(row1[[1, i + 1]]) && !is.na(row2[[1, i]]) && row1[[1, i + 1]] == row2[[1, i]]) shared_values <- shared_values + 1
      if (!is.na(row1[[1, i + 1]]) && !is.na(row2[[1, i + 1]]) && row1[[1, i + 1]] == row2[[1, i + 1]]) shared_values <- shared_values + 1
      
      shared_values <- min(shared_values, 2)  # max two alleles per locus
      
      if (shared_values == 0) count_0 <- count_0 + 1
      else if (shared_values == 1) count_1 <- count_1 + 1
      else if (shared_values == 2) count_2 <- count_2 + 1
    }
  }
  
  return(list(count_0 = count_0, count_1 = count_1, count_2 = count_2))
}

# -------------------------------------------------------------------
# Input / Output settings
# -------------------------------------------------------------------
check_file <- "C:/Users/Miranda Córdova/Desktop/Miranda_dnaview/Pretratamiento_id/A10_1_resultados_conteo_pi.xlsx"
file_classification <- read_xlsx("C:/Users/Miranda Córdova/Desktop/Miranda_dnaview/Validación de datos/kinship/global-16markers_sin ceros/LR_BR_11600/otravez_filtrado_sindiscordantes/Otra vez miranda/pi-clasificacion.xlsx")

# Define sheets to process
sheets_to_process <- 'label_otrosID'
# Example alternative:
# sheets_to_process <- c("pi_children", "pi_samefolio", "uncomplete_cases", "cleaned_df_2_related", "ultimated_nonrelated")

# -------------------------------------------------------------------
# Iterate over each sheet and process comparisons
# -------------------------------------------------------------------
for (sheet in sheets_to_process) {
  # Read current sheet
  data <- read.xlsx(check_file, sheet = sheet)
  
  # Initialize results dataframe
  results <- data.frame(
    FAM_Sample_info = character(),
    VIC_Sample_info = character(),
    Shared_Markers = integer(),
    Shared_Allele = integer(),
    k0 = integer(),
    k1 = integer(),
    k2 = integer(),
    Value = numeric(),
    True_positive = logical(),
    stringsAsFactors = FALSE
  )
  
  # Iterate over each row in the dataset
  for (i in 1:nrow(data)) {
    fam_sample <- data$FAM_Sample_info[i]
    vic_sample <- data$VIC_Sample_info[i]
    value <- data$Value[i]
    
    # Count shared markers and alleles
    shared_markers <- compare_samples(fam_sample, vic_sample, profile)
    shared_alleles <- compare_samples_allele(fam_sample, vic_sample, profile)
    
    k0 <- shared_alleles$count_0
    k1 <- shared_alleles$count_1
    k2 <- shared_alleles$count_2
    shared_allele_total <- k1 + (2 * k2)
    
    # Define "true positive" based on sample code prefixes
    fam_prefix <- substring(fam_sample, 1, 8)
    vic_prefix <- substring(vic_sample, 1, 8)
    
    true_positive <- fam_prefix == vic_prefix && (
      (substring(fam_sample, 10, 10) %in% c("S","T","D","E") && substring(vic_sample, 10, 10) %in% c("Q","P","J")) ||
      (substring(fam_sample, 10, 10) %in% c("Q","P","J") && substring(vic_sample, 10, 10) %in% c("S","T","D","E")) ||
      (substring(fam_sample, 10, 10) %in% c("F","M") && substring(vic_sample, 10, 10) %in% c("H","I","J")) ||
      (substring(fam_sample, 10, 10) %in% c("H","I","J") && substring(vic_sample, 10, 10) %in% c("F","M")) ||
      (substring(fam_sample, 10, 10) %in% c("U","A","B","C","J") && substring(vic_sample, 10, 10) %in% c("M","F")) ||
      (substring(fam_sample, 10, 10) %in% c("M","F") && substring(vic_sample, 10, 10) %in% c("U","A","B","C","J"))
    )
    
    # Append result
    results <- rbind(results, data.frame(
      FAM_Sample_info = fam_sample,
      VIC_Sample_info = vic_sample,
      Shared_Markers = shared_markers,
      Shared_Allele = shared_allele_total,
      k0 = k0, k1 = k1, k2 = k2,
      Value = value,
      True_positive = true_positive,
      stringsAsFactors = FALSE
    ))
  }
  
  # Write results into workbook (one sheet per input sheet)
  addWorksheet(wb_results, sheetName = sheet)
  writeData(wb_results, sheet = sheet, x = results)
}

# -------------------------------------------------------------------
# Save workbook with results
# -------------------------------------------------------------------
saveWorkbook(wb_results, paste0(check_file, "/A14_sharemarker_pi.xlsx"), overwrite = TRUE)

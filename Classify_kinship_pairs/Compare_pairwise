#libraries
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(openxlsx)
library(purrr) 

# I have three files:
#   
# -The Dnaview output
# -The file containing non-genetic data
# -The file with profile data

# When inconsistencies were detected between the last two, we decided to avoid 
#assumptions or standard data cleaning methods due to the sensitivity of the 
#information. Instead, we chose to generate a data frame that allows us to 
#compare potential discrepancies between folios and relationships using simple 
#code.
# 
# This approach ensures that we can systematically identify inconsistencies 
#while preserving the integrity of the data. Ultimately, it helps us compile 
#the necessary information to compare records between relatives.
# 

#Read data
data <- read_excel("C:/Users/Miranda Córdova/Desktop/Miranda_dnaview/Validación de datos/ejercicio solo 500 en dnaview/filtered_pi.xlsx")
# colnames(data)
# [1] "FAM"           "VIC_before_vs" "Pedigree" (letter/ROL)     "Kinship"       "Value" (LR)       
# [6] "Folio"         "VIC_folio"    

# No genetic data, in my case have primary data of identity
metadata <- read_excel("C:/Users/Miranda Córdova/Desktop/Miranda_dnaview/Validación de datos/Datos/combined_data_mod.xlsx")

# opcional :Find the pattern 'de la' followed by a word and remove the space
#Treatment of compound surnames for comparison
# metadata <- metadata %>%
#   mutate(DESAPARECIDO_MOD = gsub("DEL(\\s+)([a-zA-Z]+)", "DEL\\2", DESAPARECIDO),
#          DESAPARECIDO_MOD = gsub("DE(\\s+)([a-zA-Z]+)", "DE\\2", DESAPARECIDO_MOD),
#          DESAPARECIDO_MOD = gsub("LAS(\\s+)([a-zA-Z]+)", "LAS\\2", DESAPARECIDO_MOD),
#          DESAPARECIDO_MOD = gsub("DELA(\\s+)([a-zA-Z]+)", "DELA\\2", DESAPARECIDO_MOD),
#          DESAPARECIDO_MOD = gsub("DELOS(\\s+)([a-zA-Z]+)", "DELOS\\2", DESAPARECIDO_MOD),
#          FAMILIAR_MOD = gsub("DEL(\\s+)([a-zA-Z]+)", "DEL\\2", `FAMILIAR 1`),
#          FAMILIAR_MOD = gsub("DE(\\s+)([a-zA-Z]+)", "DE\\2", FAMILIAR_MOD),
#          FAMILIAR_MOD = gsub("LAS(\\s+)([a-zA-Z]+)", "LAS\\2", FAMILIAR_MOD),
#          FAMILIAR_MOD = gsub("DELA(\\s+)([a-zA-Z]+)", "DELA\\2", FAMILIAR_MOD),
#          FAMILIAR_MOD = gsub("DELOS(\\s+)([a-zA-Z]+)", "DELOS\\2", FAMILIAR_MOD))


# > colnames(metadata)
# [1] "ESTATUS"(stage of searching)                       "FOLIO"(file number)                       
# [3] "DESAPARECIDO"(name of missing person)              "SEXO" (gender)                       
# [5] "EDAD"(age)                                         "FECHA"(missing day)                       
# [7] "MUNICIPIO" (localities)                            "ESTADO"                     
# [9] "DESCRIPCION DEL DESAPARECIDO"(Distinctive features)"FAMILIAR 1"(name of donor)                       
# [11] "COMMENT" (inicials)                               "PARENTESCO"  (kinship)                   
# [13] "Sample_folio" (folio different format)            "DESAPARECIDO_MOD" (without compound surnames-missing persons)               
# [15] "FAMILIAR_MOD" (without compound surnames-relatives)                
#  

# Folio in profiles and initials of donors
profile <- read_excel("C:/Users/Miranda Córdova/Desktop/Miranda_dnaview/Validación de datos/Datos/fam_05-21_sinceros_sindiscordane.xlsx")

# > colnames(fam_data)
# [1] "FOLIO" (file number)      "SAMPLE INFO"(unique code)  "COMMENT"(inicials donors) 

# Functions 
#########
#Change the column names to better understand the comparison and pretreatment data 
process_dataframe <- function(df) {
  # Rename columns

  colnames(df)[colnames(df) == "VIC_before_vs"] <- "VIC" 
  colnames(df)[colnames(df) == "Pedigree"] <- "FAM_Pedigree"
  colnames(df)[colnames(df) == "Folio"] <- "FAM_Folio"
  colnames(df)[colnames(df) == "VIC_folio"] <- "VIC_Sample_info"
  
  # Create columns. one with a folio and another with the role or relationship of each comparisons
  df$FAM_Sample_info <- paste(df$FAM_Folio, df$FAM_Pedigree, sep = " ")# Concatenating Columns into FAM_Sample_info
  df$VIC_Pedigree <- substr(df$VIC_Sample_info, 10, 10) #Extracting rol Character for VIC_Pedigree
  df$VIC_Folio <- substr(gsub("[^0-9]", "", df$VIC_Sample_info), 1, 8) #Extracting Numeric Characters for VIC_Folio
  
  # rearrange columns
  df <- df[, c('FAM', 'VIC', 'Kinship', 'FAM_Folio', 'VIC_Folio', 
               'FAM_Pedigree', 'VIC_Pedigree', 'Value', 
               'FAM_Sample_info', 'VIC_Sample_info')]
  
  # Filter rows to exclude victim assignment values in dnaview
  df_ready <- subset(df, !((FAM_Pedigree %in% c('0', 'V')) |
                                grepl("[0V]$", VIC_Sample_info)))
  
  
  return(df_ready)
}

#Match folio and comments with pairwise with file
process_comments <- function(data_subset, fam_data) {
  # Initialize COMMENT_Vic and COMMENT_Fam columns with NA
  data_subset$VIC_Comment <- NA
  data_subset$FAM_Comment <- NA
  data_subset$VIC_profile_Folio <- NA
  data_subset$FAM_profile_Folio <- NA
  # Iterate over each row in data_subset
  for (i in 1:nrow(data_subset)) {
    # Get the VIC_folio value for the current row
    VIC_si <- data_subset$VIC_Sample_info[i]
    FAM_si <- data_subset$FAM_Sample_info[i]
    
    # Find the corresponding rows in fam_data where 'SAMPLE INFO' matches VIC_folio_value or COMMENT_Fam
    matching_row_1 <- fam_data[grep(VIC_si, fam_data$`SAMPLE INFO`), ]
    matching_row_2 <- fam_data[grep(FAM_si, fam_data$`SAMPLE INFO`), ]
    
    # Assign COMMENT values if matches are found
    if (nrow(matching_row_1) > 0) {
      data_subset$VIC_Comment[i] <- matching_row_1$COMMENT[1]
      data_subset$VIC_profile_Folio[i] <- matching_row_1$FOLIO[1]
      
    }else{
      data_subset$VIC_Comment[i] <- NA
      data_subset$VIC_profile_Folio[i] <- NA
    }
    if (nrow(matching_row_2) > 0) {
      data_subset$FAM_Comment[i] <- matching_row_2$COMMENT[1]
      data_subset$FAM_profile_Folio[i] <- matching_row_2$FOLIO[1]
      
    }else{
      data_subset$FAM_Comment[i] <- NA
      data_subset$FAM_profile_Folio[i] <- NA
    }
  }
  
  return(data_subset)
}

# Match no genetic data with pairwise
process_kinship <- function(data_subset, metadata) {
  #create columns 
  data_subset$kinship_Fam <- NA
  data_subset$donant_Fam <- NA
  data_subset$desap_Fam <- NA
  data_subset$STATUS_Fam <- NA
  data_subset$kinship_Vic <- NA
  data_subset$donant_Vic <- NA
  data_subset$desap_Vic <- NA
  data_subset$STATUS_Vic <- NA
  
  # Iterate over each row in data_subset
  for (i in 1:nrow(data_subset)) {
    a <- data_subset$VIC_profile_Folio[i]
    b <- data_subset$VIC_Comment[i]
    c <- data_subset$FAM_profile_Folio[i]
    d <- data_subset$FAM_Comment[i]
    
    # Find the corresponding row indices in metadata
    matching_row_1 <- which(metadata$FOLIO == a & metadata$COMMENT == b)
    matching_row_2 <- which(metadata$FOLIO == c & metadata$COMMENT == d)
    # This option is added because not all the relatives are registered,
    #however the belong to the same folio
    matching_row_3 <- which(metadata$FOLIO == a)
    matching_row_4 <- which(metadata$FOLIO == c)
    
    # Assign COMMENT values if matches are found
    if (length(matching_row_1) > 0) {
      data_subset$desap_Vic[i] <- metadata$DESAPARECIDO_MOD[matching_row_1]
      data_subset$STATUS_Vic[i] <- metadata$ESTATUS[matching_row_1]
      data_subset$donant_Vic[i] <- metadata$FAMILIAR_MOD[matching_row_1]
      data_subset$kinship_Vic[i] <- metadata$PARENTESCO[matching_row_1]
    }
    if (length(matching_row_2) > 0) {
      data_subset$desap_Fam[i] <- metadata$DESAPARECIDO_MOD[matching_row_2]
      data_subset$STATUS_Fam[i] <- metadata$ESTATUS[matching_row_2]
      data_subset$donant_Fam[i] <- metadata$FAMILIAR_MOD[matching_row_2]
      data_subset$kinship_Fam[i] <- metadata$PARENTESCO[matching_row_2]
    }
    if (length(matching_row_3) > 0) {
      data_subset$desap_Vic[i] <- metadata$DESAPARECIDO[matching_row_3]
      data_subset$STATUS_Vic[i] <- metadata$ESTATUS[matching_row_3]
      
    }
    if (length(matching_row_4) > 0) {
      
      data_subset$desap_Fam[i] <- metadata$DESAPARECIDO_MOD[matching_row_4]
      data_subset$STATUS_Fam[i] <- metadata$ESTATUS[matching_row_4]
    }
  }
  
  return(data_subset)
}

determine_relationship <- function(data) {
  data$clasiffication <- dplyr::case_when(
    # Condiciones para 'Grandparent'
    ((data$FAM_Pedigree %in% c("F", "M") & grepl("[DEST]$", data$VIC_Sample_info)) |
       (data$FAM_Pedigree %in% c("D", "E", "S", "T") & grepl("[FM]$", data$VIC_Sample_info))) ~ "Possible Grandparent",
    
    # Condiciones para 'Sibling'
    ((data$FAM_Pedigree %in% c("F", "M", "R", "Z") & grepl("[RZFM]$", data$VIC_Sample_info)) |
       (data$FAM_Pedigree %in% c("D", "E", "S", "T") & grepl("[DEST]$", data$VIC_Sample_info))) |      
      ((data$FAM_Folio == data$VIC_Folio) & 
         (data$FAM_Pedigree %in% c("D", "E", "S", "T", "J")) &
         (grepl("[DESTJ]$", data$VIC_Sample_info)))~ " Possible Sibling",
    
    # Condiciones para 'Uncle'
    ((data$FAM_Pedigree %in% c("A", "B", "C", "U") & grepl("[DEST]$", data$VIC_Sample_info)) |
       (data$FAM_Pedigree %in% c("D", "E", "S", "T") & grepl("[ABCU]$", data$VIC_Sample_info))) ~ "Possible Uncle-",
    
    # Condiciones para 'Parent'
    ((data$FAM_Pedigree %in% c("S", "T", "D", "E") & grepl("[QP]$", data$VIC_Sample_info)) |
       (data$FAM_Pedigree %in% c("Q", "P") & grepl("[STDE]$", data$VIC_Sample_info))) |
        ((data$FAM_Folio == data$VIC_Folio) &
         (data$FAM_Pedigree %in% c("R", "Z", "F", "M")) &
         (grepl("[O]$", data$VIC_Sample_info))) |
        ((data$FAM_Folio == data$VIC_Folio) &
         (data$FAM_Pedigree %in% c("O")) &
         (grepl("[RZFM]$", data$VIC_Sample_info))) |~ " Possible Parent ",
    
    # Condiciones para 'Half-sibling'
    ((data$FAM_Pedigree %in% c("H", "I") & grepl("[ABCU]$", data$VIC_Sample_info)) |
       (data$FAM_Pedigree %in% c("H", "I") & grepl("[HI]$", data$VIC_Sample_info)) |
       (data$FAM_Pedigree %in% c("A", "B", "C", "U") & grepl("[HI]$", data$VIC_Sample_info))) ~ " Possible Half-sibling",
    
    # Condiciones para true kinship (Sibling)
    ((data$FAM_Folio == data$VIC_Folio) & 
       (data$FAM_Pedigree %in% c("U", "A", "B", "C", "J")) &
       (grepl("[UABC]$", data$VIC_Sample_info)))  ~ "True-Sibling",
    
    # Condiciones para Parent-Child
    ((data$FAM_Folio == data$VIC_Folio) &
       (data$FAM_Pedigree %in% c("S", "T", "D", "E")) &
       (grepl("[QPJ]$", data$VIC_Sample_info))) |
      ((data$FAM_Folio == data$VIC_Folio) &
         (data$FAM_Pedigree %in% c("Q", "P", "J")) &
         (grepl("[TSDE]$", data$VIC_Sample_info))) |
      ((data$FAM_Folio == data$VIC_Folio) &
         (data$FAM_Pedigree %in% c("F", "M")) &
         (grepl("[KJ]$", data$VIC_Sample_info))) |
      ((data$FAM_Folio == data$VIC_Folio) &
         (data$FAM_Pedigree %in% c("K", "J")) &
         (grepl("[FM]$", data$VIC_Sample_info))) |      
      ((data$FAM_Folio == data$VIC_Folio) &
         (data$FAM_Pedigree %in% c("U", "A", "B", "C", "J")) &
         (grepl("[MF]$", data$VIC_Sample_info))) |
      ((data$FAM_Folio == data$VIC_Folio) &
         (data$FAM_Pedigree %in% c("M", "F")) &
         (grepl("[ACUBJ]$", data$VIC_Sample_info))) ~ "True-Parent-Child",
    
    # Default a 'OTHER'
    TRUE ~ "OTHER"
  )
  return(data)
}

classify_kinship_pairs <- function(data) {
  
  # Sort data in descending order by 'Value' column
  data <- data %>% arrange(desc(Value))
  
  # Create temporary columns by ordering the values
  data <- data %>%
    mutate(
      Temp_Sample = pmap_chr(list(FAM_Sample_info, VIC_Sample_info), ~ paste(sort(trimws(c(...))), collapse = "")),
      Temp_Comment = pmap_chr(list(VIC_Comment, FAM_Comment), ~ paste(sort(trimws(c(...))), collapse = ""))
    )
  
  # Filter duplicates keeping only the row 
  data1 <- data %>%
    group_by(Temp_Sample, Temp_Comment, Value)%>% # group unique combination
    slice(1) %>%   # take first row
    ungroup() %>%
    select(-Temp_Sample, -Temp_Comment)
  
  # Get ONLY the duplicate rows that were not selected in data1
  duplicated_rows <- data %>%
    group_by(Temp_Sample, Temp_Comment, Value) %>%
    filter(n() > 1) %>%  
    filter(!row_number() %in% 1) %>%  
    ungroup() %>%
    select(-Temp_Sample, -Temp_Comment)
  
  # Filter categories True-sibling and True-Parent-Child
  true_kin <- data1 %>% filter(clasiffication %in% c("True-sibling", "True-Parent-Child"))
  
  # Exclude these categories
  filtered_data <- data1 %>% filter(!clasiffication %in% c("True-sibling", "True-Parent-Child"))
  
  # Divide into two groups according folio
  same_folio <- filtered_data %>% filter(FAM_Folio == VIC_Folio) #related with missing person
  different_folio <- filtered_data %>% filter(FAM_Folio != VIC_Folio) # not related 
  
  # Function to extract surnames with NA handling and short names
  extract_lastnames <- function(name) {
    if (is.na(name) || name == "") {
      return(c("", ""))
    }
    words <- unlist(strsplit(name, " "))
    if (length(words) >= 2) {
      return(c(words[length(words) - 1], words[length(words)]))
    } else {
      return(c("", words[1]))
    }
  }
  
  # Apply surname extraction in cases with different folios
  different_folio <- different_folio %>% 
    mutate(
      desap_Fam_lastname_1 = sapply(desap_Fam, function(x) extract_lastnames(x)[1]),
      desap_Fam_lastname_2 = sapply(desap_Fam, function(x) extract_lastnames(x)[2]),
      desap_Vic_lastname_1 = sapply(desap_Vic, function(x) extract_lastnames(x)[1]),
      desap_Vic_lastname_2 = sapply(desap_Vic, function(x) extract_lastnames(x)[2]),
      donant_Fam_lastname_1 = sapply(donant_Fam, function(x) extract_lastnames(x)[1]),
      donant_Fam_lastname_2 = sapply(donant_Fam, function(x) extract_lastnames(x)[2]),
      donant_Vic_lastname_1 = sapply(donant_Vic, function(x) extract_lastnames(x)[1]),
      donant_Vic_lastname_2 = sapply(donant_Vic, function(x) extract_lastnames(x)[2])
    )
  
  # Classification according to surnames
  different_folio <- different_folio %>% mutate(
    clasiffication = case_when(
      desap_Fam == desap_Vic | donant_Fam == donant_Vic ~ "Duplicado",
      desap_Fam_lastname_1 == desap_Vic_lastname_1 & desap_Fam_lastname_2 == desap_Vic_lastname_2 ~ "related",
      desap_Fam_lastname_1 == desap_Vic_lastname_1 & desap_Fam_lastname_2 != desap_Vic_lastname_2 ~ "desap paterno",
      desap_Fam_lastname_1 != desap_Vic_lastname_1 & desap_Fam_lastname_2 == desap_Vic_lastname_2 ~ "desap materno",
      donant_Fam_lastname_1 == donant_Vic_lastname_1 & donant_Fam_lastname_2 != donant_Vic_lastname_2 ~ "donante paterno",
      donant_Fam_lastname_1 != donant_Vic_lastname_1 & donant_Fam_lastname_2 == donant_Vic_lastname_2 ~ "donante materno",
      TRUE ~ "not related"
    )
  )
  
  # Save to an Excel file with different sheets
  wb <- createWorkbook()
  
  addWorksheet(wb, "original_data")
  writeData(wb, "original_data", data)
  
  addWorksheet(wb, "True_Kinship")
  writeData(wb, "True_Kinship", true_kin)
  
  addWorksheet(wb, "Same_Folio")
  writeData(wb, "Same_Folio", same_folio)
  
  addWorksheet(wb, "Different_Folio")
  writeData(wb, "Different_Folio", different_folio)
  
  addWorksheet(wb, "Duplicated_Rows")
  writeData(wb, "Duplicated_Rows", duplicated_rows)
  
  saveWorkbook(wb, "Clasificacion.xlsx", overwrite = TRUE)
}
#########

# Apply the function in dataframes, I preferred the segmentation of the 
#process to detect errors.
data <- process_dataframe(data) #organize and clarify colnames
data_comment <- process_comments(data, profile) # match no genetic data from profiles file

data_kinship <- process_kinship(data_comment, metadata) # march no genetic data
data_classify <- determine_relationship(data_kinship)
classify_kinship_pairs(data_classify)

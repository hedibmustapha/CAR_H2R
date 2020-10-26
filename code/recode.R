library(dplyr)
library(readxl)
library(stringr)


data <- read_excel("data/data.xlsx")
data_short_names <- data

recode_df <- read_excel("./input/Questionnaire_Kobo__MSNA2020_ki_final2.xlsx", sheet = "choices")

# Recode to labels
for (i in 1: length(data_short_names) ) {
  for (j in 1: nrow(data_short_names)) {
    
      for (l in 1:nrow(recode_df)) {
        if(data_short_names[j,i] %in% recode_df$name[l]){
          data_short_names[j,i] <- recode_df$label[l]
        } 
      }
    
  }
  print(paste0("Column ",i , " checked/recoded!"))
}

write.csv(data_short_names,"data/recoded/recoded_data.csv", row.names = F)


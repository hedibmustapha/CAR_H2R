re_create_parent_question <- function(df, multiple_choices, separator){
  #Loop throughout received questions
  for (m in 1:length(multiple_choices)) {
    choice_char_count <- nchar(multiple_choices[m]) + 2
    choices <- df %>% select(starts_with(paste0(multiple_choices[m], separator)))
    #Loop throughout choices of each question
    for (c in 1:length(choices)) {
      choice_name <- substring(names(choices[c]),choice_char_count)
      #Loop throughout rows of each choice
      for (r in 1:nrow(choices)) {
        if (!is.na(choices[r,c]) & as.numeric(choices[[c]][r]) == 1 & choices[[c]][r] != "AC") {
          temp <- str_detect(df[[multiple_choices[m]]][r], choice_name)
          if(!is.na(temp) & !temp){
            #Insert the choice name to parent question
            df[r, multiple_choices[m]] <- paste(df[[multiple_choices[m]]][r], choice_name, sep = " ")
            df[r, multiple_choices[m]] <- gsub("AC ","",df[r, multiple_choices[m]])
          }
        }
      }
    }
    cat("\014")
    print (paste("Creating parent ", m, "of", length(multiple_choices)))
  }
  return(df)
}

multiple_choices <- c(
  "ig_3_groupop",
  "abri_1_type",
  "secal_7_choc",
  "protec_2_secu_ad_hommes",
  "protec_2_secu_ad_femmes",
  "protec_3_secu_kid_garcons",
  "protec_3_secu_kid_filles",
  "protec_5_travailforce_type",
  "protec_9_commu"
)


parent_created <- re_create_parent_question(final_data, multiple_choices, separator = ".")

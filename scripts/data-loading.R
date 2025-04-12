library(qualtRics)


load_data <- function(study) {
  if(study == "s1"){
    raw_data <- read_survey(
      "data/anonymized/s1/anonymized_data.csv", 
      col_types = readr::cols(Languages = readr::col_character(), AIChatbotsUsed = readr::col_character())
    )
  } else if(study == "s2"){
    raw_data <- read_survey(
      "data/anonymized/s2/anonymized_data.csv", 
      col_types = readr::cols(AIChatbotsUsed = readr::col_character())
    )
  }
  
  return(raw_data)
}


factorize_data <- function(study, raw_data){
  
  raw_data$Appelman_1 <- as.numeric(as.character(raw_data$Appelman_1))
  raw_data$Appelman_2 <- as.numeric(as.character(raw_data$Appelman_2))
  raw_data$Appelman_3 <- as.numeric(as.character(raw_data$Appelman_3))
  raw_data$Appelman_4 <- as.numeric(as.character(raw_data$Appelman_4))
    # factor(as.numeric(raw_data$Appelman_4), 
    #                              levels = c(-2, -1, 0, 1, 2), 
    #                              labels = c("Describes very poorly", "Describes poorly", "Neutral", "Describes well", "Describes very well")
    #                              )
  raw_data$Appelman_5 <- as.numeric(as.character(raw_data$Appelman_5))
  # raw_data$Boring <- factor(as.numeric(raw_data$Appelman_5), 
  #                           levels = c(-2, -1, 0, 1, 2), 
  #                           labels = c("Describes very poorly", "Describes poorly", "Neutral", "Describes well", "Describes very well")
  #                           )
  raw_data$Appelman_6 <- as.numeric(as.character(raw_data$Appelman_6))
  # raw_data$Engaging <- factor(as.numeric(raw_data$Appelman_6), 
  #                             levels = c(-2, -1, 0, 1, 2), 
  #                             labels = c("Describes very poorly", "Describes poorly", "Neutral", "Describes well", "Describes very well")
  #                             )
  
  
  raw_data$TrustBehaviour_1 <- as.numeric(as.character(raw_data$TrustBehaviour_1))
  raw_data$TrustBehaviour_2 <- as.numeric(as.character(raw_data$TrustBehaviour_2))
  raw_data$TrustBehaviour_3 <- as.numeric(as.character(raw_data$TrustBehaviour_3))
  raw_data$TrustBehaviour_4 <- as.numeric(as.character(raw_data$TrustBehaviour_4))
  raw_data$TrustBehaviour_5 <- as.numeric(as.character(raw_data$TrustBehaviour_5))
  raw_data$TrustBehaviour_6 <- as.numeric(as.character(raw_data$TrustBehaviour_6))
  raw_data$TrustBehaviour_7 <- as.numeric(as.character(raw_data$TrustBehaviour_7))
  raw_data$TrustBehaviour_8 <- as.numeric(as.character(raw_data$TrustBehaviour_8))
  raw_data$TrustBehaviour_9 <- as.numeric(as.character(raw_data$TrustBehaviour_9))
  
  
  raw_data$Experience_1 <- as.numeric(as.character(raw_data$Experience_1))
  raw_data$Experience_2 <- as.numeric(as.character(raw_data$Experience_2))
  raw_data$Experience_3 <- as.numeric(as.character(raw_data$Experience_3))
  # raw_data$Experience_4 <- factor(as.numeric(raw_data$Experience_4),
  #                                       levels = c(1, 2, 3, 4, 5), 
  #                                       labels = c("Strongly disagree", "Disagree", "Neither agree or disagree", "Agree", "Strongly agree"))
  raw_data$Experience_4 <- as.numeric(as.character(raw_data$Experience_4))
  raw_data$Experience_5 <- as.numeric(as.character(raw_data$Experience_5))
  raw_data$Experience_6 <- as.numeric(as.character(raw_data$Experience_6))
  # raw_data$Experience_7 <- factor(as.numeric(raw_data$Experience_7),
  #                                      levels = c(1, 2, 3, 4, 5), 
  #                                      labels = c("Strongly disagree", "Disagree", "Neither agree or disagree", "Agree", "Strongly agree"))
  raw_data$Experience_7 <- as.numeric(as.character(raw_data$Experience_7))
  raw_data$Experience_8 <- as.numeric(as.character(raw_data$Experience_8))
  raw_data$Experience_9 <- as.numeric(as.character(raw_data$Experience_9))
  raw_data$Experience_10 <- as.numeric(as.character(raw_data$Experience_10))
  raw_data$Experience_11 <- as.numeric(as.character(raw_data$Experience_11))
  
  raw_data$Sex <- factor(raw_data$Sex, 
                         levels = c(1,2,3,4), 
                         labels = c("Male", "Female", "Intersex", "Prefer not to say"))
  
  raw_data$Gender <- factor(raw_data$Gender, 
                            levels = c(1,2,3,4,5), 
                            labels = c("Non-binary / third gender", "Man", "Woman", "Prefer to self-describe", "Prefer not to say"))
  
  raw_data$Education <- factor(raw_data$Education, 
                               levels = c(1,2,3,4,5,6,7,8,9), 
                               labels = c("SomePrimary", "Primary", "SomeSecondarySchool", 
                                          "Secondary", "VocationalOrSimilar", "SomeUniversity", 
                                          "UniversityBachelorsDegree", "GraduateProfessionalDegree", 
                                          "PreferNotToSay"))
  
  raw_data$SurveyTopicCheck_coded <- factor(raw_data$SurveyTopicCheck_coded,
                                            levels = c(0,1,2,3,4,5,6,7),
                                            labels = c("No Answer", "AI", "Junk Data", "Non-AI Purpose", "Random", "Unsure", "Interesting", "None"))
  
  raw_data$Unrealistic <- factor(as.numeric(raw_data$Unrealistic), 
                                 levels = c(1, 2), 
                                 labels = c("No", "Yes"))
  
  raw_data$TechnicalIssues <- factor(as.numeric(raw_data$TechnicalIssues), 
                                     levels = c(1, 2), 
                                     labels = c("No", "Yes"))
  
  raw_data$AIChatbotsFrequency <- factor(raw_data$AIChatbotsFrequency,
                                         levels = c(1,2,3,4,5,6,7,8),
                                         labels = c("More than once a day",
                                                    "About once a day",
                                                    "About once a week", 
                                                    "About once every two weeks",
                                                    "Less than once a month",
                                                    "About once a month",
                                                    "I've only tried these a couple of times",
                                                    "I've never used these") )
  
  
  raw_data$ScienceContent <- factor(raw_data$ScienceContent,
                                    levels = c(1,2,3,4,5,6,7,8),
                                    labels = c("More than once a day",
                                               "About once a day",
                                               "About once a week", 
                                               "About once every two weeks",
                                               "Less than once a month",
                                               "About once a month",
                                               "I've only tried these a couple of times",
                                               "I've never used these") )
  
  if(study == "s1"){
    raw_data$Condition <- factor(raw_data$Condition, 
                                 levels = c("Low", "Medium", "High"))
    
    raw_data$English <- factor(raw_data$English, 
                               levels = c(1,2,3), 
                               labels = c("Yes", "No", "Learned English concurrently with another language"))
    
    raw_data$Unrealistic_coded <- factor(raw_data$Unrealistic_coded,
                                         levels = c(0,1,2,3,4,5,6),
                                         labels = c("No Answer", "AI/Not AI", "AI no personality", "Realistic", "Description Not Believable", "Junk Data", "Question why AI in High"))
    
  } else if(study == "s2"){
    raw_data$Condition <- factor(raw_data$Condition, 
                                 levels = c("Low", "High"))
    
    raw_data$Unrealistic_coded <- factor(raw_data$Unrealistic_coded,
                                         levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14),
                                         labels = c("No Answer", "AI didn't write", "AI no personality", "Realistic", "Description Not Believable", "Junk Data", "Question why AI in High", "Didn't follow instructions", "Sliders confusing", "The chat", "Didn't understand blog post", "other", "Distrust experiments", "N/A", "Quality/Content of blog post"))
    
  }
  return(raw_data)
  
}


explode_chatbots_used <- function(raw_data){
  # Explode comma-separated list fields
  
  # AIChatbotsUsed
  
  # Define AI Chatbot labels
  chatbot_labels <- c("1" = "ChatGPT", "2" = "Claude", "3" = "Gemini", 
                      "4" = "Copilot", "5" = "Grok", "7" = "Other", "8" = "None")
  
  raw_data <- raw_data %>%
    # Step 1: Split AIChatbotsUsed into separate rows
    separate_rows(AIChatbotsUsed, sep = ",") %>%
    # Step 2: Map chatbot codes to labels
    mutate(AIChatbotsUsed = chatbot_labels[AIChatbotsUsed]) %>%
    # Step 3: Create indicator variables for each language
    mutate(value = 1) %>%
    # Step 4: Pivot to wide format
    pivot_wider(names_from = AIChatbotsUsed, values_from = value, 
                names_prefix = "AIChatbotsUsed_", values_fill = 0)
}


explode_languages <- function(raw_data){
  # Languages
  
  # Define language labels
  language_labels <- c("1" = "English", "2" = "Mandarin", "3" = "Hindi", 
                       "4" = "Spanish", "5" = "French", "6" = "Arabic", 
                       "7" = "Bengali", "8" = "Portuguese", "9" = "Russian", 
                       "10" = "Urdu", "11" = "Other", "12" = "Prefer not to say")
  
  raw_data <- raw_data %>%
    # Step 1: Split Languages into separate rows
    separate_rows(Languages, sep = ",") %>%
    # Step 2: Map language codes to labels
    mutate(Languages = language_labels[Languages]) %>%
    # Step 3: Create indicator variables for each language
    mutate(value = 1) %>%
    # Step 4: Pivot to wide format
    pivot_wider(names_from = Languages, values_from = value, 
                names_prefix = "LanguagesFluent_", values_fill = 0)
}

exclude_non_consenting_participants <- function(raw_data){
  raw_data <- raw_data |>
    filter(ConsentForm == 1)  # Provided consent
  
  return(raw_data)
}

# One-off helpers ---------------------------------------------------------

# This was used to output the column information.
export_variables_as_markdown <- function(raw_data){
  dictionary <- labelled::generate_dictionary(raw_data)
  variables_md <- kable(dictionary, align = "c")
  save_kable(variables_md, "variables.md")
}

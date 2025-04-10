

# duplicate participants --------------------------------------------------

count_duplicate_participants <- function(data) {
  duplicates <- data |>
    group_by(id) |>
    summarise(n = n(), .groups = "keep") |>
    filter(n > 1) |>
    nrow()
  
  return(duplicates)
}

deduplicate_participants <- function(data) {
  # Filtering out responses from participants who completed the survey multiple times.
  # Only keeping the first complete one for each.
  
  #TODO: fix this to consider if they completed one after a partial attempt.
  deduped_data <- data |>
    group_by(id) |>  # Group by participant ID
    filter(RecordedDate == min(RecordedDate)) |>  #  keep only the first recorded entry for each group
    ungroup() |>  # Remove the grouping
    distinct(id, .keep_all = TRUE)  #  select only distinct rows (i.e. remove any duplicates)
  
  return(deduped_data)
}

# data filtering ----------------------------------------------------------

# clean_data <- function(data, study) {
#   cleaned_data <- data |>
#     filter(Progress == 100) |>  # Completed surveys
#     filter(ConsentForm == 1) |>  # Provided consent
#     filter(Status == 0) |> # Used anonymous link to complete survey (not preview)
#     filter(`Duration (in seconds)` >= (median(`Duration (in seconds)`)) / 3) |>
#     filter(TopicCheck == 1) |>  # Passed topic check (attention check)
#     filter(Q_RecaptchaScore >= 0.5) #|>  # Recaptcha score from Qualtrics == 1
#   # filter(Unrealistic_coded != 5 & SurveyTopicCheck_coded != 2) # Free text field does not appear to be random keystrokes (Unrealistic experience in survey, study purpose)
#   
#   if (study == "s1") {
#     cleaned_data <- cleaned_data |>
#       filter(EndDate >= "2024-12-04") |>  # Started on or after "2024-12-04" to remove test runs
#       filter(Reread != 1) |>  # Did not need to reread the instructions
#       filter(
#         (AgentTypeCheck == 1 & Condition == "Low") |
#           (AgentTypeCheck == 1 & Condition == "Medium") |
#           (AgentTypeCheck == 2 & Condition == "High")
#       ) |>  # Passed agent check (attention check)
#       filter(GodspeedMETI_28 == 100) |>  # Explicit item value check (attention check)
#       filter(GodspeedMETI_29 == 0) # Explicit item value check (attention check)
#   } else if (study == "s2") {
#     cleaned_data <- cleaned_data |>
#       filter(GodspeedMETI_28 == 20) |>  # Explicit item value check (attention check)
#       filter(GodspeedMETI_29 == 67) # Explicit item value check (attention check)
#   }
#   
#   return(cleaned_data)
# }

clean_data <- function(data, study) {
  initial_count <- nrow(data)
  cat("Initial number of participants:", initial_count, "\n")
  
  # Filter 1: Completed surveys (Progress == 100)
  step1 <- dplyr::filter(data, Progress == 100)
  cat("After Completed surveys filter (Progress == 100): Excluded", initial_count - nrow(step1), "participants\n")
  
  # Filter 2: Provided consent (ConsentForm == 1)
  step2 <- dplyr::filter(step1, ConsentForm == 1)
  cat("After Provided consent filter (ConsentForm == 1): Excluded", nrow(step1) - nrow(step2), "participants\n")
  
  # Filter 3: Used anonymous link (Status == 0)
  step3 <- dplyr::filter(step2, Status == 0)
  cat("After Anonymous link filter (Status == 0): Excluded", nrow(step2) - nrow(step3), "participants\n")
  
  # Filter 4: Duration filter
  step4 <- dplyr::filter(step3, `Duration (in seconds)` >= (median(`Duration (in seconds)`, na.rm = TRUE)) / 3)
  cat("After Duration filter (>= median/3): Excluded", nrow(step3) - nrow(step4), "participants\n")
  
  # Filter 5: TopicCheck (attention check)
  step5 <- dplyr::filter(step4, TopicCheck == 1)
  cat("After TopicCheck filter (TopicCheck == 1): Excluded", nrow(step4) - nrow(step5), "participants\n")
  
  # Filter 6: Recaptcha score filter
  step6 <- dplyr::filter(step5, Q_RecaptchaScore >= 0.5)
  cat("After Q_RecaptchaScore filter (>= 0.5): Excluded", nrow(step5) - nrow(step6), "participants\n")
  
  cleaned_data <- step6
  
  if (study == "s1") {
    # Filter 7: EndDate filter
    step7 <- dplyr::filter(cleaned_data, EndDate >= "2024-12-04")
    cat("After EndDate filter (>= '2024-12-04'): Excluded", nrow(cleaned_data) - nrow(step7), "participants\n")
    cleaned_data <- step7
    
    # # Filter 8: Reread filter
    # step8 <- dplyr::filter(cleaned_data, Reread != 1)
    # cat("After Reread filter (Reread != 1): Excluded", nrow(cleaned_data) - nrow(step8), "participants\n")
    # cleaned_data <- step8
    
    # Filter 9: AgentTypeCheck and Condition combination
    step9 <- dplyr::filter(cleaned_data, 
                           (AgentTypeCheck == 1 & Condition %in% c("Low", "Medium")) |
                             (AgentTypeCheck == 2 & Condition == "High"))
    cat("After AgentTypeCheck/Condition filter: Excluded", nrow(cleaned_data) - nrow(step9), "participants\n")
    cleaned_data <- step9
    
    # Filter 10: GodspeedMETI_28 filter
    step10 <- dplyr::filter(cleaned_data, GodspeedMETI_28 == 100)
    cat("After GodspeedMETI_28 filter (== 100): Excluded", nrow(cleaned_data) - nrow(step10), "participants\n")
    cleaned_data <- step10
    
    # Filter 11: GodspeedMETI_29 filter
    step11 <- dplyr::filter(cleaned_data, GodspeedMETI_29 == 0)
    cat("After GodspeedMETI_29 filter (== 0): Excluded", nrow(cleaned_data) - nrow(step11), "participants\n")
    cleaned_data <- step11
    
  } else if (study == "s2") {
    # Filter 7: GodspeedMETI_28 filter for s2
    step7 <- dplyr::filter(cleaned_data, GodspeedMETI_28 == 20)
    cat("After GodspeedMETI_28 filter (== 20): Excluded", nrow(cleaned_data) - nrow(step7), "participants\n")
    cleaned_data <- step7
    
    # Filter 8: GodspeedMETI_29 filter for s2
    step8 <- dplyr::filter(cleaned_data, GodspeedMETI_29 == 67)
    cat("After GodspeedMETI_29 filter (== 67): Excluded", nrow(cleaned_data) - nrow(step8), "participants\n")
    cleaned_data <- step8
  }
  
  cat("Final number of participants:", nrow(cleaned_data), "\n")
  return(cleaned_data)
}

# data imputation ---------------------------------------------------------


get_columns_excluded_from_imputation <- function() {
  return(
    c(
      "ConsentForm",
      "AgentNameCheck",
      "AgentTypeCheck",
      "TopicCheck",
      "Q_RecaptchaScore",
      "SurveyTopicCheck"
    )
  )
}

missing_values_within_conditions <- function(data) {
  # Count missing values per variable within each condition
  missing_values_table <- data %>%
    group_by(Condition) %>%
    summarise(across(everything(), ~ sum(is.na(.)), .names = "{.col}")) %>%
    pivot_longer(-Condition, names_to = "Variable", values_to = "Missing_Count")
  
  return(missing_values_table |> filter(Missing_Count > 0))
}


impute_data <- function(data) {
  exclude_vars <- get_columns_excluded_from_imputation()
  
  # Perform imputation only on selected variables
  imputed_data <- data %>%
    group_by(Condition) %>%
    mutate(
      # Fill numeric variables with the median, excluding certain variables
      across(
        where(is.numeric) & !any_of(exclude_vars) & !ends_with("_TEXT"),
        ~ replace_na(., median(., na.rm = TRUE))
      ),
      # Fill categorical variables with the mode, excluding certain variables
      across(
        where( ~ (is.character(.) |
                    is.factor(.))) & !any_of(exclude_vars) & !ends_with("_TEXT"),
        ~ replace_na(., names(sort(
          table(.), decreasing = TRUE
        ))[1])
      )
    ) %>%
    ungroup()
  
  # Print confirmation message
  print("Missing values have been imputed (excluding specified variables).")
  
  return(imputed_data)
  
}


# data coding -------------------------------------------------------------

recode_data <- function(data, study) {
  recoded_data <- data |>
    mutate(
      TrustBehaviour_4r = 7 - TrustBehaviour_4,
      TrustBehaviour_5r = 7 - TrustBehaviour_5,
      GodspeedMETI_10r = 100 - GodspeedMETI_10,
      GodspeedMETI_13r = 100 - GodspeedMETI_13,
      GodspeedMETI_15r = 100 - GodspeedMETI_15,
      GodspeedMETI_16r = 100 - GodspeedMETI_16,
      GodspeedMETI_17r = 100 - GodspeedMETI_17,
      GodspeedMETI_18r = 100 - GodspeedMETI_18,
      GodspeedMETI_19r = 100 - GodspeedMETI_19,
      GodspeedMETI_20r = 100 - GodspeedMETI_20,
      GodspeedMETI_21r = 100 - GodspeedMETI_21,
      GodspeedMETI_22r = 100 - GodspeedMETI_22,
      GodspeedMETI_23r = 100 - GodspeedMETI_23,
      GodspeedMETI_24r = 100 - GodspeedMETI_24,
      GodspeedMETI_25r = 100 - GodspeedMETI_25,
      GodspeedMETI_26r = 100 - GodspeedMETI_26,
      Appelman_5r = -1 * Appelman_5
    )
  
  if (study == "s2") {
    recoded_data <- recoded_data |>
      mutate(GodspeedMETI_12 = 100 - GodspeedMETI_25) 
  }
  
  return(recoded_data)
}

# data standardization ----------------------------------------------------

standardize_data <- function(data) {
  standardized_data <- data %>%
    mutate(across(
      c(
        starts_with("TrustBehaviour"),
        starts_with("GodspeedMETI"),
        starts_with("Appelman"),
        # Appelman_1:Appelman_3,
        starts_with("Experience"), 
        -Experience_4
      ),
      ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
      .names = "z.{.col}"
    ))
  return(standardized_data)
}

# data regrouping ---------------------------------------------------------

regroup_data <- function(data) {
  data$age_range <- cut(data$Age_1, 6)
  
  data <- data |>
    mutate(Education_regrouped = Education)
  
  data$Education_regrouped <- fct_collapse(
    data$Education_regrouped,
    non_post_secondary = c(
      "SomePrimary",
      "Primary",
      "SomeSecondarySchool",
      "Secondary",
      "PreferNotToSay"
    ),
    post_secondary = c(
      "VocationalOrSimilar",
      "SomeUniversity",
      "UniversityBachelorsDegree",
      "GraduateProfessionalDegree"
    )
  )
  
  data$AIChatbotsFrequency_regrouped <- fct_collapse(
    data$AIChatbotsFrequency,
    frequently = c("More than once a day", "About once a day", "About once a week"),
    occasionally = c(
      "About once every two weeks",
      "Less than once a month",
      "About once a month"
    ),
    rarely = c("I've only tried these a couple of times", "I've never used these")
    
  )
  
  data$ScienceContent_regrouped <- fct_collapse(
    data$ScienceContent,
    frequently = c("More than once a day", "About once a day", "About once a week"),
    occasionally = c(
      "About once every two weeks",
      "Less than once a month",
      "About once a month"
    ),
    rarely = c("I've only tried these a couple of times", "I've never used these")
    
  )
  
  return(data)
}


# Compute Scores -----------------------------------------------------------

get_content_trust_score_appelman_cols <- function(){
  return(c("z.Appelman_1", "z.Appelman_2", "z.Appelman_3"))
}

get_content_trust_score_behaviour_cols <- function(){
  return(c("z.TrustBehaviour_1", "z.TrustBehaviour_2",  "z.TrustBehaviour_3" ,"z.TrustBehaviour_4r", "z.TrustBehaviour_5r", "z.TrustBehaviour_6", "z.TrustBehaviour_9"))
}

get_content_trust_score_combined_cols <- function(){
  return(append(get_content_trust_score_appelman_cols(), get_content_trust_score_behaviour_cols()))
}

get_author_trust_score_behaviour_cols <- function(study){
  if (study == "s1"){
    author_trust_score_behaviour_cols = c("z.TrustBehaviour_7", "z.TrustBehaviour_8")
    
  } else if (study == "s2"){
    author_trust_score_behaviour_cols = c("z.TrustBehaviour_7", "z.TrustBehaviour_8", "z.TrustBehaviour_10",  "z.TrustBehaviour_11")
  }
  
  return(author_trust_score_behaviour_cols)
}

get_expertise_score_METI_cols <- function(){
  return(c("z.GodspeedMETI_10", "z.GodspeedMETI_13", "z.GodspeedMETI_15r", "z.GodspeedMETI_16r", "z.GodspeedMETI_17r", "z.GodspeedMETI_18r"))
}

get_integrity_score_METI_cols <- function(){
  return(c("z.GodspeedMETI_19r", "z.GodspeedMETI_20r", "z.GodspeedMETI_21r", "z.GodspeedMETI_22r"))
}
get_benevolence_score_METI_cols <- function(){
  return(c("z.GodspeedMETI_23r", "z.GodspeedMETI_24r", "z.GodspeedMETI_25r", "z.GodspeedMETI_26r"))
}
get_author_trust_score_METI_cols <- function(){
  return(append(append(get_expertise_score_METI_cols(), get_integrity_score_METI_cols()), get_benevolence_score_METI_cols()))
}
get_author_trust_score_combined_cols <- function(study){
  return(append(get_author_trust_score_behaviour_cols(study), get_author_trust_score_METI_cols()))
}
get_anthropomorphism_score_godspeed_cols <- function(){
  return(c("z.GodspeedMETI_1", "z.GodspeedMETI_2", "z.GodspeedMETI_3", "z.GodspeedMETI_4"))
}
get_likeability_score_godspeed_cols <- function(){
  return(c("z.GodspeedMETI_5", "z.GodspeedMETI_6", "z.GodspeedMETI_7", "z.GodspeedMETI_8", "z.GodspeedMETI_9"))
}
get_competence_score_godspeed_cols <- function(){
  return(c("z.GodspeedMETI_10", "z.GodspeedMETI_11", "z.GodspeedMETI_12", "z.GodspeedMETI_13", "z.GodspeedMETI_14"))
}
get_intention_cols <- function(){
  return(c("z.Experience_1", "z.Experience_2", "z.Experience_3"))
}
get_fear_cols <- function(){
  return(c("z.Experience_5", "z.Experience_6"))
}
# get_participant_expertise_cols <- function(){
#   return(c("z.Experience_8", "z.Experience_9", "z.Experience_10", "z.Experience_11"))
# }

get_writing_experience_cols <- function(){
  return(c("z.Experience_7", "z.Experience_10", "z.Experience_11"))
}

get_science_experience_cols <- function(){
  return(c("z.Experience_8", "z.Experience_9"))
}

get_writing_quality_cols <- function(){
  return(c("z.Appelman_4", "z.Appelman_5r", "z.Appelman_6"))
}


# Calculate scores for Content Trust, Author Trust, Anthropomorphism, Likeability, Competence, Expertise, Integrity, Benevolence
compute_scores <- function(data, study){
  content_trust_score_appelman_cols = get_content_trust_score_appelman_cols()
  content_trust_score_behaviour_cols = get_content_trust_score_behaviour_cols()
  content_trust_score_combined_cols =  get_content_trust_score_combined_cols()
  author_trust_score_behaviour_cols = get_author_trust_score_behaviour_cols(study)
  
  expertise_score_METI_cols = get_expertise_score_METI_cols()
  integrity_score_METI_cols = get_integrity_score_METI_cols()
  benevolence_score_METI_cols = get_benevolence_score_METI_cols()
  
  author_trust_score_METI_cols = get_author_trust_score_METI_cols()
  
  author_trust_score_combined_cols = get_author_trust_score_combined_cols(study)
  
  anthropomorphism_score_godspeed_cols = get_anthropomorphism_score_godspeed_cols()
  likeability_score_godspeed_cols = get_likeability_score_godspeed_cols()
  competence_score_godspeed_cols = get_competence_score_godspeed_cols()
  
  # intention to use AI 
  intention_cols = get_intention_cols()
  
  fear_cols = get_fear_cols()
  
  writing_expertise_cols = get_writing_experience_cols()
  science_expertise_cols = get_science_experience_cols()
  
  writing_quality_cols = get_writing_quality_cols()
  
  data <- data |>
    mutate(
      content_trust_appelman_score = rowMeans(
        select(data, all_of(content_trust_score_appelman_cols)), na.rm = TRUE),
      content_trust_behaviour_score = rowMeans(
        select(data, all_of(content_trust_score_behaviour_cols)), na.rm = TRUE),
      content_trust_combined_score = rowMeans(
        select(data, all_of(content_trust_score_combined_cols)), na.rm = TRUE),
      author_trust_behaviour_score = rowMeans(
        select(data, all_of(author_trust_score_behaviour_cols)), na.rm = TRUE),
      author_trust_METI_score = rowMeans(
        select(data, all_of(author_trust_score_METI_cols)), na.rm = TRUE),
      author_trust_combined_score = rowMeans(
        select(data, all_of(author_trust_score_combined_cols)), na.rm = TRUE),
      anthropomorphism_score = rowMeans(
        select(data, all_of(anthropomorphism_score_godspeed_cols)), na.rm = TRUE),
      likeability_score = rowMeans(
        select(data, all_of(likeability_score_godspeed_cols)), na.rm = TRUE),
      competence_score = rowMeans(
        select(data, all_of(competence_score_godspeed_cols)), na.rm = TRUE), 
      expertise_score = rowMeans(
        select(data, all_of(expertise_score_METI_cols)), na.rm = TRUE), 
      integrity_score = rowMeans(
        select(data, all_of(integrity_score_METI_cols)), na.rm = TRUE), 
      benevolence_score = rowMeans(
        select(data, all_of(benevolence_score_METI_cols)), na.rm = TRUE), 
      intention_to_use_score = rowMeans(
        select(data, all_of(intention_cols)), na.rm = TRUE), 
      fear_of_ai_score = rowMeans(
        select(data, all_of(fear_cols)), na.rm = TRUE), 
      writing_expertise = rowMeans(
        select(data, all_of(writing_expertise_cols)), na.rm = TRUE), 
      science_expertise = rowMeans(
        select(data, all_of(science_expertise_cols)), na.rm = TRUE),
      writing_quality = rowMeans(
        select(data, all_of(writing_quality_cols)), na.rm = TRUE)
    )
  
  return(data)
  
}

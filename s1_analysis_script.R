#| label: setup

library(knitr)
library(kableExtra)
# library(rstatix)
library(tidyverse)
# library(forcats)
# # library(qualtRics)
# library(haven)
# library(jtools)
# library(forcats)
# library(FactoMineR)
# library(psych)
# library(interactions)
# library(corrplot::corrplot)
# library(rcartocolor)
# library(ggpubr)
# library(ggtext)
# library(ggsignif)
# library(apaTables)
# 
# library(FSA)
# 
# library(performance)
# library(see)


source("./scripts/data-loading.R", local = TRUE)
source("./scripts/data-cleaning.R", local = TRUE)
source("./scripts/analysis-data-properties.R", local = TRUE)
source("./scripts/analysis-descriptives.R", local = TRUE)
source("./scripts/analysis-reliability.R", local = TRUE)
source("./scripts/analysis-inferential.R", local = TRUE)
source("./scripts/analysis-correlation.R", local = TRUE)
source("./scripts/analysis-moderation-mediation.R", local = TRUE)
source("./scripts/plotting.R", local = TRUE)
source("./scripts/common.R", local = TRUE)

directory_setup()
theme_set(my_theme)

s1_study <- "s1"



s1_raw_data <- load_data(s1_study)
s1_raw_data <- factorize_data(s1_study, s1_raw_data)



s1_raw_data <- explode_chatbots_used(s1_raw_data)

s1_raw_data <- explode_languages(s1_raw_data)



s1_raw_data <- exclude_non_consenting_participants(s1_raw_data)


summarize_columns(s1_raw_data, SurveyTopicCheck_coded)



s1_raw_data |> summarize_columns(Unrealistic)

s1_raw_data |> summarize_columns(Unrealistic, Unrealistic_coded) 


recruited_participants <- s1_raw_data |> count_recruited_participants(s1_study)


s1_cleaned_data <- s1_raw_data |> clean_data(s1_study)




# Compute skewness for each numeric variable
s1_cleaned_data |> check_skewness(s1_study)

# Shapiro-Wilk normality test 
s1_cleaned_data |>
  select(-Progress, -ConsentForm, -`Duration (in seconds)`, -Status, -Finished, -TopicCheck, -GodspeedMETI_28, -GodspeedMETI_29, -AIChatbotsUsed_NA, -Reread, -LanguagesFluent_NA) |>
  check_normality(s1_study)




missing_values_within_conditions(s1_cleaned_data)

# Perform imputation only on selected variables
s1_imputed_data <- impute_data(s1_cleaned_data)

missing_values_within_conditions(s1_imputed_data)



s1_recoded_data <- s1_imputed_data |> recode_data(s1_study)



s1_cleaned_data <- s1_recoded_data |> standardize_data()




s1_cleaned_data <- s1_cleaned_data |> regroup_data()




s1_cleaned_data <- s1_cleaned_data |> compute_scores(s1_study)




basic_descriptives(s1_cleaned_data$anthropomorphism_score)

s1_cleaned_data |> cronbachs_alpha(get_anthropomorphism_score_godspeed_cols())




basic_descriptives(s1_cleaned_data$likeability_score)

s1_cleaned_data |> cronbachs_alpha(get_likeability_score_godspeed_cols())




basic_descriptives(s1_cleaned_data$competence_score)

s1_cleaned_data |> cronbachs_alpha(get_competence_score_godspeed_cols())




basic_descriptives(s1_cleaned_data$expertise_score)

s1_cleaned_data |> cronbachs_alpha(get_expertise_score_METI_cols())




basic_descriptives(s1_cleaned_data$integrity_score)

s1_cleaned_data |> cronbachs_alpha(get_integrity_score_METI_cols())




basic_descriptives(s1_cleaned_data$benevolence_score)

s1_cleaned_data |> cronbachs_alpha(get_benevolence_score_METI_cols())




basic_descriptives(s1_cleaned_data$author_trust_METI_score)

s1_cleaned_data |> cronbachs_alpha(get_author_trust_score_METI_cols())



basic_descriptives(s1_cleaned_data$author_trust_behaviour_score)

s1_cleaned_data |> cronbachs_alpha(get_author_trust_score_behaviour_cols(s1_study))




basic_descriptives(s1_cleaned_data$author_trust_combined_score)

s1_cleaned_data |> cronbachs_alpha(get_author_trust_score_combined_cols(s1_study))




basic_descriptives(s1_cleaned_data$content_trust_appelman_score)

s1_cleaned_data |> cronbachs_alpha(get_content_trust_score_appelman_cols())



basic_descriptives(s1_cleaned_data$content_trust_behaviour_score)

s1_cleaned_data |> cronbachs_alpha(get_content_trust_score_behaviour_cols())




basic_descriptives(s1_cleaned_data$content_trust_combined_score)

s1_cleaned_data |> cronbachs_alpha(get_content_trust_score_combined_cols())



basic_descriptives(s1_cleaned_data$intention_to_use_score)
s1_cleaned_data |> cronbachs_alpha(get_intention_cols())


basic_descriptives(s1_cleaned_data$fear_of_ai_score)
s1_cleaned_data |> cronbachs_alpha(get_fear_cols())


basic_descriptives(s1_cleaned_data$professional_content_expertise)
s1_cleaned_data |> cronbachs_alpha(get_participant_expertise_cols())



basic_descriptives(s1_cleaned_data$`Duration (in seconds)` / 60)


s1_cleaned_data |> 
  summarize_columns(TechnicalIssues) 



s1_cleaned_data |> 
  summarize_columns(Unrealistic_coded) 


s1_cleaned_data |> 
  summarize_columns(Condition)



s1_cleaned_data |> 
  summarize_columns(age_range)

s1_cleaned_data |> 
  summarize_columns(age_range, Condition) 



s1_cleaned_data |> 
  summarize_columns(Gender) 

s1_cleaned_data |> 
  summarize_columns(Gender, Condition) 




s1_cleaned_data |> 
  summarize_columns(Sex)

s1_cleaned_data |> 
  summarize_columns(Sex, Condition)



s1_cleaned_data |> 
  summarize_columns(Education) 

s1_cleaned_data |> 
  summarize_columns(Education, Condition) 

s1_cleaned_data |> 
  summarize_columns(Education_regrouped) 

s1_cleaned_data |> 
  summarize_columns(Education_regrouped, Condition) 




s1_cleaned_data |> 
  summarize_columns(AIChatbotsFrequency) 

s1_cleaned_data |> 
  summarize_columns(AIChatbotsFrequency, Condition) 

s1_cleaned_data |> 
  summarize_columns(AIChatbotsFrequency_regrouped) 

s1_cleaned_data |> 
  summarize_columns(AIChatbotsFrequency_regrouped, Condition) 

s1_cleaned_data |> summarize_exploded_columns("AIChatbotsUsed", AIChatbotsUsed)
  
s1_cleaned_data |> summarize_exploded_columns("AIChatbotsUsed", AIChatbotsUsed, Condition)




s1_cleaned_data |> 
  summarize_columns(ScienceContent) 

s1_cleaned_data |> 
  summarize_columns(ScienceContent, Condition) 

s1_cleaned_data |> 
  summarize_columns(ScienceContent_regrouped) 

s1_cleaned_data |> 
  summarize_columns(ScienceContent_regrouped, Condition) 




s1_cleaned_data |> 
  summarize_columns(intention_to_use_score) 

s1_cleaned_data |> 
  summarize_columns(intention_to_use_score, Condition) 

s1_cleaned_data |> 
  summarize_columns(fear_of_ai_score) 

s1_cleaned_data |> 
  summarize_columns(fear_of_ai_score, Condition) 

s1_cleaned_data |> 
  summarize_columns(professional_content_expertise)

s1_cleaned_data |> 
  summarize_columns(professional_content_expertise, Condition) 

# changed opinion of AI
s1_cleaned_data |> 
  summarize_columns(Experience_4) 

s1_cleaned_data |> 
  summarize_columns(Experience_4, Condition) 


s1_cleaned_data |> 
  summarize_columns(Experience_7) 

s1_cleaned_data |> 
  summarize_columns(Experience_7, Condition) 





s1_cleaned_data |> 
  summarize_columns(English) 

s1_cleaned_data |> 
  summarize_columns(English, Condition) 



s1_cleaned_data |> summarize_exploded_columns("LanguagesFluent", LanguagesFluent)

s1_cleaned_data |> summarize_exploded_columns("LanguagesFluent", LanguagesFluent, Condition)




violin_plot(s1_cleaned_data, 
            s1_study,
            "Condition",
            "anthropomorphism_score",
            include_legend = FALSE)


s1_cleaned_data |> descriptives_by_group(Condition, anthropomorphism_score) 


aov_model <- s1_cleaned_data |> run_between_subjects_anova(anthropomorphism_score ~ Condition, "anthropomorphism_score")



s1_cleaned_data |> run_simple_effects_t_tests(anthropomorphism_score ~ Condition)


violin_plot(s1_cleaned_data, 
            s1_study,
            "Condition",
            "likeability_score",
            include_legend = FALSE)


s1_cleaned_data |> descriptives_by_group(Condition, likeability_score) 


aov_model <- s1_cleaned_data |> run_between_subjects_anova(likeability_score ~ Condition, "likeability_score")



s1_cleaned_data |> run_simple_effects_t_tests(likeability_score ~ Condition)


violin_plot(s1_cleaned_data, 
            s1_study,
            "Condition",
            "competence_score",
            include_legend = FALSE)


s1_cleaned_data |> descriptives_by_group(Condition, competence_score) 


aov_model <- s1_cleaned_data |> run_between_subjects_anova(competence_score ~ Condition, "competence_score")



s1_cleaned_data |> run_simple_effects_t_tests(competence_score ~ Condition)



plot <- violin_plot(s1_cleaned_data, 
            s1_study,
            "Condition",
            "content_trust_combined_score",
            include_legend = FALSE)

plot



s1_cleaned_data |> descriptives_by_group(Condition, content_trust_combined_score)



aov_model <- s1_cleaned_data |> run_between_subjects_anova(content_trust_combined_score ~ Condition, "competence_score")



s1_cleaned_data |> run_simple_effects_t_tests(content_trust_combined_score ~ Condition)



s1_cleaned_data |> linear_model(content_trust_combined_score ~ anthropomorphism_score)



s1_cleaned_data |> test_correlation("anthropomorphism_score", "content_trust_combined_score")

plot <- correlation_plot(s1_cleaned_data, 
            s1_study,
            "anthropomorphism_score",
            "content_trust_combined_score")

plot



s1_cleaned_data |> test_correlation("author_trust_combined_score", "content_trust_combined_score")

plots <- correlation_plot(s1_cleaned_data, 
            s1_study,
            "author_trust_combined_score",
            "content_trust_combined_score")

# Display the plots
plots$lm
plots$loess




lm_model <- s1_cleaned_data |> linear_model(content_trust_combined_score ~  author_trust_combined_score * anthropomorphism_score)

plot <- s1_cleaned_data |> interaction_plot_3(
  lm_model, 
  pred_name = "author_trust_combined_score",
  mod_name = "anthropomorphism_score", 
  target_name = "content_trust_combined_score", 
  study = s1_study)

plot






lm_model <- s1_cleaned_data |> linear_model(content_trust_combined_score ~ anthropomorphism_score * author_trust_combined_score * AIChatbotsFrequency_regrouped)

plot <- s1_cleaned_data |> interaction_plot_4(
  lm_model, 
  pred_name = "author_trust_combined_score",
  mod_name = "anthropomorphism_score", 
  mod2_name = "AIChatbotsFrequency_regrouped",
  mod2_v = c("frequently", "occasionally", "rarely"),
  target_name = "content_trust_combined_score", 
  study = s1_study)

plot




aov_model <- s1_cleaned_data |> run_between_subjects_anova(content_trust_combined_score ~ age_range, "content_trust_combined_score")




s1_cleaned_data |> test_correlation("Age_1", "content_trust_combined_score")

plots <- correlation_plot(s1_cleaned_data, 
            s1_study,
            "Age_1",
            "content_trust_combined_score")

# Display the plots
plots$lm
plots$loess




aov_model <- s1_cleaned_data |>
  droplevels() |>
  run_between_subjects_anova(content_trust_combined_score ~ Gender, "content_trust_combined_score")

s1_cleaned_data |>
  droplevels() |> run_simple_effects_t_tests(content_trust_combined_score ~ Gender)





aov_model <- s1_cleaned_data |>
  droplevels() |>
  run_between_subjects_anova(content_trust_combined_score ~ Sex, "content_trust_combined_score")




aov_model <- s1_cleaned_data |>
  droplevels() |>
  run_between_subjects_anova(content_trust_combined_score ~ Education_regrouped, "content_trust_combined_score")




aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(content_trust_combined_score ~ AIChatbotsFrequency_regrouped, "content_trust_combined_score")

s1_cleaned_data |> run_simple_effects_t_tests(content_trust_combined_score ~ AIChatbotsFrequency_regrouped)

plot <- violin_plot(s1_cleaned_data, 
            s1_study,
            "AIChatbotsFrequency_regrouped",
            "content_trust_combined_score",
            include_legend = FALSE, 
            comparisons = list(  c("rarely", "occasionally"),
    c("rarely", "frequently"),
    c("occasionally", "frequently") ))

plot



lm_model <- s1_cleaned_data |> linear_model(content_trust_combined_score ~ Condition * AIChatbotsFrequency_regrouped)

plot <- s1_cleaned_data |>
  categorical_interaction_plot_3(lm_model,
                                pred_name = "Condition",
                                mod_name = "AIChatbotsFrequency_regrouped",
                                target_name = "content_trust_combined_score",
                                study = s1_study
                                )

plot




aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(content_trust_combined_score ~ ScienceContent_regrouped, "content_trust_combined_score")

s1_cleaned_data |> run_simple_effects_t_tests(content_trust_combined_score ~ ScienceContent_regrouped)

plot <- violin_plot(s1_cleaned_data, 
            s1_study,
            "ScienceContent_regrouped",
            "content_trust_combined_score",
            include_legend = FALSE, 
            comparisons = list(  c("rarely", "occasionally"),
    c("rarely", "frequently"),
    c("occasionally", "frequently") ))

plot





lm_model <- s1_cleaned_data |> linear_model(content_trust_combined_score ~ Condition * ScienceContent_regrouped)

plot <- s1_cleaned_data |>
  categorical_interaction_plot_3(lm_model,
                                pred_name = "Condition",
                                mod_name = "ScienceContent_regrouped",
                                target_name = "content_trust_combined_score",
                                study = s1_study
                                )

plot




s1_cleaned_data |> test_correlation("intention_to_use_score", "content_trust_combined_score")

plots <- correlation_plot(s1_cleaned_data, 
            s1_study,
            "intention_to_use_score",
            "content_trust_combined_score")

# Display the plots
plots$lm
plots$loess





s1_cleaned_data |> descriptives_by_group(Experience_4, content_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(content_trust_combined_score ~ Experience_4, "content_trust_combined_score")

s1_cleaned_data |> run_simple_effects_t_tests(content_trust_combined_score ~ Experience_4)

plot <- violin_plot(s1_cleaned_data,
            s1_study,
            "Experience_4",
            "content_trust_combined_score",
            include_legend = FALSE,
            comparisons = list(  
              c("1", "2"),
              c("2", "3"),
              c("3", "4"),
              c("4", "5"),
              c("1", "3"),
              c("2", "4"),
              c("3", "5"),
              c("1", "4"),
              c("2", "5"),
              c("1", "5")
              )
    )





s1_cleaned_data |> test_correlation("fear_of_ai_score", "content_trust_combined_score")

plots <- correlation_plot(s1_cleaned_data, 
            s1_study,
            "fear_of_ai_score",
            "content_trust_combined_score")

# Display the plots
plots$lm
plots$loess




s1_cleaned_data |> descriptives_by_group(Experience_7, content_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(content_trust_combined_score ~ Experience_7, "content_trust_combined_score")

s1_cleaned_data |> run_simple_effects_t_tests(content_trust_combined_score ~ Experience_7)

plot <- violin_plot(s1_cleaned_data,
            s1_study,
            "Experience_7",
            "content_trust_combined_score",
            include_legend = FALSE,
            comparisons = list(  
              c("1", "2"),
              c("2", "3"),
              c("3", "4"),
              c("4", "5"),
              c("1", "3"),
              c("2", "4"),
              c("3", "5"),
              c("1", "4"),
              c("2", "5"),
              c("1", "5")
              )
    )




s1_cleaned_data |> test_correlation("professional_content_expertise", "content_trust_combined_score")

plots <- correlation_plot(s1_cleaned_data, 
            s1_study,
            "professional_content_expertise",
            "content_trust_combined_score")

# Display the plots
plots$lm
plots$loess



s1_cleaned_data |> descriptives_by_group(Appelman_4, content_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(content_trust_combined_score ~ Appelman_4, "content_trust_combined_score")

s1_cleaned_data |> run_simple_effects_t_tests(content_trust_combined_score ~ Appelman_4)

plot <- violin_plot(s1_cleaned_data,
            s1_study,
            "Appelman_4",
            "content_trust_combined_score",
            include_legend = FALSE,
            comparisons = list(  
              c("-2", "-1"),
              c("-1", "0"),
              c("0", "1"),
              c("1", "2"),
              c("-2", "0"),
              c("-1", "1"),
              c("0", "2"),
              c("-2", "1"),
              c("-1", "2"),
              c("-2", "2")
              )
    )



lm_model <- s1_cleaned_data |> linear_model(content_trust_combined_score ~ Condition * Appelman_4)

plot <- s1_cleaned_data |>
  categorical_interaction_plot_3(lm_model,
                                pred_name = "Condition",
                                mod_name = "Appelman_4",
                                target_name = "content_trust_combined_score",
                                study = s1_study
                                )

plot



s1_cleaned_data |> descriptives_by_group(Appelman_5, content_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(content_trust_combined_score ~ Appelman_5, "content_trust_combined_score")

s1_cleaned_data |> run_simple_effects_t_tests(content_trust_combined_score ~ Appelman_5)

plot <- violin_plot(s1_cleaned_data,
            s1_study,
            "Appelman_5",
            "content_trust_combined_score",
            include_legend = FALSE,
            comparisons = list(  
              c("-2", "-1"),
              c("-1", "0"),
              c("0", "1"),
              c("1", "2"),
              c("-2", "0"),
              c("-1", "1"),
              c("0", "2"),
              c("-2", "1"),
              c("-1", "2"),
              c("-2", "2")
              )
    )



lm_model <- s1_cleaned_data |> linear_model(content_trust_combined_score ~ Condition * Appelman_5)

plot <- s1_cleaned_data |>
  categorical_interaction_plot_3(lm_model,
                                pred_name = "Condition",
                                mod_name = "Appelman_5",
                                target_name = "content_trust_combined_score",
                                study = s1_study
                                )

plot



s1_cleaned_data |> descriptives_by_group(Appelman_6, content_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(content_trust_combined_score ~ Appelman_6, "content_trust_combined_score")

s1_cleaned_data |> run_simple_effects_t_tests(content_trust_combined_score ~ Appelman_6)

plot <- violin_plot(s1_cleaned_data,
            s1_study,
            "Appelman_6",
            "content_trust_combined_score",
            include_legend = FALSE,
            comparisons = list(  
              c("-2", "-1"),
              c("-1", "0"),
              c("0", "1"),
              c("1", "2"),
              c("-2", "0"),
              c("-1", "1"),
              c("0", "2"),
              c("-2", "1"),
              c("-1", "2"),
              c("-2", "2")
              )
    )



lm_model <- s1_cleaned_data |> linear_model(content_trust_combined_score ~ Condition * Appelman_6)

plot <- s1_cleaned_data |>
  categorical_interaction_plot_3(lm_model,
                                pred_name = "Condition",
                                mod_name = "Appelman_6",
                                target_name = "content_trust_combined_score",
                                study = s1_study
                                )

plot



s1_cleaned_data |> descriptives_by_group(SurveyTopicCheck_coded, content_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(content_trust_combined_score ~ SurveyTopicCheck_coded, "content_trust_combined_score")



s1_cleaned_data |> descriptives_by_group(Unrealistic, content_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(content_trust_combined_score ~ Unrealistic, "content_trust_combined_score")



s1_cleaned_data |> descriptives_by_group(Unrealistic_coded, content_trust_combined_score) 

aov_model <- s1_cleaned_data |>
   group_by(Unrealistic_coded) |> filter(n() > 3) |> ungroup() |>
  run_between_subjects_anova(content_trust_combined_score ~ Unrealistic_coded, "content_trust_combined_score")

s1_cleaned_data |>
  droplevels() |>
  run_simple_effects_t_tests(content_trust_combined_score ~ Unrealistic_coded)




plot <- violin_plot(s1_cleaned_data, 
            s1_study,
            "Condition",
            "author_trust_combined_score",
            include_legend = FALSE)

plot



s1_cleaned_data |> descriptives_by_group(Condition, author_trust_combined_score)




s1_cleaned_data |> run_inferential(author_trust_combined_score ~ Condition)
# s1_cleaned_data |> descriptives_by_group(Condition, author_trust_combined_score) 
# 
# aov_model <- s1_cleaned_data |>
#   group_by(Condition) |> filter(n() > 3) |> ungroup() |>
#   droplevels() |>
#   run_between_subjects_anova(author_trust_combined_score ~ Condition, "author_trust_combined_score")

# s1_cleaned_data |>
#   droplevels() |>
#   run_simple_effects_t_tests(author_trust_combined_score ~ Condition)




s1_cleaned_data |> linear_model(author_trust_combined_score ~ anthropomorphism_score)




s1_cleaned_data |> test_correlation("anthropomorphism_score", "author_trust_combined_score")

plot <- correlation_plot(s1_cleaned_data, 
            s1_study,
            "anthropomorphism_score",
            "author_trust_combined_score")

plot





s1_cleaned_data$Condition_Compressed <- ifelse(s1_cleaned_data$Condition == "High", 1, 0)

s1_cleaned_data$Condition_Medium <- ifelse(s1_cleaned_data$Condition == "Medium", 1, 0)
s1_cleaned_data$Condition_High <- ifelse(s1_cleaned_data$Condition == "High", 1, 0)

s1_gen_ai_con_only_data <- s1_cleaned_data |> filter(Condition != "High") 
s1_gen_ai_con_only_data$Condition <- factor(s1_gen_ai_con_only_data$Condition, levels = c("Low", "Medium"))



model_design <- '
  # Mediation paths
  anthropomorphism_score ~ a1 * Condition
  author_trust_combined_score ~ a2 * anthropomorphism_score + d * Condition 
  content_trust_combined_score ~ b1 * author_trust_combined_score + b2 * anthropomorphism_score + c_prime * Condition

  # Indirect effects
  indirect1 := a1 * b2
  indirect2 := a1 * a2 * b1
  indirect3 := d * b1

  # Total effect
  total := c_prime + indirect1 + indirect2 + indirect3
'
s1_fit_compressed <- s1_gen_ai_con_only_data |> test_med_mod_model(model_design)



plot_model(s1_fit_compressed, s1_study)


model_design <- '
  # Mediation paths
  anthropomorphism_score ~ a1 * Condition_Compressed
  author_trust_combined_score ~ a2 * anthropomorphism_score + d * Condition_Compressed 
  content_trust_combined_score ~ b1 * author_trust_combined_score + b2 * anthropomorphism_score + c_prime * Condition_Compressed

  # Indirect effects
  indirect1 := a1 * b2
  indirect2 := a1 * a2 * b1
  indirect3 := d * b1

  # Total effect
  total := c_prime + indirect1 + indirect2 + indirect3
'
s1_fit_compressed <- s1_cleaned_data |> test_med_mod_model(model_design)

plot_model(s1_fit_compressed, s1_study)


model_design <- '
  # Mediation paths
  anthropomorphism_score ~ a1 * Condition_Medium + a2 * Condition_High
  author_trust_combined_score ~ d1 * Condition_Medium + d2 * Condition_High + a3 * anthropomorphism_score
  content_trust_combined_score ~ b1 * author_trust_combined_score + b2 * anthropomorphism_score + c1 * Condition_Medium + c2 * Condition_High

  # Indirect effects
  indirect1_med := a1 * b2
  indirect1_high := a2 * b2

  indirect2_med := a1 * a3 * b1
  indirect2_high := a2 * a3 * b1

  indirect3_med := d1 * b1
  indirect3_high := d2 * b1

  total_med := c1 + indirect1_med + indirect2_med + indirect3_med
  total_high := c2 + indirect1_high + indirect2_high + indirect3_high
'
s1_fit_basic <- s1_cleaned_data |> test_med_mod_model(model_design)


plot_model(s1_fit_basic, s1_study)



model_design <- '
  # Mediation paths
  anthropomorphism_score ~ a1 * Condition
  likeability_score ~ a3 * anthropomorphism_score + e * Condition
  author_trust_combined_score ~ a2 * anthropomorphism_score + d * Condition + b3 * likeability_score
  content_trust_combined_score ~ b1 * author_trust_combined_score + b2 * anthropomorphism_score + c_prime * Condition

  # Indirect effects
  indirect1 := a1 * b2
  indirect2 := a1 * a2 * b1
  indirect3 := d * b1
  indirect4 := e * b3 * b1
  indirect5 := a1 * a3 * b3 * b1

  # Total effect
  total := c_prime + indirect1 + indirect2 + indirect3 + indirect4 + indirect5
'
s1_fit_with_likeability <- s1_gen_ai_con_only_data |> test_med_mod_model(model_design)


plot <- plot_model(s1_fit_with_likeability, s1_study)



model_design <- '
  # Mediation paths
  anthropomorphism_score ~ a1 * Condition
  likeability_score ~ a3 * anthropomorphism_score + e * Condition
  competence_score ~ f * Condition + b4 * likeability_score + b5 * anthropomorphism_score
  author_trust_combined_score ~ a2 * anthropomorphism_score + d * Condition + b3 * likeability_score + b6 * competence_score
  content_trust_combined_score ~ b1 * author_trust_combined_score + b2 * anthropomorphism_score + b7 * competence_score + b8 * likeability_score + c_prime * Condition

  # Indirect effects
  indirect1 := a1 * b2
  indirect2 := a1 * a2 * b1
  indirect3 := d * b1
  indirect4 := e * b3 * b1
  indirect5 := a1 * a3 * b3 * b1
  indirect6 := f * b7
  indirect7 := f * b6 * b1
  indirect8 := e * b8

  # Total effect
  total := c_prime + indirect1 + indirect2 + indirect3 + indirect4 + indirect5 + indirect6 + indirect7 + indirect8
'
s1_fit_with_likeability_and_competence <- s1_gen_ai_con_only_data |> test_med_mod_model(model_design)


plot <- plot_model(s1_fit_with_likeability_and_competence, s1_study)



model_design <- '
  # Mediation paths
  anthropomorphism_score ~ a1 * Condition_Compressed
  likeability_score ~ a3 * anthropomorphism_score + e * Condition_Compressed
  author_trust_combined_score ~ a2 * anthropomorphism_score + d * Condition_Compressed + b3 * likeability_score
  content_trust_combined_score ~ b1 * author_trust_combined_score + b2 * anthropomorphism_score + c_prime * Condition_Compressed

  # Indirect effects
  indirect1 := a1 * b2
  indirect2 := a1 * a2 * b1
  indirect3 := d * b1
  indirect4 := e * b3 * b1
  indirect5 := a1 * a3 * b3 * b1

  # Total effect
  total := c_prime + indirect1 + indirect2 + indirect3 + indirect4 + indirect5
'
s1_fit_with_likeability <- s1_cleaned_data |> test_med_mod_model(model_design)


plot <- plot_model(s1_fit_with_likeability, s1_study)



model_design <- '
  # Mediation paths
  anthropomorphism_score ~ a1 * Condition_Compressed
  likeability_score ~ a3 * anthropomorphism_score + e * Condition_Compressed
  competence_score ~ f * Condition_Compressed + b4 * likeability_score + b5 * anthropomorphism_score
  author_trust_combined_score ~ a2 * anthropomorphism_score + d * Condition_Compressed + b3 * likeability_score + b6 * competence_score
  content_trust_combined_score ~ b1 * author_trust_combined_score + b2 * anthropomorphism_score + b7 * competence_score + b8 * likeability_score + c_prime * Condition_Compressed

  # Indirect effects
  indirect1 := a1 * b2
  indirect2 := a1 * a2 * b1
  indirect3 := d * b1
  indirect4 := e * b3 * b1
  indirect5 := a1 * a3 * b3 * b1
  indirect6 := f * b7
  indirect7 := f * b6 * b1
  indirect8 := e * b8

  # Total effect
  total := c_prime + indirect1 + indirect2 + indirect3 + indirect4 + indirect5 + indirect6 + indirect7 + indirect8
'
s1_fit_with_likeability_and_competence <- s1_cleaned_data |> test_med_mod_model(model_design)


plot <- plot_model(s1_fit_with_likeability_and_competence, s1_study)




# lm_model <- lm(author_trust_combined_score ~ anthropomorphism_score * content_trust_combined_score, data = s1_cleaned_data)
# summary(lm_model)
# 
# plot <- interact_plot(lm_model, 
#               pred = anthropomorphism_score, 
#               modx = content_trust_combined_score,  
#               x.label ="Anthropomorphism Score",
#               y.label = "Author Trust Score",
#               legend.main = "Content Trust Score",
#               plot.points = TRUE, jitter = 0.5, point.alpha = 0.25, geom="bar", 
#               colors = safe_pal) +
#   theme(legend.position = "right")
# 
# ggsave("plots/s1/author_anthropomorphism_content_interaction.png", plot = plot, create.dir = TRUE)
# 
# plot




# lm_model <- lm(author_trust_combined_score ~ anthropomorphism_score * content_trust_combined_score * AIChatbotsFrequency_regrouped, data = s1_cleaned_data)
# summary(lm_model)
# 
# plot <- interact_plot(lm_model, 
#               pred = anthropomorphism_score, 
#               modx = content_trust_combined_score,
#               mod2 = AIChatbotsFrequency_regrouped,
#               x.label ="Anthropomorphism Score",
#               y.label = "Author Trust Score",
#               legend.main = "Content Trust Score",
#               mod2.values = c("frequently", "occasionally", "rarely"),
#               mod2.labels = c("frequently", "occasionally", "rarely"),
#               plot.points = TRUE, jitter = 0.5, point.alpha = 0.25, geom="bar", 
#               colors = safe_pal) +
#   theme(legend.position = "right")
# 
# ggsave("plots/s1/author_anthropomorphism_content_frequency_interaction.png", plot = plot, create.dir = TRUE)
# 
# plot




# lm_model <- lm(author_trust_combined_score ~ anthropomorphism_score * AIChatbotsFrequency_regrouped, data = s1_cleaned_data)
# summary(lm_model)
# 
# plot <- interact_plot(lm_model, 
#               pred = anthropomorphism_score, 
#               modx = AIChatbotsFrequency_regrouped,
#               x.label ="Anthropomorphism Score",
#               y.label = "Author Trust Score",
#               legend.main = "Frequency",
#               # mod2.values = c("frequently", "occasionally", "rarely"),
#               # mod2.labels = c("frequently", "occasionally", "rarely"),
#               plot.points = TRUE, jitter = 0.5, point.alpha = 0.25, geom="bar", 
#               colors = safe_pal) +
#   theme(legend.position = "right")
# 
# ggsave("plots/s1/author_anthropomorphism_frequency_interaction.png", plot = plot, create.dir = TRUE)
# 
# plot



s1_cleaned_data |> descriptives_by_group(age_range, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  group_by(age_range) |> filter(n() > 3) |> ungroup() |>
  droplevels() |>
  run_between_subjects_anova(author_trust_combined_score ~ age_range, "author_trust_combined_score")



s1_cleaned_data |> test_correlation("Age_1", "author_trust_combined_score")

plots <- correlation_plot(s1_cleaned_data, 
            s1_study,
            "Age_1",
            "author_trust_combined_score")

# Display the plots
plots$lm
plots$loess



s1_cleaned_data |> descriptives_by_group(Gender, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  group_by(Gender) |> filter(n() > 3) |> ungroup() |>
  droplevels() |>
  run_between_subjects_anova(author_trust_combined_score ~ Gender, "author_trust_combined_score")




s1_cleaned_data |> descriptives_by_group(Sex, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  group_by(Sex) |> filter(n() > 3) |> ungroup() |>
  droplevels() |>
  run_between_subjects_anova(author_trust_combined_score ~ Sex, "author_trust_combined_score")




s1_cleaned_data |> descriptives_by_group(Education, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  group_by(Education) |> filter(n() > 3) |> ungroup() |>
  droplevels() |>
  run_between_subjects_anova(author_trust_combined_score ~ Education, "author_trust_combined_score")



s1_cleaned_data |> descriptives_by_group(AIChatbotsFrequency_regrouped, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  group_by(AIChatbotsFrequency_regrouped) |> filter(n() > 3) |> ungroup() |>
  droplevels() |>
  run_between_subjects_anova(author_trust_combined_score ~ AIChatbotsFrequency_regrouped, "author_trust_combined_score")

s1_cleaned_data |>
  droplevels() |>
  run_simple_effects_t_tests(author_trust_combined_score ~ AIChatbotsFrequency_regrouped)

plot <- violin_plot(s1_cleaned_data, 
            s1_study,
            "AIChatbotsFrequency_regrouped",
            "author_trust_combined_score",
            include_legend = FALSE, 
            comparisons = list(  c("rarely", "occasionally"),
    c("rarely", "frequently"),
    c("occasionally", "frequently") ))

plot




lm_model <- s1_cleaned_data |> linear_model(author_trust_combined_score ~ Condition * AIChatbotsFrequency_regrouped)


plot <- s1_cleaned_data |>
  categorical_interaction_plot_3(lm_model,
                                pred_name = "Condition",
                                mod_name = "AIChatbotsFrequency_regrouped",
                                target_name = "author_trust_combined_score",
                                study = s1_study
                                )

plot


s1_cleaned_data |> descriptives_by_group(ScienceContent, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  group_by(ScienceContent) |> filter(n() > 3) |> ungroup() |>
  droplevels() |>
  run_between_subjects_anova(author_trust_combined_score ~ ScienceContent, "author_trust_combined_score")


s1_cleaned_data |> descriptives_by_group(ScienceContent_regrouped, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  group_by(ScienceContent_regrouped) |> filter(n() > 3) |> ungroup() |>
  droplevels() |>
  run_between_subjects_anova(author_trust_combined_score ~ ScienceContent_regrouped, "author_trust_combined_score")



s1_cleaned_data |> test_correlation("intention_to_use_score", "author_trust_combined_score")

plots <- correlation_plot(s1_cleaned_data, 
            s1_study,
            "intention_to_use_score",
            "author_trust_combined_score")

# Display the plots
plots$lm
plots$loess



lm_model <- s1_cleaned_data |> linear_model(author_trust_combined_score ~ Condition * intention_to_use_score)


plot <- s1_cleaned_data |>
  categorical_interaction_plot_3(lm_model,
                                pred_name = "Condition",
                                mod_name = "intention_to_use_score",
                                target_name = "author_trust_combined_score",
                                study = s1_study
                                )

plot


s1_cleaned_data |> descriptives_by_group(Experience_4, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(author_trust_combined_score ~ Experience_4, "author_trust_combined_score")

s1_cleaned_data |> run_simple_effects_t_tests(author_trust_combined_score ~ Experience_4)

plot <- violin_plot(s1_cleaned_data,
            s1_study,
            "Experience_4",
            "author_trust_combined_score",
            include_legend = FALSE,
            comparisons = list(  
              c("1", "2"),
              c("2", "3"),
              c("3", "4"),
              c("4", "5"),
              c("1", "3"),
              c("2", "4"),
              c("3", "5"),
              c("1", "4"),
              c("2", "5"),
              c("1", "5")
              )
    )


s1_cleaned_data |> test_correlation("fear_of_ai_score", "author_trust_combined_score")

plots <- correlation_plot(s1_cleaned_data, 
            s1_study,
            "fear_of_ai_score",
            "author_trust_combined_score")

# Display the plots
plots$lm
plots$loess


s1_cleaned_data |> descriptives_by_group(Experience_7, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(author_trust_combined_score ~ Experience_7, "author_trust_combined_score")

s1_cleaned_data |> run_simple_effects_t_tests(author_trust_combined_score ~ Experience_7)

plot <- violin_plot(s1_cleaned_data,
            s1_study,
            "Experience_7",
            "author_trust_combined_score",
            include_legend = FALSE,
            comparisons = list(  
              c("1", "2"),
              c("2", "3"),
              c("3", "4"),
              c("4", "5"),
              c("1", "3"),
              c("2", "4"),
              c("3", "5"),
              c("1", "4"),
              c("2", "5"),
              c("1", "5")
              )
    )


s1_cleaned_data |> test_correlation("professional_content_expertise", "author_trust_combined_score")

plots <- correlation_plot(s1_cleaned_data, 
            s1_study,
            "professional_content_expertise",
            "author_trust_combined_score")

# Display the plots
plots$lm
plots$loess


s1_cleaned_data |> descriptives_by_group(Appelman_4, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(author_trust_combined_score ~ Appelman_4, "author_trust_combined_score")

s1_cleaned_data |> run_simple_effects_t_tests(author_trust_combined_score ~ Appelman_4)

plot <- violin_plot(s1_cleaned_data,
            s1_study,
            "Appelman_4",
            "author_trust_combined_score",
            include_legend = FALSE,
            comparisons = list(  
              c("-2", "-1"),
              c("-1", "0"),
              c("0", "1"),
              c("1", "2"),
              c("-2", "0"),
              c("-1", "1"),
              c("0", "2"),
              c("-2", "1"),
              c("-1", "2"),
              c("-2", "2")
              )
    )

lm_model <- s1_cleaned_data |> linear_model(author_trust_combined_score ~ Condition * Appelman_4)

plot <- s1_cleaned_data |>
  categorical_interaction_plot_3(lm_model,
                                pred_name = "Condition",
                                mod_name = "Appelman_4",
                                target_name = "author_trust_combined_score",
                                study = s1_study
                                )

plot


s1_cleaned_data |> descriptives_by_group(Appelman_5, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(author_trust_combined_score ~ Appelman_5, "author_trust_combined_score")

s1_cleaned_data |> run_simple_effects_t_tests(author_trust_combined_score ~ Appelman_5)

plot <- violin_plot(s1_cleaned_data,
            s1_study,
            "Appelman_5",
            "author_trust_combined_score",
            include_legend = FALSE,
            comparisons = list(  
              c("-2", "-1"),
              c("-1", "0"),
              c("0", "1"),
              c("1", "2"),
              c("-2", "0"),
              c("-1", "1"),
              c("0", "2"),
              c("-2", "1"),
              c("-1", "2"),
              c("-2", "2")
              )
    )


lm_model <- s1_cleaned_data |> linear_model(author_trust_combined_score ~ Condition * Appelman_5)

plot <- s1_cleaned_data |>
  categorical_interaction_plot_3(lm_model,
                                pred_name = "Condition",
                                mod_name = "Appelman_5",
                                target_name = "author_trust_combined_score",
                                study = s1_study
                                )

plot


s1_cleaned_data |> descriptives_by_group(Appelman_6, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(author_trust_combined_score ~ Appelman_6, "author_trust_combined_score")

s1_cleaned_data |> run_simple_effects_t_tests(author_trust_combined_score ~ Appelman_6)

plot <- violin_plot(s1_cleaned_data,
            s1_study,
            "Appelman_6",
            "author_trust_combined_score",
            include_legend = FALSE,
            comparisons = list(  
              c("-2", "-1"),
              c("-1", "0"),
              c("0", "1"),
              c("1", "2"),
              c("-2", "0"),
              c("-1", "1"),
              c("0", "2"),
              c("-2", "1"),
              c("-1", "2"),
              c("-2", "2")
              )
    )
lm_model <- s1_cleaned_data |> linear_model(author_trust_combined_score ~ Condition * Appelman_6)

plot <- s1_cleaned_data |>
  categorical_interaction_plot_3(lm_model,
                                pred_name = "Condition",
                                mod_name = "Appelman_6",
                                target_name = "author_trust_combined_score",
                                study = s1_study
                                )

plot



s1_cleaned_data |> descriptives_by_group(SurveyTopicCheck_coded, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(author_trust_combined_score ~ SurveyTopicCheck_coded, "author_trust_combined_score")

s1_cleaned_data |>
  droplevels() |>
  run_simple_effects_t_tests(author_trust_combined_score ~ SurveyTopicCheck_coded)





s1_cleaned_data |> descriptives_by_group(Unrealistic, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
  run_between_subjects_anova(author_trust_combined_score ~ Unrealistic, "author_trust_combined_score")

s1_cleaned_data |> descriptives_by_group(Unrealistic_coded, author_trust_combined_score) 

aov_model <- s1_cleaned_data |>
   group_by(Unrealistic_coded) |> filter(n() > 3) |> ungroup() |>
  run_between_subjects_anova(author_trust_combined_score ~ Unrealistic_coded, "author_trust_combined_score")

s1_cleaned_data |>
  droplevels() |>
  run_simple_effects_t_tests(author_trust_combined_score ~ Unrealistic_coded)





correlation_data <- s1_cleaned_data |> select(author_trust_combined_score,

                                           content_trust_combined_score,

                                           likeability_score,

                                           competence_score,

                                           expertise_score,

                                           integrity_score,

                                           benevolence_score,

                                           anthropomorphism_score,

                                           content_trust_appelman_score,

                                           content_trust_behaviour_score,

                                           author_trust_METI_score,

                                           author_trust_behaviour_score)

corr_matrix = cor(correlation_data)

corr_matrix_p <- psych::corr.test(correlation_data)

kable(head(corr_matrix))

corrplot::corrplot(corr_matrix, type="upper", method = "circle", order = "hclust",

         addCoef.col = "white", # Add coefficient of correlation

         tl.col="black", tl.srt=45, #Text label color and rotation

         p.mat = corr_matrix_p$p, sig.level = 0.01, insig = "blank",

         diag = FALSE)




correlation_data_high <- s1_cleaned_data |>

  filter(Condition == "High") |>

  select(author_trust_combined_score, content_trust_combined_score, likeability_score, competence_score, expertise_score, integrity_score, benevolence_score)

corr_matrix_high = cor(correlation_data_high)

corr_matrix_p_high <- psych::corr.test(correlation_data_high)

kable(head(corr_matrix_high))

corrplot::corrplot(corr_matrix_high, type="upper", method = "circle", order = "hclust",

         addCoef.col = "white", # Add coefficient of correlation

         tl.col="black", tl.srt=45, #Text label color and rotation

         p.mat = corr_matrix_p_high$p, sig.level = 0.01, insig = "blank",

         diag = FALSE)




correlation_data_medium <- s1_cleaned_data |>

  filter(Condition == "Medium") |>

  select(author_trust_combined_score, content_trust_combined_score, likeability_score, competence_score, expertise_score, integrity_score, benevolence_score)

corr_matrix_medium = cor(correlation_data_medium)

corr_matrix_p_medium <- psych::corr.test(correlation_data_medium)

kable(head(corr_matrix_medium))

corrplot::corrplot(corr_matrix_medium, type="upper", method = "circle", order = "hclust",

         addCoef.col = "white", # Add coefficient of correlation

         tl.col="black", tl.srt=45, #Text label color and rotation

         p.mat = corr_matrix_p_medium$p, sig.level = 0.01, insig = "blank",

         diag = FALSE)




correlation_data_low <- s1_cleaned_data |>

  filter(Condition == "Low") |>

  select(author_trust_combined_score, content_trust_combined_score, likeability_score, competence_score, expertise_score, integrity_score, benevolence_score)

corr_matrix_low = cor(correlation_data_low)

corr_matrix_p_low <- psych::corr.test(correlation_data_low)

kable(head(corr_matrix_low))

corrplot::corrplot(corr_matrix_low, type="upper", method = "circle", order = "hclust",

         addCoef.col = "white", # Add coefficient of correlation

         tl.col="black", tl.srt=45, #Text label color and rotation

         p.mat = corr_matrix_p_low$p, sig.level = 0.01, insig = "blank",

         diag = FALSE)




cocor::cocor(~ `content_trust_combined_score` + `expertise_score` | `content_trust_combined_score` + `expertise_score`,

      data = list(

        as.data.frame(s1_cleaned_data |> filter(Condition == "High")),

        as.data.frame(s1_cleaned_data |> filter(Condition == "Low"))

      )

)




cocor::cocor(~ `content_trust_combined_score` + `competence_score` | `content_trust_combined_score` + `competence_score`,

      data = list(

        as.data.frame(s1_cleaned_data |> filter(Condition == "High")),

        as.data.frame(s1_cleaned_data |> filter(Condition == "Low"))

      )

)




cocor::cocor(~ `content_trust_combined_score` + `likeability_score` | `content_trust_combined_score` + `likeability_score`,

      data = list(

        as.data.frame(s1_cleaned_data |> filter(Condition == "High")),

        as.data.frame(s1_cleaned_data |> filter(Condition == "Low"))

      )

)




cocor::cocor(~ `content_trust_combined_score` + `benevolence_score` | `content_trust_combined_score` + `benevolence_score`,

      data = list(

        as.data.frame(s1_cleaned_data |> filter(Condition == "High")),

        as.data.frame(s1_cleaned_data |> filter(Condition == "Low"))

      )

)




cocor::cocor(~ `content_trust_combined_score` + `integrity_score` | `content_trust_combined_score` + `integrity_score`,

      data = list(

        as.data.frame(s1_cleaned_data |> filter(Condition == "High")),

        as.data.frame(s1_cleaned_data |> filter(Condition == "Low"))

      )

)




cocor::cocor(~ `content_trust_combined_score` + `author_trust_combined_score` | `content_trust_combined_score` + `author_trust_combined_score`,

      data = list(

        as.data.frame(s1_cleaned_data |> filter(Condition == "High")),

        as.data.frame(s1_cleaned_data |> filter(Condition == "Low"))

      )

)



my_packages = c("rstatix", "stats", "effectsize", "psych", "interactions",
                 "corrplot::corrplot", "performance", "FSA", "see")

write_bib(my_packages, file= "outputs/s1/packages.bib")

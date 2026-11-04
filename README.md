# Trusting the Machine: analysis code

Analysis code for "Trusting the Machine: Separating Trust in GenAI Authors From Trust in Their Content". Everything the manuscript reports is produced by the scripts below from the anonymized data in `data/anonymized/`.

Run from the project root:

```
Rscript run_pipeline.R
```

or the steps on their own:

| Script | Output |
|---|---|
| `export-manuscript-stats.R` | `outputs/manuscript/`: every statistic in the text (`stats.tex`), table bodies (`tables/`), and figures (`figures/`, PDF and PNG). With `MANUSCRIPT_DIR` set, also copied to the manuscript repository's `generated/` folder |
| `export-software-citations.R` | R and package versions (`software.tex`), their citations (`software.bib`), and `session-info.txt` |
| `revision-analyses.R` | All analyses printed in manuscript order (`revision-analyses-output.txt` is a saved copy) |
| `power-analysis.Rmd` | The a priori power analyses |

Packages: `tidyverse`, `haven`, `qualtRics`, `psych`, `lme4`, `lmerTest`, `sandwich`, `lmtest`, `car`, `pwr`, `jsonlite`, `ggplot2`, `ggpubr`, and `rmarkdown` for the power analysis.

## Files

| File | What it is |
|---|---|
| `scripts/data-loading.R` | Loads and labels the survey exports |
| `scripts/data-cleaning.R` | Exclusions, imputation, recoding, standardization, and composite scores |
| `scripts/analysis-robustness.R` | HC3 regression models: condition effects, author-minus-content difference, covariate adjustment |
| `scripts/analysis-revision.R` | All other analysis functions (below) |
| `scripts/manuscript-stats.R` | Formats results and table rows for the manuscript |
| `scripts/manuscript-figures.R` | The violin plots (Figures 3, 4, 6, 7) |
| `scripts/preprocessing.py` | Anonymizes the raw Qualtrics downloads |

Earlier exploratory analyses (notebooks, structural equation models, partial correlations, moderation and regression models) are not part of this version. They remain available at the `v1-jdm` and `v2-chb` tags.

## Condition labels

The data keep the original labels. Experiment 1: `Low` = low, `Medium` = high, `High` = human. Experiment 2: `Low` = low, `High` = high.

## Functions in `scripts/analysis-revision.R`

| Manuscript | Function(s) |
|---|---|
| Missing values (4.2, 5.2.1) | `missing_item_summary()` |
| Welch t with Cohen's d and normal-approximation 95% CI | `welch_with_d()`, `cohens_d_ci()` |
| Author vs. content trust (mixed model; equivalent difference test) | `trust_type_mixed_model()`, `trust_difference_test()` |
| Tables 2 and 4 | `robustness_row()` |
| Composite-standardized comparison (Table B.3) | `composite_standardized_models()` |
| METI subscales (Table B.4) | `welch_with_d()`, `robust_condition_model()` |
| Equivalence tests, ±0.20 to ±0.50 (Table B.2) | `tost_d()`, `tost_table()` |
| Pooled and within-condition correlations, Williams test | `correlation_summary()`, `pearson_within()`, `williams_test()` |
| Preregistered secondary correlations (Table B.6) | `secondary_correlations()` |
| Balance across conditions | `balance_tests()` |
| Exclusions by condition (Table B.1) | `exclusion_flow_by_condition()` |
| Exp. 2 with preregistered exclusions only (Table B.5) | `build_study_data(exclusions = "preregistered")` |
| Student's t vs. Welch's t vs. ANOVA (Exp. 2) | `compare_two_group_tests()` |
| Retaining the excluded participant (Exp. 2) | `build_study_data(exclusions = "keep_openended")` |
| Complete-case analyses | `build_study_data(impute = FALSE)`, `complete_case_ids()` |
| Power (Exp. 2) | `interaction_power()` |
| Conversations (Exp. 2) | `chatbot_responses()`, `chatbot_response_tests()`, `conversation_flags()`, `conversation_turn_summary()`, `conversation_flag_tests()` |

## Notes

- Cohen's d uses the pooled SD; its 95% CI uses the large-sample normal approximation, SE = sqrt((n1 + n2)/(n1 n2) + d²/(2(n1 + n2))).
- Regression coefficients (B) are in standardized-item units, because each composite is the mean of z-scored items (SD < 1).
- Equivalence bounds are in pooled-SD units, with Welch standard errors and degrees of freedom.
- Within-condition correlations center each variable on its condition mean; df = N − k − 1 (k = number of conditions), and the Williams test uses df = N − k − 2.
- Frequency questions (GenAI use, similar media) enter balance tests and secondary correlations as 8-point ordinal scores, and covariate-adjusted models as 3-level factors.
- Response-level chatbot comparisons use participant-clustered standard errors with df = participants − 1. The conversation keyword rules were not validated against human coding.

# Revision analyses (CHB Reports)

Analyses added or changed for the Computers in Human Behavior Reports revision. They reuse the existing pipeline (`scripts/data-loading.R`, `scripts/data-cleaning.R`) and do not change any of its results.

Run from the project root:

```
Rscript revision-analyses.R
```

`run_pipeline.R` also runs it and writes the output to `rendered/revision-analyses-output.txt`. `revision-analyses-output.txt` in the project root is the output that the manuscript numbers were checked against.

New packages: `lme4`, `lmerTest`, `sandwich`, `lmtest`, `car`, `pwr`, `jsonlite`.

## Files

| File | What it is |
|---|---|
| `scripts/analysis-robustness.R` | HC3 regression models: condition effects, author-minus-content difference, covariate adjustment |
| `scripts/analysis-revision.R` | All other revision functions (below) |
| `revision-analyses.R` | Runs everything in manuscript order and prints the results |

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

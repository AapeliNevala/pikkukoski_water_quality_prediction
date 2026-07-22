# Pikkukoski Water Quality Prediction

A dashboard that nowcasts bacteria levels (Enterococci and *E. coli*) at
Pikkukoski beach, on the Vantaanjoki river in Helsinki, from daily rainfall
and river flow data.

Live dashboard: https://aapelinevala.github.io/pikkukoski_water_quality_prediction/

## What this project is

- A **Bayesian regression model** (brms/Stan, Student-t likelihood) that
  predicts `log(bacteria)` from trailing rainfall, same-day river flow, a
  within-season smooth trend, and a per-year effect — trained on June–August
  lab samples only (the beach's swimming season).
- A **nowcasting tool**: since official lab samples only come in every so
  often, the model fills the gap by predicting bacteria levels for every day
  that has rain/flow data, with a 90% credible interval and a posterior
  probability of being under the regulatory limit.
- A small **data pipeline** (`R/00_fetch_data.R` → `main.R` →
  `dashboard.qmd`, wired together by `pipeline.sh`) that pulls fresh rain
  (FMI) and river-flow (SYKE) data automatically, and prints new bacteria
  readings from the city's annual PDF report for manual review before they're
  added.
- A **hobby/experimental project**, run and updated irregularly.

## What this project is not

- **Not an official water-quality service.** It is not affiliated with the
  City of Helsinki, FMI, or SYKE, and is not a substitute for their official
  reports or advisories.
- **Not a real-time guarantee.** The dashboard is updated at random times,
  not on a fixed schedule you can rely on.
- **Not validated for safety decisions.** The model is experimental,
  trained on a small number of seasonal samples, and can be wrong — it
  should not be used as a reason to swim, or not to swim. Always prefer
  official advisories and your own judgement.

## Layout

```
R/00_fetch_data.R   fetch new rain/flow/bacteria data
main.R              build features, fit models, generate predictions
R/03_model.R        the brms model + divergence-triggered refit
dashboard.qmd       the Quarto dashboard
pipeline.sh         runs the whole thing end to end, then copies
                    dashboard.html -> index.html for GitHub Pages
```

## License

MIT — see [LICENSE](LICENSE).

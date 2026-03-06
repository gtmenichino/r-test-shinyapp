# ecorest HSI Explorer

A Shiny web application for exploring USFWS Habitat Suitability Index (HSI) models from the `ecorest` R package. Provides dynamic UI for 10 representative HSI models with real-time score calculation and suitability curve visualization.

## Project Structure

- `app.R` — Single-file Shiny app (UI + server)
- `r-test-shinyapp.Rproj` — RStudio project file

## Dependencies

- `shiny` — Web framework
- `ecorest` — HSI models and calculation functions (519 USFWS models)
- `viridis` — Color palette for plots and score display

Install: `install.packages(c("shiny", "ecorest", "viridis"))`

## Run

```r
shiny::runApp()
```

Or click "Run App" in RStudio.

## Conventions

- 2-space indentation (per .Rproj settings)
- Tidyverse style
- Single-file app structure for Posit Connect Cloud compatibility

## Deployment

Target: Posit Connect Cloud (via GitHub integration)

- Push to GitHub, connect repo in Posit Connect Cloud
- Uses `app.R` as entry point

## Key ecorest Functions

- `HSImodels` — Named list of SI curve data frames (paired columns: breakpoints + SI values)
- `HSImetadata` — Model metadata including species, equations, and SIV mappings
- `SIcalc(SI_df, input_vector)` — Interpolate individual SI values from breakpoint curves
- `HSIeqtn(model_name, SIV_vector, HSImetadata)` — Compute overall HSI from component equation

# ecorest HSI Explorer

A Shiny web application for exploring USFWS Habitat Suitability Index (HSI) models from the [`ecorest`](https://github.com/USACE/ecorest) R package.

**[Launch the app](https://019cc554-d901-f9bc-8610-17d53e759bce.share.connect.posit.cloud/)**

## Features

- **10 representative HSI models** selected from the 519 USFWS models in `ecorest`, spanning 2 to 7 habitat variables each
- **Interactive sliders** dynamically generated for each model's input variables with ecologically meaningful ranges
- **Real-time HSI score calculation** using the official `ecorest` equations and SI interpolation
- **Suitability curve visualization** showing individual SI curves with your current input values highlighted
- **Color-coded score display** using a viridis palette from red (poor) to green (optimal)

## Models included

| Model | Species | Variables |
|---|---|---|
| bairdssparrow | Baird's Sparrow | 2 |
| americanshadRiv | American Shad (Riverine) | 3 |
| barredowl | Barred Owl | 3 |
| baldeagleBreeding | Bald Eagle (Breeding) | 4 |
| easterncottontail | Eastern Cottontail | 4 |
| easternmeadowlark | Eastern Meadowlark | 5 |
| beltedkingfisherLenticConstWave | Belted Kingfisher | 6 |
| bluewingedtealBreeding | Blue-winged Teal (Breeding) | 6 |
| brooktroutLacAllLtoe15C | Brook Trout | 7 |
| bluegrouse | Blue Grouse | 7 |

## Run locally

### Prerequisites

```r
install.packages(c("shiny", "ecorest", "viridis"))
```

### Launch

```r
shiny::runApp()
```

Or open `r-test-shinyapp.Rproj` in RStudio and click **Run App**.

## Deployment

Deployed to [Posit Connect Cloud](https://connect.posit.cloud/) via GitHub integration.

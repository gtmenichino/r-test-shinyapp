library(shiny)
library(ecorest)
library(viridis)

# --- Selected Models (10 diverse species with varying complexity) ---
selected_models <- c(
  "Baird's Sparrow" = "bairdssparrow",
  "American Shad (River)" = "americanshadRiv",
  "Barred Owl" = "barredowl",
  "Bald Eagle (Breeding)" = "baldeagleBreeding",
  "Eastern Cottontail" = "easterncottontail",
  "Eastern Meadowlark" = "easternmeadowlark",
  "Belted Kingfisher (Lentic)" = "beltedkingfisherLenticConstWave",
  "Blue-winged Teal (Breeding)" = "bluewingedtealBreeding",
  "Brook Trout (Lake)" = "brooktroutLacAllLtoe15C",
  "Blue Grouse" = "bluegrouse"
)

# --- Pre-extract model info ---
model_info <- lapply(names(selected_models), function(display_name) {
  mod_name <- unname(selected_models[display_name])
  si_data <- HSImodels[[mod_name]]
  meta_row <- HSImetadata[HSImetadata$model == mod_name, ]
  n_pairs <- ncol(si_data) / 2
  valid_pairs <- Filter(function(i) !all(is.na(si_data[[2 * i - 1]])), seq_len(n_pairs))
  n_vars <- length(valid_pairs)

  vars <- lapply(valid_pairs, function(i) {
    x_col <- 2 * i - 1
    y_col <- 2 * i
    x_raw <- si_data[[x_col]]
    y_raw <- si_data[[y_col]]
    valid <- !is.na(x_raw) & !is.na(y_raw)
    x_vals <- as.numeric(x_raw[valid])
    y_vals <- as.numeric(y_raw[valid])

    label <- gsub("\\.", " ", names(si_data)[x_col])
    range_val <- max(x_vals) - min(x_vals)
    step <- if (range_val == 0) 0.01 else signif(range_val / 100, 1)

    list(
      label = label,
      x = x_vals,
      y = y_vals,
      min_val = min(x_vals),
      max_val = max(x_vals),
      mid_val = round((min(x_vals) + max(x_vals)) / 2, 2),
      step = step
    )
  })

  list(
    mod_name = mod_name,
    si_data = si_data,
    n_vars = n_vars,
    vars = vars,
    species = as.character(meta_row$species),
    equation = as.character(meta_row$Eqtn)
  )
})
names(model_info) <- names(selected_models)

# --- UI ---
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .hsi-score {
      font-size: 52px;
      font-weight: bold;
      text-align: center;
      padding: 8px;
    }
    .score-label {
      text-align: center;
      color: #666;
      margin-top: 4px;
    }
  "))),

  titlePanel("ecorest HSI Explorer"),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput("model_select", "Select HSI Model",
                  choices = names(selected_models)),
      tags$p(tags$em(textOutput("species_label", inline = TRUE)),
             style = "color: #666; margin-bottom: 15px;"),
      tags$hr(),
      uiOutput("dynamic_inputs"),
      tags$hr(),
      wellPanel(
        tags$div(class = "score-label", tags$strong("Overall HSI Score")),
        uiOutput("hsi_score_display"),
        tags$div(class = "score-label",
                 "Equation: ", textOutput("equation_label", inline = TRUE))
      )
    ),

    mainPanel(
      width = 8,
      tags$h4("Suitability Index Curves"),
      plotOutput("si_plots")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {

  current_model <- reactive({
    req(input$model_select)
    model_info[[input$model_select]]
  })

  output$species_label <- renderText(current_model()$species)
  output$equation_label <- renderText(current_model()$equation)

  # Dynamic slider inputs for the selected model
  output$dynamic_inputs <- renderUI({
    info <- current_model()
    sliders <- lapply(seq_len(info$n_vars), function(i) {
      v <- info$vars[[i]]
      sliderInput(
        inputId = paste0("var_", i),
        label = v$label,
        min = v$min_val,
        max = v$max_val,
        value = v$mid_val,
        step = v$step
      )
    })
    do.call(tagList, sliders)
  })

  # Collect current slider values
  input_values <- reactive({
    info <- current_model()
    vals <- lapply(seq_len(info$n_vars), function(i) {
      input[[paste0("var_", i)]]
    })
    req(all(!sapply(vals, is.null)))
    as.numeric(vals)
  })

  # Compute individual SI values via interpolation
  si_values <- reactive({
    info <- current_model()
    vals <- input_values()
    SIcalc(info$si_data, vals)
  })

  # Compute overall HSI score
  hsi_value <- reactive({
    si <- si_values()
    info <- current_model()
    result <- HSIeqtn(info$mod_name, si, HSImetadata)
    if (is.numeric(result)) round(result, 3) else NA
  })

  # HSI score display with viridis color
  output$hsi_score_display <- renderUI({
    hsi <- tryCatch(hsi_value(), error = function(e) NA)
    if (is.null(hsi) || is.na(hsi)) {
      tags$div(class = "hsi-score", style = "color: #ccc;", "--")
    } else {
      idx <- max(1, min(100, ceiling(hsi * 100)))
      col <- viridis(100)[idx]
      tags$div(class = "hsi-score", style = paste0("color: ", col, ";"),
               format(hsi, nsmall = 3))
    }
  })

  # Suitability index curve plots
  output$si_plots <- renderPlot({
    info <- current_model()
    si <- tryCatch(si_values(), error = function(e) NULL)
    vals <- tryCatch(input_values(), error = function(e) NULL)

    n <- info$n_vars
    ncols <- min(n, 3)
    nrows <- ceiling(n / ncols)
    par(mfrow = c(nrows, ncols), mar = c(4.5, 4, 3, 1))

    colors <- viridis(n)

    for (i in seq_len(n)) {
      v <- info$vars[[i]]

      plot(v$x, v$y, type = "o", lwd = 2.5, pch = 16, cex = 1.2,
           col = colors[i],
           xlab = v$label, ylab = "SI Value",
           main = paste0("SI Variable ", i),
           ylim = c(0, 1.05), las = 1)

      # Shaded area under curve
      polygon(c(v$x[1], v$x, v$x[length(v$x)]),
              c(0, v$y, 0),
              col = adjustcolor(colors[i], alpha.f = 0.15),
              border = NA)

      # Current input position marker
      if (!is.null(vals) && !is.null(si) &&
          length(vals) >= i && length(si) >= i && is.numeric(si[i])) {
        segments(vals[i], 0, vals[i], si[i], lty = 2, col = "gray50")
        segments(min(v$x), si[i], vals[i], si[i], lty = 2, col = "gray50")
        points(vals[i], si[i], pch = 18, cex = 3, col = "red")
        text(vals[i], si[i], labels = round(si[i], 2),
             pos = 3, col = "red", font = 2, cex = 1.1)
      }
    }
  }, height = function() {
    info <- model_info[[input$model_select]]
    if (is.null(info)) return(400)
    n <- info$n_vars
    nrows <- ceiling(n / min(n, 3))
    nrows * 300
  })
}

shinyApp(ui, server)

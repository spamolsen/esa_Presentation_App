library(shiny)
library(mgcv)
library(ggplot2)
library(plotly)
library(extrafont)

# Set up font
loadfonts(device = "win")
font_import(pattern = "Times New Roman")

# Custom theme with larger text sizes
plot_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 14),
    legend.position = c(0.95, 0.5),
    legend.direction = "vertical",
    legend.title.align = 0.5
  )

# Load models at startup
models <- reactiveVal(NULL)

observe({
  tryCatch({
    load("data/publication_models.RData")
    if (exists("bam_pan_allvar") && exists("bam_sweep_allvar")) {
      models(list(pan = bam_pan_allvar, sweep = bam_sweep_allvar))
    }
  }, error = function(e) {
    print(paste("Error loading models:", e$message))
  })
})

server <- function(input, output) {
  # Reactive expression to get the selected model
  selected_model <- reactive({
    model_list <- models()
    if (is.null(model_list)) return(NULL)
    
    if (input$model_type == "Pan Trap") {
      return(model_list$pan)
    } else {
      return(model_list$sweep)
    }
  })

  # Reactive expression to get diagnostics model
  diagnostics_model <- reactive({
    model_list <- models()
    if (is.null(model_list)) return(NULL)
    
    if (input$model_type_diag == "Pan Trap") {
      return(model_list$pan)
    } else {
      return(model_list$sweep)
    }
  })

  # Get the correct predictor variables for the model
  predictor_vars <- reactive({
    model <- selected_model()
    if (is.null(model)) return(NULL)
    
    # Initialize empty lists for different types of terms
    smooth_terms <- list()
    tensor_terms <- list()
    param_terms <- list()
    
    # Process smooth terms
    for (i in seq_along(model$smooth)) {
      term <- model$smooth[[i]]
      
      # Handle tensor products
      if (length(term$term) > 1) {
        # Combine tensor product terms into one name
        tensor_name <- paste(term$term, collapse = " + ")
        tensor_terms[[tensor_name]] <- term$label
      } else {
        # Handle regular smooth terms
        smooth_terms[[term$term]] <- term$label
      }
    }
    
    # Process parametric terms
    terms <- terms(model)
    var_names <- attr(terms, "term.labels")
    
    # Add parametric terms that aren't already in smooth terms
    for (var in var_names) {
      if (!any(grepl(var, c(names(smooth_terms), names(tensor_terms))))) {
        param_terms[[var]] <- var
      }
    }
    
    # Combine all terms into a data frame for display
    all_terms <- data.frame(
      type = c(rep("Smooth", length(smooth_terms)),
               rep("Tensor", length(tensor_terms)),
               rep("Parametric", length(param_terms))),
      label = c(names(smooth_terms), names(tensor_terms), names(param_terms)),
      model_label = c(unlist(smooth_terms), unlist(tensor_terms), unlist(param_terms))
    )
    
    # Sort by type and then by label first
    all_terms <- all_terms[order(all_terms$type, all_terms$label), ]
    
    # Filter out random effects and first two parametric terms
    random_effects <- c("sample_site", "sample_date", "year")
    # Get indices of parametric terms
    param_indices <- which(all_terms$type == "Parametric")
    # Remove first two parametric terms
    all_terms <- all_terms[-c(param_indices[1:2]), ]
    
    # Filter out random effects
    all_terms <- all_terms[!all_terms$label %in% random_effects, ]
    
    # Return the labels for the dropdown
    return(all_terms$label)
  })

  # Get the model term index for plotting
  get_term_index <- function(model, term_label) {
    # First check if it's a tensor product
    tensor_terms <- sapply(model$smooth, function(x) {
      if (length(x$term) > 1) {
        paste(x$term, collapse = " + ")
      } else {
        NA
      }
    })
    
    tensor_index <- which(tensor_terms == term_label)
    if (length(tensor_index) > 0) return(tensor_index)
    
    # Then check if it's a smooth term
    smooth_terms <- sapply(model$smooth, function(x) {
      if (length(x$term) == 1) {
        x$term[1]
      } else {
        NA
      }
    })
    
    smooth_index <- which(smooth_terms == term_label)
    if (length(smooth_index) > 0) return(smooth_index)
    
    # Finally check if it's a parametric term
    param_index <- which(names(model$coefficients) == term_label)
    if (length(param_index) > 0) return(param_index)
    
    return(NULL)
  }

  # Update variable selector based on model type
  output$variable_selector <- renderUI({
    predictors <- predictor_vars()
    if (is.null(predictors)) return(NULL)
    
    selectInput(
      "predictor",
      "Select Predictor Variable:",
      choices = predictors,
      selected = predictors[1]
    )
  })

  # Output to render PNG image
  # Create a reactive value to store the image class
  png_class <- reactiveVal("")
  
  output$png_image <- renderImage({
    # Return empty image by default
    empty_img <- list(src = "", width = 0, height = 0)
    
    model <- selected_model()
    predictor <- input$predictor
    
    if (is.null(model) || is.null(predictor)) {
      return(empty_img)
    }
    
    # Get the term index to determine what type of term this is
    term_index <- get_term_index(model, predictor)
    if (is.null(term_index)) {
      return(empty_img)
    }
    
    term <- model$smooth[[term_index]]
    
    # Determine which PNG to show
    png_file <- NULL
    model_type <- ifelse(input$model_type == "Pan Trap", "pt", "sn")
    
    if (length(term$term) > 1) {  # Tensor product (interaction)
      if (any(grepl("humidity", term$term))) {
        png_file <- file.path("www", paste0(model_type, "_hc.png"))
        png_class("humidity")
      } else if (any(grepl("temp", term$term))) {
        png_file <- file.path("www", paste0(model_type, "_temp.png"))
        png_class("")
      }
    } else if (grepl("days_since_origin_scaled", term$term[1])) {
      png_file <- file.path("www", paste0(model_type, "_ts.png"))
      png_class("trend")
    }
    
    if (is.null(png_file) || !file.exists(png_file)) {
      return(empty_img)
    }
    
    # Get the full normalized path
    png_file <- normalizePath(png_file, mustWork = FALSE)
    
    # Return a list containing the filename and alt text
    if (file.exists(png_file)) {
      list(
        src = png_file,
        alt = "PNG plot",
        width = "100%",
        height = "auto"
      )
    } else {
      empty_img
    }
  }, deleteFile = FALSE)
  
  # Render the PNG wrapper with the correct class
  output$png_wrapper <- renderUI({
    div(class = paste("png-wrapper", png_class()),
        imageOutput("png_image")
    )
  })

  # Main plot output (interactive)
  output$main_plot <- renderPlotly({
    model <- selected_model()
    predictor <- input$predictor
    
    if (is.null(model) || is.null(predictor)) return(NULL)
    
    # Get the term index
    term_index <- get_term_index(model, predictor)
    if (is.null(term_index)) return(NULL)
    # Check if we should display a PNG instead
    term_index <- get_term_index(model, predictor)
    if (!is.null(term_index)) {
      term <- model$smooth[[term_index]]
      
      # Hide plot for tensor products (interactions)
      if (length(term$term) > 1) {
        return(NULL)
      } else if (grepl("days_since_origin_scaled", term$term[1])) {
        # Hide plot for days_since_origin_scaled
        return(NULL)
      }
    }
    
    # Otherwise proceed with interactive plot
    
    # Determine if it's a tensor product
    tensor_terms <- sapply(model$smooth, function(x) {
      if (length(x$term) > 1) {
        paste(x$term, collapse = " + ")
      } else {
        NA
      }
    })
    
    is_tensor <- predictor %in% tensor_terms
    
    # Extract data for plotting
    term <- model$smooth[[term_index]]
    
    # Get the data for this term
    if (length(term$term) == 1) {
      # For single smooth terms
      # Get the variable name from the model
      var_name <- term$term[1]
      
      # Get the original variable name (without scaling prefix)
      original_var <- gsub("^s_", "", var_name)
      
      # Get both scaled and unscaled versions
      # First check if the original variable exists in the model
      if (original_var %in% names(model$model)) {
        x_scaled <- model$model[[var_name]]
        x_unscaled <- model$model[[original_var]]
      } else {
        # If original variable doesn't exist, use the scaled version
        x_scaled <- model$model[[var_name]]
        x_unscaled <- x_scaled  # Use scaled as unscaled if original not found
      }
      
      # Get the effect using the correct scaling
      if (original_var %in% c("windspeed", "temp_diff")) {
        # For windspeed and temp_diff, use unscaled version
        y <- model$coefficients[term$label] * x_unscaled
        rug_data <- x_unscaled
      } else {
        # For other variables, use scaled version
        y <- model$coefficients[term$label] * x_scaled
        rug_data <- x_scaled
      }
    } else {
      # For tensor products
      # Get original variable names
      original_var1 <- gsub("^s_", "", term$term[1])
      original_var2 <- gsub("^s_", "", term$term[2])
      
      # Get both scaled and unscaled versions
      x1_scaled <- model$model[[term$term[1]]]
      x2_scaled <- model$model[[term$term[2]]]
      x1_unscaled <- model$model[[original_var1]]
      x2_unscaled <- model$model[[original_var2]]
      
      # Create data frame with unscaled versions
      x <- data.frame(x1 = x1_unscaled, x2 = x2_unscaled)
      
      # Get the effect using the correct scaling
      if (original_var1 %in% c("windspeed", "temp_diff") || 
          original_var2 %in% c("windspeed", "temp_diff")) {
        # If either variable is windspeed or temp_diff, use unscaled version
        # First check if we have the original variables
        if (original_var1 %in% names(model$model) && 
            original_var2 %in% names(model$model)) {
          # Use original variables if they exist
          pred_data <- data.frame(
            original_var1 = x1_unscaled,
            original_var2 = x2_unscaled
          )
          y <- predict(model, type = "terms", newdata = pred_data)[, term$label]
        } else {
          # Fall back to using scaled variables if original not found
          y <- predict(model, type = "terms", newdata = x)[, term$label]
        }
      } else {
        # For other variables, use scaled version
        y <- predict(model, type = "terms")[, term$label]
      }
    }
    
    # Create ggplot
    if (length(term$term) == 1) {
      # Single smooth term plot
      response_var <- all.vars(formula(model))[1]
      y_actual <- model$model[[response_var]]
      
      # Determine if this is a parametric term
      is_parametric <- term$dim == 0
      
      if (is_parametric) {
        # For parametric terms, get the original variable name
        param_var <- gsub("^s_", "", term$term[1])
        # Get the original variable values
        x_values <- model$model[[param_var]]
        
        df <- data.frame(x = x_values, y = y, y_actual = y_actual)
        
        # Create base plot for parametric term
        p <- ggplot(df, aes(x = x)) +
          # Add points for parametric relationship
          geom_point(aes(y = y), color = "#8da0cb", size = 3) +
          geom_point(aes(y = y_actual), color = "#666666", size = 2, alpha = 0.7)
      } else {
        # For smooth terms, create a prediction data frame matching the original approach
        pred_data <- model$model[1, , drop = FALSE]
        pred_data <- pred_data[rep(1, 200), ]  # Use 200 points for smooth line
        
        # Create sequence of predictor values
        pred_seq <- seq(min(x_scaled, na.rm = TRUE), 
                       max(x_scaled, na.rm = TRUE), 
                       length.out = 200)
        
        # Set the predictor variable to our sequence
        pred_data[[term$term]] <- pred_seq
        
        # Get predictions on the response scale with standard errors
        preds <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)
        
        # Create prediction data frame
        pred_data <- model$model[1, , drop = FALSE]
        pred_data <- pred_data[rep(1, 200), ]
        
        # For days_since_origin_scaled, use the original days_since_origin values
        if (term$term[1] == "days_since_origin_scaled") {
          # Create sequence of days_since_origin values
          x_vals <- seq(min(model$model$days_since_origin, na.rm = TRUE),
                       max(model$model$days_since_origin, na.rm = TRUE),
                       length.out = 200)
          
          # Set the scaled variable in pred_data
          pred_data$days_since_origin_scaled <- scale(x_vals)
          
          # Get predictions for the smooth term
          preds <- predict(model, newdata = pred_data, type = "terms", se.fit = TRUE)
          
          # Create data frame for plotting
          plot_data <- data.frame(
            x_plot = x_vals,
            fit = preds$fit[, term$label],  # Get the specific term's prediction
            # Calculate approximate 95% CI on response scale
            upper = preds$fit[, term$label] + 1.96 * preds$se.fit[, term$label],
            lower = pmax(preds$fit[, term$label] - 1.96 * preds$se.fit[, term$label], 0)
          )
        } else {
          # For other variables, use unscaled values
          x_vals <- seq(min(x_unscaled, na.rm = TRUE),
                       max(x_unscaled, na.rm = TRUE),
                       length.out = 200)
          
          # Set the predictor variable in pred_data
          pred_data[[term$term[1]]] <- x_vals
          
          # Get predictions for the smooth term
          preds <- predict(model, newdata = pred_data, type = "terms", se.fit = TRUE)
          
          # Create data frame for plotting
          plot_data <- data.frame(
            x_plot = x_vals,
            fit = preds$fit[, term$label],  # Get the specific term's prediction
            # Calculate approximate 95% CI on response scale
            upper = preds$fit[, term$label] + 1.96 * preds$se.fit[, term$label],
            lower = pmax(preds$fit[, term$label] - 1.96 * preds$se.fit[, term$label], 0)
          )
        }
        
        # Create base plot with data points using appropriate x values
        if (term$term[1] == "days_since_origin_scaled") {
          # For days_since_origin_scaled, use days_since_origin for x-axis
          plot_data_points <- data.frame(
            x_plot = model$model$days_since_origin,
            y_actual = model$model[[all.vars(formula(model))[1]]]
          )
          rug_data_points <- data.frame(x_plot = model$model$days_since_origin)
        } else {
          # For other variables, use unscaled values
          plot_data_points <- data.frame(x_plot = x_unscaled, y_actual = y_actual)
          rug_data_points <- data.frame(x_plot = x_unscaled)
        }
        
        p <- ggplot() +
          # Add individual data points
          geom_point(
            data = plot_data_points,
            aes(x = x_plot, y = y_actual),
            color = "black", size = 1.5, alpha = 0.6
          ) +
          # Add GAM smooth line from the model
          geom_line(
            data = plot_data,
            aes(x = x_plot, y = fit),
            color = "royalblue", linewidth = 1.2
          ) +
          # Add confidence interval
          geom_ribbon(
            data = plot_data,
            aes(x = x_plot, ymin = lower, ymax = upper),
            fill = "royalblue", alpha = 0.2
          ) +
          # Add rug plot
          geom_rug(
            data = rug_data_points,
            aes(x = x_plot),
            sides = "b", color = "#666666", alpha = 0.5
          )
      }
      
      # Set axis labels and theme
      p <- p + 
        labs(title = if (term$term[1] == "days_since_origin_scaled") {
          "GAM Effect of Days Since Origin"
        } else {
          paste("GAM Effect of", original_var)
        },
        x = if (term$term[1] == "days_since_origin_scaled") {
          "Days Since Origin"
        } else {
          original_var
        },
        y = "Partial Effect") +
        theme_minimal(base_family = "Times New Roman") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 18, color = "black"),
          axis.title = element_text(size = 14, color = "black", face = "bold"),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.text = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          plot.margin = unit(c(1, 1, 1, 1), "cm")
        )
    } else {
      # Tensor product plot - create a grid for smooth surface
      grid_size <- 50
      x1_range <- range(x1_unscaled, na.rm = TRUE)
      x2_range <- range(x2_unscaled, na.rm = TRUE)
      
      # Create prediction grid using unscaled ranges
      grid_data <- expand.grid(
        x1 = seq(x1_range[1], x1_range[2], length.out = grid_size),
        x2 = seq(x2_range[1], x2_range[2], length.out = grid_size)  # Correct orientation
      )
      
      # Create a temporary data frame for prediction using the model's original data
      pred_data <- model$model[1, ]  # Get one row with right structure
      pred_data <- pred_data[rep(1, nrow(grid_data)), ]
      pred_data[[original_var1]] <- grid_data$x1  # Use unscaled variable names
      pred_data[[original_var2]] <- grid_data$x2
      
      # Predict values for the grid
      grid_data$y <- predict(model, newdata = pred_data, type = "terms")[, term$label]
      
      # Ensure grid_data has all required columns
      grid_data$x1_unscaled <- grid_data$x1
      grid_data$x2_unscaled <- grid_data$x2
      
      # Create contour plot with fill using original scales
      y_min <- min(grid_data$y, na.rm = TRUE)
      y_max <- max(grid_data$y, na.rm = TRUE)
      
      # Create a sequence of breaks that spans the actual range
      breaks <- seq(y_min, y_max, length.out = 10)
      rounded_breaks <- round(breaks, 2)
      
      p <- ggplot(grid_data, aes(x = x1_unscaled, y = x2_unscaled)) +
        geom_raster(aes(fill = y), interpolate = TRUE) +  # Raster plot for colors
        geom_contour(aes(z = y), color = "white", linewidth = 0.5, bins = 10) +  # Contour lines
        scale_fill_viridis_c(
          option = "B",
          direction = 1,  # Darker colors for lower values
          breaks = breaks,
          labels = rounded_breaks,
          limits = c(y_min, y_max)
        ) +
        scale_x_continuous(
          name = original_var1,
          breaks = pretty(grid_data$x1_unscaled, n = 5),
          labels = function(x) round(x, 2)
        ) +
        scale_y_continuous(
          name = original_var2,
          breaks = pretty(grid_data$x2_unscaled, n = 5),
          labels = function(x) round(x, 2)
        ) +
        labs(
          title = paste("Tensor Product of", original_var1, "and", original_var2),
          fill = "Predicted Effect"
        ) +
        guides(fill = guide_colorbar(
          title.position = "top",
          title.hjust = 0.5,
          barwidth = 1,
          barheight = 15,
          direction = "vertical"
        )) +
        plot_theme
      
      # Set axis labels and theme
      p <- p + 
        scale_x_continuous(
          breaks = pretty(grid_data$x1_unscaled, n = 5),
          labels = function(x) round(x, 2)
        ) +
        scale_y_continuous(
          breaks = pretty(grid_data$x2_unscaled, n = 5),
          labels = function(x) round(x, 2)
        ) +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              panel.background = element_rect(fill = "#f5f5f5", color = NA)) +
        labs(title = paste("Tensor Product of", original_var1, "and", original_var2),
             x = original_var1,
             y = original_var2,
             fill = "Effect")
    }
    
    # For tensor products, add a subtitle
    if (is_tensor) {
      p <- p + annotate("text",
                       x = Inf, y = Inf,
                       label = paste("Terms:", paste(term$term, collapse = ", ")),
                       hjust = 1, vjust = 1,
                       size = 3)
    }
    
    # Convert to plotly
    ggplotly(p, tooltip = if (length(term$term) == 1) c("x", "y") else c("x1", "x2", "y")) %>% 
      layout(
        hovermode = "closest",
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
      )
  })

  # Diagnostics images
  output$diagnostics_plot <- renderUI({
    model_type <- input$model_type_diag
    
    if (is.null(model_type)) return(NULL)
    
    # Determine which images to show based on model type
    if (model_type == "Pan Trap") {
      img1 <- "pt_1.png"
      img2 <- "pt_2.png"
    } else {
      img1 <- "sn_1.png"
      img2 <- "sn_2.png"
    }
    
    # Create a vertical layout with both images
    tagList(
      div(style = "margin-bottom: 20px;",
          img(src = img1, style = "width: 100%; height: auto;")
      ),
      div(
        img(src = img2, style = "width: 100%; height: auto;")
      )
    )
  })
}

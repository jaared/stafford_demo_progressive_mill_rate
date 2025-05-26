# app.R

library(shiny)
library(tidyverse)
library(scales)  # For dollar formatting in the table
library(patchwork) # For combining ggplot objects

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Progressive Property Taxation Model"),
  
  # Overall App Description
  fluidRow(
    column(12,
           p("This application models a hypothetical progressive property taxation system, comparing it to a traditional flat mill rate system. It uses a **simulated dataset** based on hypothetical property values from Stafford, CT, to illustrate the potential impact on different home values."), # Emphasized "simulated dataset"
           p("The goal is to demonstrate how a progressive tax structure could shift the tax burden while maintaining overall revenue neutrality for the municipality. Explore the impact by adjusting the parameter below and observing the changes in the plots and table.")
    )
  ),
  hr(), # Separator
  
  # Sidebar with a slider input for the beta parameter
  sidebarLayout(
    sidebarPanel(
      sliderInput("beta_param",
                  "Log Function Beta Parameter:",
                  min = 0,
                  max = 10,
                  value = 1,
                  step = 0.1),
      hr(),
      p(strong("How the Beta Parameter Works:")),
      p("This parameter controls the 'progressivity' of the tax system. A higher beta value increases the effective mill rate more steeply for higher-valued properties, thereby shifting more of the tax burden towards them. Conversely, lower beta values (approaching 0) make the system behave more like a traditional flat tax."),
      p("Adjust the slider to see how different levels of progressivity affect tax outcomes.")
    ),
    
    # Show plots and table
    mainPanel(
      # Description for Mill Rate Plot
      p(strong("Effective Mill Rate under Progressive Tax:")),
      p("This plot illustrates how the effective mill rate (taxes due per $1,000 of assessed value) changes with a property's appraised value under the progressive model (blue line) compared to the current flat mill rate (red dashed line). Observe how the blue line's curve changes with the beta parameter."),
      h3("Effective Mill Rate under Progressive Tax"),
      plotOutput("millRatePlot"),
      hr(), # Separator
      
      # Description for Taxes Due Comparison Plot
      p(strong("Taxes Due Comparison:")),
      p("This plot shows the total annual taxes due for properties across different appraised values, comparing the progressive tax model (blue line) with the current flat mill rate model (red line). Notice how the progressive tax can be lower for smaller homes and higher for larger ones, while ensuring the town collects the same total revenue."),
      h3("Taxes Due Comparison"),
      plotOutput("taxesDuePlot"),
      hr(), # Separator
      
      # Description for Tax Savings Plot
      p(strong("Change in Taxes (Progressive vs. Flat):")),
      p("This plot highlights the net difference in taxes (Progressive Tax - Flat Mill Tax) for properties across the value spectrum. A negative value indicates tax savings under the progressive system, while a positive value means higher taxes compared to the flat rate. The point where the line crosses zero indicates the property value at which taxes are approximately the same under both systems."),
      h3("Change in Taxes (Progressive vs. Flat)"),
      plotOutput("taxSavingsPlot"),
      hr(), # Separator
      
      # Description for Summary Table
      p(strong("Taxation Summary for Common Home Valuations:")),
      p("The table below provides a detailed summary of taxes for specific common property valuations. This allows for a direct numerical comparison of annual taxes and the tax increase/decrease under the traditional flat mill rates versus the proposed progressive system."),
      h3("Taxation Summary for Common Home Valuations"),
      tableOutput("tax_summary_table")
    )
  )
)

# Define server logic required to draw plots
server <- function(input, output) {
  
  # Define helper functions within the server scope but outside reactive expressions.
  # Functions now explicitly use 'assessed_value' for clarity
  calculate_progressive_term <- function(assessed_value, beta_param) {
    # Adding +1 to assessed_value inside log to avoid log(0) and ensure positive log results.
    # beta_param now acts as an exponent, directly controlling the steepness of progressivity.
    assessed_value * (log(assessed_value + 1)) ^ beta_param
  }
  
  f_progressive_adjustment <- function(assessed_value_vector, target_revenue, beta_param) {
    sum_of_progressive_terms <- sum(calculate_progressive_term(assessed_value_vector, beta_param))
    if (sum_of_progressive_terms == 0) {
      return(1e-9) # Return a very small number to avoid division by zero
    }
    target_revenue / sum_of_progressive_terms
  }
  
  # --- Model parameters ---
  mill_multiplier <- 1000
  
  # Fixed mill rates for comparison
  mill_rate_24_25 <- 38.59
  mill_rate_25_26 <- 38.71
  
  # --- Data Import ---
  # Read data from CSV file
  # Assuming 'fake_stafford_ct_property_values.csv' is in the same directory as app.R
  stafford_data_raw <- read_csv(file.path('data', "fake_stafford_ct_property_values.csv"), show_col_types = FALSE)
  
  # Filter out any rows with non-positive Appraised_Value or Assessed_Value
  # These values would cause issues with log transformations or division by zero,
  # leading to NaN/Inf in calculations and plot errors.
  stafford_data_filtered <- stafford_data_raw %>%
    filter(Appraised_Value > 0, Assessed_Value > 0)
  
  # Calculate tax_valuation_ratio from the FILTERED imported data
  tax_valuation_ratio <- sum(stafford_data_filtered$Assessed_Value) / sum(stafford_data_filtered$Appraised_Value)
  
  # target_revenue now calculated from the sum of Assessed_Value and mill_rate_25_26
  target_revenue <- sum(stafford_data_filtered$Assessed_Value) * (mill_rate_25_26 / mill_multiplier)
  
  # Initial static dataframe for PLOTTING (base data from imported values)
  # Uses 'appraised_value' for plotting x-axis and 'assessed_value' for calculations
  initial_plot_df <- stafford_data_filtered %>% # Use filtered data here
    rename(appraised_value = Appraised_Value, assessed_value = Assessed_Value) %>%
    arrange(appraised_value) # Ensure initial sorting by appraised_value
  
  # Reactive value for the PLOTTING data frame
  reactive_plot_df <- reactiveVal(initial_plot_df)
  
  # Define the specific assessed values for the SUMMARY TABLE
  table_assessed_values_base <- seq(100000, 500000, by = 5000)
  
  # Initial static dataframe for TABLE (base data for specific assessed values)
  initial_table_df <- data.frame(
    assessed_value = table_assessed_values_base # This is the Assessed Value
  ) %>%
    mutate(
      # Calculate Appraised Value using the derived tax_valuation_ratio
      appraised_value = assessed_value / tax_valuation_ratio, # Renamed from market_value
      # Calculate taxes based on fixed mill rates
      taxes_24_25 = assessed_value * (mill_rate_24_25 / mill_multiplier),
      taxes_25_26 = assessed_value * (mill_rate_25_26 / mill_multiplier),
      # Calculate increase for fixed mill rates
      increase_fixed_mill = taxes_25_26 - taxes_24_25
    ) %>%
    arrange(assessed_value) # Ensure initial sorting by assessed_value
  
  # Reactive value for the TABLE data frame
  reactive_table_df <- reactiveVal(initial_table_df)
  
  # flat_mill_rate_val now uses the specified mill_rate_25_26
  # This serves as a reference mill rate for comparison on plots and in table.
  flat_mill_rate_val <- mill_rate_25_26 / mill_multiplier
  
  
  # Observe changes in input$beta_param and update BOTH data frames
  observeEvent(input$beta_param, {
    
    beta_val <- input$beta_param
    
    # --- Update the PLOTTING data frame (for scatter plots) ---
    current_plot_df <- initial_plot_df
    
    # Calculate adjustment term based on *assessed values* from imported data
    # This ensures the total progressive tax revenue equals target_revenue from the imported grand list.
    plot_adjustment_term <- f_progressive_adjustment(current_plot_df$assessed_value, target_revenue, beta_val)
    
    current_plot_df <- current_plot_df %>%
      mutate(
        adjustment_term = plot_adjustment_term,
        beta_val = beta_val) %>%
      mutate(
        progressive_taxes = adjustment_term * calculate_progressive_term(assessed_value, beta_val),
        progressive_tax_rate = progressive_taxes / assessed_value,
        progressive_mill_rate = mill_multiplier * progressive_tax_rate,
        # taxes_at_flat_mill now uses the new flat_mill_rate_val
        taxes_at_flat_mill = assessed_value * flat_mill_rate_val,
        tax_savings = progressive_taxes - taxes_at_flat_mill
      ) %>%
      arrange(appraised_value) # Ensure data is sorted after all mutations for consistent plotting
    reactive_plot_df(current_plot_df) # Update the reactive value
    
    
    # --- Update the TABLE data frame (for summary table) ---
    current_table_df <- initial_table_df
    
    # IMPORTANT: Use the SAME adjustment_term calculated from the SIMULATED data
    # to maintain consistency in overall revenue targeting.
    table_adjustment_term <- plot_adjustment_term
    
    current_table_df <- current_table_df %>%
      mutate(
        adjustment_term = table_adjustment_term,
        beta_val = beta_val) %>%
      mutate(
        progressive_taxes = adjustment_term * calculate_progressive_term(assessed_value, beta_val),
        progressive_tax_rate = progressive_taxes / assessed_value,
        progressive_mill_rate = mill_multiplier * progressive_tax_rate,
        taxes_at_flat_mill = assessed_value * flat_mill_rate_val, # Re-calculate to be explicit
        tax_savings = progressive_taxes - taxes_at_flat_mill # This is the (Progressive - Flat) from the model
      ) %>%
      arrange(assessed_value) # Ensure data is sorted after all mutations for consistent plotting
    reactive_table_df(current_table_df) # Update the reactive value for the table
  })
  
  
  # Render Plot 1: Effective Mill Rate
  output$millRatePlot <- renderPlot({
    # Access the reactive dataframe for plots
    data <- reactive_plot_df() %>%
      arrange(appraised_value) %>%
      # Filter out NA/NaN values in the relevant columns before plotting
      filter(!is.na(progressive_mill_rate), !is.nan(progressive_mill_rate),
             !is.na(appraised_value), !is.nan(appraised_value))
    req(nrow(data) > 0)
    
    # Main plot (line only)
    gg_main <- ggplot(data, aes(x = appraised_value, y = progressive_mill_rate)) +
      geom_line(color = "steelblue", linewidth = 1.2) +
      xlab('Appraised Value ($)') +
      ylab(paste0('Effective mill rate (per $', mill_multiplier, ')')) +
      geom_hline(yintercept = flat_mill_rate_val * mill_multiplier, lty = 2, color = 'red', linewidth = 1) +
      scale_x_continuous(labels = scales::dollar_format(),
                         breaks = scales::pretty_breaks(n = 8)) + # Removed limits
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01), # Format as number with 2 decimal places
                         breaks = scales::pretty_breaks(n = 8)) + # Removed limits
      ggtitle('Effective Mill Rate under Progressive Tax',
              paste0('vs flat ', round(flat_mill_rate_val * mill_multiplier, 2), ' mill')) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
      )
    
    # Marginal histogram (x-axis)
    gg_hist_x <- ggplot(data, aes(x = appraised_value)) +
      geom_histogram(binwidth = (max(data$appraised_value) - min(data$appraised_value)) / 50, # Dynamic binwidth
                     fill = "lightblue", color = "darkblue", alpha = 0.7) +
      scale_x_continuous() + # Removed limits
      theme_void() +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
      )
    
    # Combine plots using patchwork
    combined_plot <- gg_hist_x / gg_main +
      plot_layout(heights = c(1, 4))
    
    print(combined_plot)
  })
  
  # Render Plot 2: Taxes Due Comparison
  output$taxesDuePlot <- renderPlot({
    # Access the reactive dataframe for plots
    data <- reactive_plot_df() %>%
      arrange(appraised_value) %>%
      # Filter out NA/NaN values in the relevant columns before plotting
      filter(!is.na(progressive_taxes), !is.nan(progressive_taxes),
             !is.na(taxes_at_flat_mill), !is.nan(taxes_at_flat_mill),
             !is.na(appraised_value), !is.nan(appraised_value))
    req(nrow(data) > 0)
    
    # Main plot (line only - removed geom_point)
    gg_main <- data %>%
      select(appraised_value, progressive_taxes, taxes_at_flat_mill) %>%
      pivot_longer(cols = c(progressive_taxes, taxes_at_flat_mill), names_to = "tax_type", values_to = "taxes_due") %>%
      ggplot(aes(x = appraised_value, y = taxes_due, color = tax_type)) +
      geom_line(linewidth = 1) + # Removed geom_point()
      scale_color_manual(values = c("progressive_taxes" = "blue", "taxes_at_flat_mill" = "red"),
                         labels = c("progressive_taxes" = "Progressive Tax", "taxes_at_flat_mill" = "Flat Mill Tax")) +
      xlab('Appraised Value ($)') +
      ylab('Taxes due ($)') +
      scale_x_continuous(labels = scales::dollar_format(), breaks = scales::pretty_breaks(n = 8)) + # Removed limits
      scale_y_continuous(labels = scales::dollar_format(), # Format as dollar
                         breaks = scales::pretty_breaks(n = 8)) + # Removed limits
      ggtitle('Taxes due',
              paste0('comparing progressive tax with ', round(flat_mill_rate_val * mill_multiplier, 2), ' mill')) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
      )
    
    # Marginal histogram (x-axis)
    gg_hist_x <- ggplot(data, aes(x = appraised_value)) +
      geom_histogram(binwidth = (max(data$appraised_value) - min(data$appraised_value)) / 50,
                     fill = "lightblue", color = "darkblue", alpha = 0.7) +
      scale_x_continuous() + # Removed limits
      theme_void() +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
      )
    
    # Combine plots using patchwork
    combined_plot <- gg_hist_x / gg_main +
      plot_layout(heights = c(1, 4))
    
    print(combined_plot)
  })
  
  # Render Plot 3: Change in Taxes
  output$taxSavingsPlot <- renderPlot({
    # Access the reactive dataframe for plots
    data <- reactive_plot_df() %>%
      arrange(appraised_value) %>%
      # Filter out NA/NaN values in the relevant columns before plotting
      filter(!is.na(tax_savings), !is.nan(tax_savings),
             !is.na(appraised_value), !is.nan(appraised_value))
    req(nrow(data) > 0)
    
    # Main plot (line only - removed geom_point)
    gg_main <- ggplot(data, aes(x = appraised_value, y = tax_savings)) +
      geom_line(linewidth = 1, color = "darkgreen") +
      xlab('Appraised Value ($)') +
      ylab('Change in taxes (Progressive - Flat) ($)') +
      geom_hline(yintercept = 0, lty = 2, color = 'grey', linewidth = 1) +
      scale_x_continuous(labels = scales::dollar_format(), breaks = scales::pretty_breaks(n = 8)) + # Removed limits
      scale_y_continuous(labels = scales::dollar_format(), # Format as dollar
                         breaks = scales::pretty_breaks(n = 8)) + # Removed limits
      ggtitle('Change in Taxes (Progressive vs. Flat)') +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
      )
    
    # Marginal histogram (x-axis)
    gg_hist_x <- ggplot(data, aes(x = appraised_value)) +
      geom_histogram(binwidth = (max(data$appraised_value) - min(data$appraised_value)) / 50,
                     fill = "lightcoral", color = "darkred", alpha = 0.7) +
      scale_x_continuous() + # Removed limits
      theme_void() +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
      )
    
    # Combine plots using patchwork
    combined_plot <- gg_hist_x / gg_main +
      plot_layout(heights = c(1, 4))
    
    print(combined_plot)
  })
  
  # Render the summary table with updated columns
  output$tax_summary_table <- renderTable({
    data <- reactive_table_df() # Access the reactive dataframe for the table
    req(nrow(data) > 0)
    
    data %>%
      select(
        `Appraised Value` = appraised_value,
        `Assessed Value` = assessed_value,
        `Annual Taxes @ Mill Rate 24-25 (38.59)` = taxes_24_25,
        `Annual Taxes @ Mill Rate 25-26 (38.71)` = taxes_25_26,
        `Tax Increase (2024-25 to 2025-26)` = increase_fixed_mill,
        `Annual Taxes @ Progressive Mill Rate 25-26` = progressive_taxes,
        `Progressive Mill Rate 25-26` = progressive_mill_rate, # NEW COLUMN
        `Annual Tax Difference (Progressive vs. Flat 25-26)` = tax_savings
      ) %>%
      mutate(
        `Appraised Value` = scales::dollar(`Appraised Value`, accuracy = 1),
        `Assessed Value` = scales::dollar(`Assessed Value`, accuracy = 1),
        `Annual Taxes @ Mill Rate 24-25 (38.59)` = scales::dollar(`Annual Taxes @ Mill Rate 24-25 (38.59)`, accuracy = 0.01),
        `Annual Taxes @ Mill Rate 25-26 (38.71)` = scales::dollar(`Annual Taxes @ Mill Rate 25-26 (38.71)`, accuracy = 0.01),
        `Tax Increase (2024-25 to 2025-26)` = scales::dollar(`Tax Increase (2024-25 to 2025-26)`, accuracy = 0.01),
        `Annual Taxes @ Progressive Mill Rate 25-26` = scales::dollar(`Annual Taxes @ Progressive Mill Rate 25-26`, accuracy = 0.01),
        `Progressive Mill Rate 25-26` = scales::number(`Progressive Mill Rate 25-26`, accuracy = 0.01), # Format as number with 2 decimal places
        `Annual Tax Difference (Progressive vs. Flat 25-26)` = scales::dollar(`Annual Tax Difference (Progressive vs. Flat 25-26)`, accuracy = 0.01)
      )
  }, digits = 2)
}

# Run the application
shinyApp(ui = ui, server = server)

# app.R

library(shiny)
library(tidyverse)
library(ggExtra) # For marginal histograms

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Progressive Property Taxation Model"),
  
  # Sidebar with a slider input for the beta parameter
  sidebarLayout(
    sidebarPanel(
      sliderInput("beta_param",
                  "Log Function Beta Parameter:",
                  min = -5,
                  max = 5,
                  value = 0,
                  step = 0.1),
      hr(),
      p("Adjust the beta parameter to see how it affects the progressive tax system's impact on different home values."),
      p("A higher beta value generally increases the progressive nature of the tax.")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Effective Mill Rate under Progressive Tax"),
      plotOutput("millRatePlot"),
      h3("Taxes Due Comparison"),
      plotOutput("taxesDuePlot"),
      h3("Change in Taxes (Progressive vs. Flat)"),
      plotOutput("taxSavingsPlot")
    )
  )
)

# Define server logic required to draw plots
server <- function(input, output) {
  
  # Reactive expression to perform calculations based on input$beta_param
  tax_data <- reactive({
    beta_val <- input$beta_param # Get the beta value from the slider
    
    # Model parameters (can also be made interactive if desired)
    households <- 4569 * 2 # * 2 to account for businesses
    median_home_price <- 249900
    tax_valuation <- .7
    home_price_standard_deviation <- 35000
    target_revenue <- 47000000
    
    set.seed(1) # Ensure reproducibility
    home_values <- rnorm(
      n = households,
      mean = median_home_price,
      sd = home_price_standard_deviation
    )
    
    df <- data.frame(home_values = home_values) %>%
      mutate(home_tax_value = tax_valuation * home_values) %>%
      arrange(home_values)
    
    # Calculate flat mill rate
    flat_mill_rate <- target_revenue / sum(df$home_tax_value)
    
    # Function to calculate the adjustment term for progressive tax
    # This function now includes the beta parameter
    f_progressive <- function(home_tax_value, target_revenue, beta_param) {
      target_revenue / sum(home_tax_value * (log(home_tax_value) + beta_param))
    }
    
    # Calculate adjustment term based on the current beta value
    adjustment_term <- f_progressive(df$home_tax_value, target_revenue, beta_val)
    
    # Calculate progressive taxes and related metrics
    df_final <- df %>%
      mutate(
        # Apply the beta parameter in the progressive tax calculation
        progressive_taxes = adjustment_term * home_tax_value * (log(home_tax_value) + beta_val),
        progressive_tax_rate = progressive_taxes / home_tax_value,
        progressive_mill_rate = 1000 * progressive_tax_rate, # Multiplied by 1000
        taxes_at_flat_mill = home_tax_value * flat_mill_rate,
        tax_savings = progressive_taxes - taxes_at_flat_mill
      )
    
    # Ensure that target revenue is met (for verification, not used in plotting directly)
    # sum(df_final$progressive_taxes) # Should be close to target_revenue
    
    list(df = df_final, flat_mill_rate = flat_mill_rate, target_revenue = target_revenue)
  })
  
  # Render Plot 1: Effective Mill Rate
  output$millRatePlot <- renderPlot({
    data <- tax_data()$df
    flat_mill <- tax_data()$flat_mill_rate
    
    gg <- ggplot(data, aes(x = home_values, y = progressive_mill_rate)) +
      geom_line() +
      geom_point(alpha = 0.5) +
      xlab('Assessed home value ($)') +
      ylab('Effective mill rate (per $1000)') +
      geom_hline(yintercept = flat_mill * 1000, lty = 2, color = 'red') + # Multiplied by 1000
      scale_x_continuous(labels = scales::dollar_format(), breaks = seq(0, max(data$home_values), by = 50000)) +
      ggtitle('Effective Mill Rate under progressive tax',
              paste0('vs flat ', round(flat_mill * 1000, 2), ' mill')) + # Multiplied by 1000
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability
    
    ggMarginal(gg, type = 'histogram', margins = 'x') # Only x-axis histogram for clarity
  })
  
  # Render Plot 2: Taxes Due Comparison
  output$taxesDuePlot <- renderPlot({
    data <- tax_data()$df
    flat_mill <- tax_data()$flat_mill_rate
    
    gg <- data %>%
      select(home_values, progressive_taxes, taxes_at_flat_mill) %>%
      pivot_longer(cols = c(progressive_taxes, taxes_at_flat_mill), names_to = "tax_type", values_to = "taxes_due") %>%
      ggplot(aes(x = home_values, y = taxes_due, color = tax_type)) +
      geom_point(alpha = 0.5) +
      geom_line() +
      scale_color_manual(values = c("progressive_taxes" = "blue", "taxes_at_flat_mill" = "red"),
                         labels = c("progressive_taxes" = "Progressive Tax", "taxes_at_flat_mill" = "Flat Mill Tax")) +
      xlab('Assessed home value ($)') +
      ylab('Taxes due ($)') +
      scale_x_continuous(labels = scales::dollar_format(), breaks = seq(0, max(data$home_values), by = 50000)) +
      scale_y_continuous(labels = scales::dollar_format()) +
      ggtitle('Taxes due',
              paste0('comparing progressive tax with ', round(flat_mill * 1000, 2), ' mill')) + # Multiplied by 1000
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggMarginal(gg, type = 'histogram', margins = 'x') # Only x-axis histogram for clarity
  })
  
  # Render Plot 3: Change in Taxes
  output$taxSavingsPlot <- renderPlot({
    data <- tax_data()$df
    
    gg <- ggplot(data, aes(x = home_values, y = tax_savings)) +
      geom_line() +
      geom_point(alpha = 0.5) +
      xlab('Assessed home value ($)') +
      ylab('Change in taxes (Progressive - Flat) ($)') +
      geom_hline(yintercept = 0, lty = 2, color = 'grey') +
      scale_x_continuous(labels = scales::dollar_format(), breaks = seq(0, max(data$home_values), by = 50000)) +
      scale_y_continuous(labels = scales::dollar_format()) +
      ggtitle('Change in Taxes (Progressive vs. Flat)') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggMarginal(gg, type = 'histogram', margins = 'x') # Only x-axis histogram for clarity
  })
}

# Run the application
shinyApp(ui = ui, server = server)
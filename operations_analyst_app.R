library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(DT)
library(scales) # Make sure this is installed for dollar formatting

# Load and preprocess data
payment_data <- read.csv("Operations_analyst_data.csv")
payment_data <- payment_data %>%
    mutate(
        day = as_date(day),
        entity = as_factor(entity),
        product = as_factor(product),
        price_tier = as_factor(price_tier),
        anticipation_method = as_factor(anticipation_method),
        payment_method = as_factor(payment_method),
        installments = as_factor(installments)
    )

# Define UI
ui <- page_navbar(
    title = "CloudWalk Transaction Analysis",
    theme = bs_theme(bootswatch = "flatly"),
    
    # TPV Analysis Tab
    nav_panel("TPV Analysis",
              page_sidebar(
                  sidebar = sidebar(
                      width = 300,
                      dateRangeInput("date_range_tpv", "Select Date Range:",
                                     start = min(payment_data$day),
                                     end = max(payment_data$day)),
                      
                      selectInput("tpv_groupby", "Group TPV by:",
                                  choices = list(
                                      "Product" = "product",
                                      "Entity" = "entity",
                                      "Payment Method" = "payment_method"
                                  ),
                                  selected = "product")
                  ),
                  
                  card(
                      card_header("Total Payment Volume (TPV) Analysis"),
                      plotOutput("tpv_plot", height = "500px")
                  )
              )
    ),
    
    # Average Ticket Analysis Tab
    nav_panel("Average Ticket",
              page_sidebar(
                  sidebar = sidebar(
                      width = 300,
                      dateRangeInput("date_range_ticket", "Select Date Range:",
                                     start = min(payment_data$day),
                                     end = max(payment_data$day)),
                      
                      selectInput("ticket_groupby", "Group Average Ticket by:",
                                  choices = list(
                                      "Product" = "product",
                                      "Entity" = "entity",
                                      "Payment Method" = "payment_method"
                                  ),
                                  selected = "product")
                  ),
                  
                  card(
                      card_header("Average Ticket Size Analysis"),
                      plotOutput("ticket_plot", height = "500px")
                  )
              )
    ),
    
    # Transaction Volume Tab
    nav_panel("Transaction Volume",
              page_sidebar(
                  sidebar = sidebar(
                      width = 300,
                      dateRangeInput("date_range_volume", "Select Date Range:",
                                     start = min(payment_data$day),
                                     end = max(payment_data$day)),
                      
                      selectInput("volume_groupby", "Group Transaction Volume by:",
                                  choices = list(
                                      "Installments" = "installments",
                                      "Price Tier" = "price_tier"
                                  ),
                                  selected = "installments")
                  ),
                  
                  card(
                      card_header("Transaction Volume Analysis"),
                      plotOutput("volume_plot", height = "500px")
                  )
              )
    ),
    
    # Trends Tab (NEW)
    nav_panel("Trends",
              page_sidebar(
                  sidebar = sidebar(
                      width = 300,
                      dateRangeInput("date_range_trends", "Select Date Range:",
                                     start = min(payment_data$day),
                                     end = max(payment_data$day)),
                      
                      selectInput("trend_metric", "Select Metric:",
                                  choices = c("TPV", "Average Ticket", "Transaction Volume"),
                                  selected = "TPV"),
                      
                      radioButtons("trend_granularity", "Select Granularity:",
                                   choices = c("Monthly", "Weekly"),
                                   selected = "Monthly")
                  ),
                  
                  card(
                      card_header("Trends Over Time"),
                      plotOutput("trend_plot", height = "500px")
                  )
              )
    ),
    
    # Methods Analysis Tab
    nav_panel("Methods Analysis",
              page_sidebar(
                  sidebar = sidebar(
                      width = 300,
                      dateRangeInput("date_range_methods", "Select Date Range:",
                                     start = min(payment_data$day),
                                     end = max(payment_data$day)),
                      
                      selectInput("entity_filter", "Filter by Entity:",
                                  choices = c("All", levels(payment_data$entity)),
                                  selected = "All")
                  ),
                  
                  card(
                      card_header("Anticipation Method Usage by Entity"),
                      plotOutput("methods_plot", height = "500px")
                  )
              )
    ),
    
    # Summary Dashboard Tab
    nav_panel("Dashboard",
              layout_columns(
                  col_widths = c(6, 6),
                  card(
                      card_header("Key Metrics"),
                      verbatimTextOutput("summary_stats")
                  ),
                  card(
                      card_header("Top Performers"),
                      tableOutput("top_performers")
                  )
              ),
              
              layout_columns(
                  col_widths = c(12),
                  card(
                      card_header("Data Sample"),
                      DTOutput("data_table")
                  )
              )
    )
)

# Define Server
server <- function(input, output) {
    
    # Reactive data functions
    filtered_data_tpv <- reactive({
        payment_data %>%
            filter(day >= input$date_range_tpv[1] & day <= input$date_range_tpv[2])
    })
    
    filtered_data_ticket <- reactive({
        payment_data %>%
            filter(day >= input$date_range_ticket[1] & day <= input$date_range_ticket[2])
    })
    
    filtered_data_volume <- reactive({
        payment_data %>%
            filter(day >= input$date_range_volume[1] & day <= input$date_range_volume[2])
    })
    
    filtered_data_methods <- reactive({
        data <- payment_data %>%
            filter(day >= input$date_range_methods[1] & day <= input$date_range_methods[2])
        
        if(input$entity_filter != "All") {
            data <- data %>% filter(entity == input$entity_filter)
        }
        return(data)
    })
    
    filtered_data_trends <- reactive({
        payment_data %>%
            filter(day >= input$date_range_trends[1] & day <= input$date_range_trends[2])
    })
    
    # TPV Plot (updated)
    output$tpv_plot <- renderPlot({
        data <- filtered_data_tpv()
        
        plot_data <- data %>%
            group_by(group_var = !!sym(input$tpv_groupby)) %>%
            summarise(tpv = sum(amount_transacted) / 1000000000, .groups = 'drop')
        
        x_label <- str_to_title(str_replace_all(input$tpv_groupby, "_", " "))
        y_label <- "TPV (Billions)"
        title <- paste("Total TPV by", x_label)
        
        ggplot(plot_data, aes(x = reorder(group_var, -tpv), y = tpv)) +
            geom_bar(stat = "identity", fill = "steelblue4", alpha = 0.8) +
            labs(title = title, x = x_label, y = y_label) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Average Ticket Plot
    output$ticket_plot <- renderPlot({
        plot_data <- filtered_data_ticket() %>%
            group_by(group_var = !!sym(input$ticket_groupby)) %>%
            summarise(avg_ticket = sum(amount_transacted) / sum(quantity_transactions), .groups = 'drop')
        
        x_label <- str_to_title(str_replace_all(input$ticket_groupby, "_", " "))
        
        ggplot(plot_data, aes(x = reorder(group_var, -avg_ticket), y = avg_ticket)) +
            geom_bar(stat = "identity", fill = "steelblue4", alpha = 0.8) +
            labs(title = paste("Average Ticket Size by", x_label),
                 x = x_label, y = "Average Ticket Size") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Transaction Volume Plot
    output$volume_plot <- renderPlot({
        data <- filtered_data_volume()
        
        plot_data <- data %>%
            group_by(group_var = !!sym(input$volume_groupby)) %>%
            summarise(total_transactions = sum(quantity_transactions), .groups = 'drop')
        
        x_label <- str_to_title(str_replace_all(input$volume_groupby, "_", " "))
        title <- paste("Transaction Volume by", x_label)
        
        ggplot(plot_data, aes(x = reorder(group_var, -total_transactions), y = total_transactions)) +
            geom_bar(stat = "identity", fill = "steelblue4", alpha = 0.8) +
            labs(title = title, x = x_label, y = "Total Transaction Volume") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Trends Plot (NEW)
    output$trend_plot <- renderPlot({
        data <- filtered_data_trends()
        
        # Determine grouping variable (monthly or weekly)
        grouping_var <- switch(input$trend_granularity,
                               "Monthly" = "month",
                               "Weekly" = "week")
        
        # Calculate summary data based on metric and granularity
        plot_data <- data %>%
            mutate(time_period = floor_date(day, unit = grouping_var)) %>%
            group_by(time_period) %>%
            summarise(
                value = case_when(
                    input$trend_metric == "TPV" ~ sum(amount_transacted) / 1000000,
                    input$trend_metric == "Average Ticket" ~ sum(amount_transacted) / sum(quantity_transactions),
                    input$trend_metric == "Transaction Volume" ~ sum(quantity_transactions),
                    TRUE ~ NA_real_
                ),
                .groups = 'drop'
            )
        
        # Define plot titles and labels
        y_label <- switch(input$trend_metric,
                          "TPV" = "TPV (Millions)",
                          "Average Ticket" = "Average Ticket Size",
                          "Transaction Volume" = "Total Transaction Volume")
        
        plot_title <- paste(input$trend_metric, "Evolution", "by", input$trend_granularity)
        
        # Create the ggplot
        ggplot(plot_data, aes(x = time_period, y = value)) +
            geom_line(color = "steelblue4", size = 1) +
            geom_point(color = "steelblue4") +
            labs(title = plot_title, x = "Date", y = y_label) +
            theme_minimal() +
            scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Anticipation Methods Plot
    output$methods_plot <- renderPlot({
        filtered_data_methods() %>%
            ggplot(aes(x = entity, fill = anticipation_method)) +
            geom_bar(position = "fill") +
            labs(title = "Anticipation Method Usage by Entity",
                 x = "Entity", y = "Proportion", fill = "Anticipation Method") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_fill_brewer(palette = "Paired")
    })
    
    # Summary Statistics
    output$summary_stats <- renderText({
        data <- payment_data
        total_tpv <- sum(data$amount_transacted)
        total_transactions <- sum(data$quantity_transactions)
        avg_ticket_overall <- total_tpv / total_transactions
        unique_merchants <- sum(data$quantity_of_merchants)
        
        paste(
            paste("Total TPV:", scales::dollar(total_tpv / 1000000000, prefix = "$", suffix = "B")),
            paste("Total Transactions:", format(total_transactions, big.mark = ",")),
            paste("Overall Average Ticket:", scales::dollar(avg_ticket_overall, prefix = "$")),
            paste("Total Merchant Engagements:", format(unique_merchants, big.mark = ",")),
            paste("Data from", min(data$day), "to", max(data$day)),
            sep = "\n"
        )
    })
    
    # Top Performers Table
    output$top_performers <- renderTable({
        top_product_tpv <- payment_data %>%
            group_by(Product = product) %>%
            summarise(Value = sum(amount_transacted)) %>%
            mutate(Metric = "Top Product by TPV", Value = scales::dollar(Value / 1000000, prefix = "$", suffix = "M")) %>%
            arrange(desc(Value)) %>%
            head(1)
        
        top_tier_trans <- payment_data %>%
            group_by(Product = price_tier) %>%
            summarise(Value = sum(quantity_transactions)) %>%
            mutate(Metric = "Top Price Tier by Transactions", Value = format(Value, big.mark = ",")) %>%
            arrange(desc(Value)) %>%
            head(1)
        
        bind_rows(top_product_tpv, top_tier_trans) %>%
            select(Metric, Performer = Product, Value)
    })
    
    # Data Table
    output$data_table <- renderDT({
        datatable(payment_data, options = list(pageLength = 5, scrollX = TRUE))
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)

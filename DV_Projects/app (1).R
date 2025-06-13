library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(scales)
library(moments)
library(DT)
library(bslib)


# Load Data
df <- read.csv("final_structured_cardholders1.csv")
df$Date <- suppressWarnings(ymd(df$Date))
df$Datetime <- as.POSIXct(paste(df$Date, df$Time))
df$Hour <- hour(df$Datetime)
df$Weekday <- weekdays(df$Date)
df$Month <- format(df$Date, "%Y-%m")

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation { color: red; }
      .container-fluid { background-color: #f7f9fb; padding: 20px; }
      h2 { color: #003366; }
    "))
  ),
  div(
    h2("📊 Cardholder Sales Dashboard", style = "color: #003366;"),
    p("Authored by Arman Manoukian & Mariam Manoukian", style = "font-size: 14px; color: gray;")
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Filters", style = "color: #333"),
      selectInput("selected_gender", "Gender", choices = unique(df$Gender), selected = unique(df$Gender), multiple = TRUE),
      sliderInput("age_range", "Age Range", min(df$Age), max(df$Age), value = c(min(df$Age), max(df$Age))),
      dateRangeInput("date_range", "Date Range", start = min(df$Date), end = max(df$Date)),
      downloadButton("download_all_data", "Download Filtered Data")
    ),
    mainPanel(
      fluidRow(
        column(4, div(style="background-color:#eaf2f8; padding:15px; border-radius:10px;", strong("💰 Total Revenue:"), textOutput("totalRevenue"))),
        column(4, div(style="background-color:#e8f8f5; padding:15px; border-radius:10px;", strong("📦 Total Transactions:"), textOutput("totalTransactions"))),
        column(4, div(style="background-color:#fef9e7; padding:15px; border-radius:10px;", strong("🧍 Unique Customers:"), textOutput("uniqueCustomers")))
      ),
      tabsetPanel(
        tabPanel("Data Preview", DT::dataTableOutput("dataTable")),
        tabPanel("Gender & Age", plotlyOutput("genderPlot"), plotlyOutput("agePlot")),
        tabPanel(
          "Revenue & Products",
          plotlyOutput("topProductsPlot"),  
          br(),
          plotlyOutput("revenueDensityPlot"), 
          br(),
          plotlyOutput("revenueTimePlot")   
        ),
        tabPanel("Time Analysis", plotlyOutput("hourlyPlot"), plotlyOutput("weekdayPlot")),
        tabPanel("Advanced Insights", plotlyOutput("genderTrend"), plotlyOutput("ageGroupRevenue")),
        tabPanel("Demographics & Spending", plotlyOutput("revenueByGenderBar"), plotlyOutput("ageGroupSpendingPlot")),
        tabPanel("Revenue Behavior", plotlyOutput("genderMonthlyTrend")),
        tabPanel(
          "Stats Summary",
          fluidRow(
            column(6, verbatimTextOutput("fullStatsSummary"))
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  filtered_df <- reactive({
    df %>%
      filter(
        Gender %in% input$selected_gender,
        Age >= input$age_range[1] & Age <= input$age_range[2],
        Date >= input$date_range[1] & Date <= input$date_range[2]
      )
  })
  
  output$dataTable <- DT::renderDataTable({ filtered_df() })
  
  output$totalRevenue <- renderText({ paste0("֏ ", format(round(sum(filtered_df()$Revenue)), big.mark = ",")) })
  output$totalTransactions <- renderText({ format(nrow(filtered_df()), big.mark = ",") })
  output$uniqueCustomers <- renderText({ format(length(unique(filtered_df()$Birthdate)), big.mark = ",") })
  
  output$genderPlot <- renderPlotly({
    ggplotly(ggplot(filtered_df(), aes(x = Gender, fill = Gender)) +
               geom_bar() +
               scale_fill_brewer(palette = "Set2") +
               labs(title = "Gender Distribution", x = "Gender", y = "Count") +
               theme_minimal())
  })
  
  output$agePlot <- renderPlotly({
    ggplotly(ggplot(filtered_df(), aes(x = Age)) +
               geom_histogram(binwidth = 5, fill = "#FFA07A", color = "black") +
               labs(title = "Age Distribution", x = "Age", y = "Count") +
               theme_minimal())
  })
  
  output$topProductsPlot <- renderPlotly({
    top_products <- filtered_df() %>%
      group_by(Product) %>%
      summarise(TotalRevenue = sum(Revenue), AvgPrice = sum(Revenue)/sum(Quantity)) %>%
      arrange(desc(TotalRevenue)) %>%
      slice(1:15)
    
    ggplotly(
      ggplot(top_products, aes(x = reorder(Product, TotalRevenue), y = TotalRevenue, fill = AvgPrice)) +
        geom_col() +
        coord_flip() +
        scale_fill_viridis_c(option = "C") +
        labs(title = "Top 15 Products by Revenue (Color = Avg Price)",
             x = "Product", y = "Total Revenue") +
        theme_minimal()
    )
  })
  
  output$revenueTimePlot <- renderPlotly({
    df_grouped <- filtered_df() %>%
      group_by(Month) %>%
      summarise(MonthlyRevenue = sum(Revenue))
    
    ggplotly(ggplot(df_grouped, aes(x = Month, y = MonthlyRevenue, group = 1)) +
               geom_line(color = "#B22222", size = 1.2) +
               geom_point(color = "#B22222") +
               labs(title = "Monthly Revenue Trend", x = "Month", y = "Revenue") +
               theme_minimal())
  })
  
  output$hourlyPlot <- renderPlotly({
    ggplotly(ggplot(filtered_df(), aes(x = Hour)) +
               geom_histogram(binwidth = 1, fill = "#9370DB", color = "white") +
               labs(title = "Hourly Purchase Distribution", x = "Hour", y = "Count") +
               theme_minimal())
  })
  
  output$weekdayPlot <- renderPlotly({
    df_day <- filtered_df() %>%
      count(Weekday)
    
    ggplotly(ggplot(df_day, aes(x = reorder(Weekday, n), y = n)) +
               geom_col(fill = "#2E8B57") + coord_flip() +
               labs(title = "Transactions by Weekday", x = "Weekday", y = "Count") +
               theme_minimal())
  })
  
  output$revenueQtyPlot <- renderPlotly({
    ggplotly(ggplot(filtered_df(), aes(x = Quantity, y = Revenue)) +
               geom_jitter(alpha = 0.4, color = "#FF8C00") +
               geom_smooth(method = "lm", se = FALSE, color = "black") +
               labs(title = "Revenue vs Quantity", x = "Quantity", y = "Revenue") +
               theme_minimal())
  })
  
  output$ageRevenuePlot <- renderPlotly({
    ggplotly(ggplot(filtered_df(), aes(x = Age, y = Revenue)) +
               geom_point(alpha = 0.4, color = "#1E90FF") +
               geom_smooth(method = "lm", color = "black") +
               labs(title = "Age vs Revenue", x = "Age", y = "Revenue") +
               theme_minimal())
  })
  
  output$revenueByGenderBar <- renderPlotly({
    df_gender <- filtered_df() %>%
      group_by(Gender) %>%
      summarise(AvgRevenue = mean(Revenue))
    
    ggplotly(ggplot(df_gender, aes(x = Gender, y = AvgRevenue, fill = Gender)) +
               geom_col() +
               labs(title = "Average Revenue by Gender", y = "Average Revenue") +
               theme_minimal())
  })
  
  output$ageGroupSpendingPlot <- renderPlotly({
    df_age <- filtered_df() %>%
      mutate(AgeGroup = cut(Age, breaks = c(0, 20, 30, 40, 50, 60, 70, Inf),
                            labels = c("0–20", "21–30", "31–40", "41–50", "51–60", "61–70", "70+")))
    
    ggplotly(ggplot(df_age, aes(x = AgeGroup, y = Revenue)) +
               geom_boxplot(fill = "#9370DB") +
               labs(title = "Revenue Distribution by Age Group", x = "Age Group", y = "Revenue") +
               theme_minimal())
  })
  
  output$revenueDensityPlot <- renderPlotly({
    p <- ggplot(filtered_df(), aes(x = Revenue)) +
      geom_density(fill = "lightblue", alpha = 0.6) +
      labs(title = "Revenue Density Plot", x = "Revenue", y = "Density") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$genderMonthlyTrend <- renderPlotly({
    df_gender_month <- filtered_df() %>%
      group_by(Month, Gender) %>%
      summarise(TotalRevenue = sum(Revenue))
    
    ggplotly(ggplot(df_gender_month, aes(x = Month, y = TotalRevenue, color = Gender, group = Gender)) +
               geom_line(size = 1.2) +
               labs(title = "Monthly Revenue Trend by Gender", x = "Month", y = "Revenue") +
               theme_minimal())
  })
  
  output$genderTrend <- renderPlotly({
    trend_gender <- filtered_df() %>%
      group_by(Month, Gender) %>%
      summarise(TotalRevenue = sum(Revenue))
    
    ggplotly(ggplot(trend_gender, aes(x = Month, y = TotalRevenue, color = Gender, group = Gender)) +
               geom_line(size = 1.2) +
               labs(title = "Revenue Trend by Gender", x = "Month", y = "Revenue") +
               theme_minimal())
  })
  
  output$ageGroupRevenue <- renderPlotly({
    age_data <- filtered_df() %>%
      mutate(AgeGroup = cut(Age, breaks = c(0, 18, 25, 35, 45, 55, 65, Inf),
                            labels = c("0-18", "19-25", "26-35", "36-45", "46-55", "56-65", "65+"))) %>%
      group_by(AgeGroup) %>%
      summarise(AvgRevenue = mean(Revenue))
    
    ggplotly(ggplot(age_data, aes(x = AgeGroup, y = AvgRevenue, fill = AgeGroup)) +
               geom_col() +
               labs(title = "Average Revenue by Age Group", x = "Age Group", y = "Average Revenue") +
               theme_minimal())
  })
  output$ageVsRevenuePlot <- renderPlotly({
  df_age <- filtered_df()
  
  ggplotly(
    ggplot(df_age, aes(x = Age, y = Revenue)) +
      geom_point(alpha = 0.5, color = "#1E90FF") +
      geom_smooth(method = "loess", color = "red") +
      labs(title = "Age vs Revenue", x = "Age", y = "Revenue") +
      theme_minimal()
  )
})

  output$fullStatsSummary <- renderPrint({
    revenue_values <- as.numeric(filtered_df()$Revenue)
    revenue_values <- revenue_values[!is.na(revenue_values)]  # Remove NAs
    
    revenue_summary <- summary(revenue_values)
    mean_revenue <- mean(revenue_values)
    median_revenue <- median(revenue_values)
    sd_revenue <- sd(revenue_values)
    min_revenue <- min(revenue_values)
    max_revenue <- max(revenue_values)
    quantiles_revenue <- quantile(revenue_values, probs = c(0.25, 0.5, 0.75))
    cv_revenue <- sd_revenue / mean_revenue
    skew <- as.numeric(skewness(revenue_values))
    kurt <- as.numeric(kurtosis(revenue_values))
    
    cat("Revenue Summary:\n")
    print(revenue_summary)
    cat("\nMean Revenue:", round(mean_revenue, 2), "\n")
    cat("Median Revenue:", round(median_revenue, 2), "\n")
    cat("Standard Deviation:", round(sd_revenue, 2), "\n")
    cat("Minimum Revenue:", round(min_revenue, 2), "\n")
    cat("Maximum Revenue:", round(max_revenue, 2), "\n")
    cat("25% Quantile:", round(quantiles_revenue[1], 2), "\n")
    cat("50% Quantile (Median):", round(quantiles_revenue[2], 2), "\n")
    cat("75% Quantile:", round(quantiles_revenue[3], 2), "\n")
    cat("Coefficient of Variation:", round(cv_revenue, 4), "\n")
    cat("Skewness:", round(skew, 4), "\n")
    cat("Kurtosis:", round(kurt, 4), "\n")
  })
  
  
  
  output$download_all_data <- downloadHandler(
    filename = function() { "final_structured_cardholders1.csv" },
    content = function(file) {
      write.csv(filtered_df(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)

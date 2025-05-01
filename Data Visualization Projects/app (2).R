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
ui <- navbarPage(
  title = div(icon("credit-card"), "Cardholder Sales Dashboard"),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",  # "flatly" = light theme. Change to "darkly" if you want dark theme by default
    primary = "#003366",
    base_font = font_google("Inter"),
    code_font = font_google("Fira Code")
  ),
  windowTitle = "Cardholder Dashboard",
  collapsible = TRUE,
  fluid = TRUE,
  inverse = FALSE,
  
  # Home
  tabPanel(
    title = div(icon("home"), "Home"),
    fluidPage(
      br(),
      div(
        style = "background-color:#eaf2f8; padding:40px; border-radius:15px; text-align:center;",
        h1("📊 Welcome to the Cardholder Sales Analytics Dashboard!", style = "font-weight:bold;"),
        h4("Created by Arman Manoukian & Mariam Manoukian", style = "color:gray;"),
        br(),
        p("Explore transaction patterns, customer demographics, revenue trends, and business insights.",
          style = "font-size:18px;")
      )
    )
  ),
  
  # Data Preview
  tabPanel(
    title = div(icon("table"), "Data Preview"),
    sidebarLayout(
      sidebarPanel(
        h4("Filters"),
        selectInput("selected_gender", "Gender", choices = unique(df$Gender),
                    selected = unique(df$Gender), multiple = TRUE),
        sliderInput("age_range", "Age Range", min(df$Age), max(df$Age),
                    value = c(min(df$Age), max(df$Age))),
        dateRangeInput("date_range", "Date Range", start = min(df$Date), end = max(df$Date)),
        downloadButton("download_all_data", "Download Filtered Data")
      ),
      mainPanel(
        DT::dataTableOutput("dataTable")
      )
    )
  ),
  
  # Gender & Age
  tabPanel(
    title = div(icon("users"), "Gender & Age"),
    fluidRow(
      column(6, plotlyOutput("genderPlot")),
      column(6, plotlyOutput("agePlot"))
    )
  ),
  
  # Revenue & Products
  tabPanel(
    title = div(icon("shopping-cart"), "Revenue & Products"),
    fluidRow(
      column(6, plotlyOutput("topProductsPlot")),
      column(6, plotlyOutput("revenueTimePlot"))
    )
  ),
  
  # Time Analysis
  tabPanel(
    title = div(icon("clock"), "Time Analysis"),
    fluidRow(
      column(6, plotlyOutput("hourlyPlot")),
      column(6, plotlyOutput("weekdayPlot"))
    )
  ),
  
  # Revenue Trends
  tabPanel(
    title = div(icon("chart-line"), "Revenue Trends"),
    fluidRow(
      column(6, plotlyOutput("genderTrend")),
      column(6, plotlyOutput("ageGroupRevenue"))
    )
  ),
  
  # Statistics
  tabPanel(
    title = div(icon("chart-bar"), "Statistics"),
    fluidPage(
      br(),
      verbatimTextOutput("skewnessOutput")
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
  
  output$genderPlot <- renderPlotly({
    ggplotly(ggplot(filtered_df(), aes(x = Gender, fill = Gender)) +
               geom_bar() +
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
  
  output$skewnessOutput <- renderPrint({
    cat("Skewness of Revenue:", skewness(filtered_df()$Revenue, na.rm = TRUE), "\n")
    cat("Kurtosis of Revenue:", kurtosis(filtered_df()$Revenue, na.rm = TRUE), "\n")
  })
  
  output$download_all_data <- downloadHandler(
    filename = function() { "final_structured_cardholders1.csv" },
    content = function(file) {
      write.csv(filtered_df(), file, row.names = FALSE)
    }
  )
}

# Run the App
shinyApp(ui = ui, server = server)

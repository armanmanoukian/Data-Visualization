# Load libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(countrycode)
library(moments)
library(treemapify)
library(rworldmap)
library(viridis)
library(forcats)
library(shinythemes)
library(DT)
library(shinyWidgets)
library(bslib)


# Load data
df <- read.csv("cleaned_data.csv", stringsAsFactors = FALSE)
df$genres <- str_squish(str_replace_all(df$genres, "'", ""))

# Unique genre choices (cleaned and sorted)
genre_choices <- sort(unique(unlist(strsplit(df$genres, ",\\s*"))))

# UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("spacelab"),

  tags$head(
    tags$style(HTML("
      body {
        background-color: #f4f6f9;
      }
      .sidebar-panel-custom {
        background-color: #ffffff;
        border-radius: 10px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        padding: 20px;
      }
      .tabbable > .nav > li > a {
        color: #2C3E50;
        font-weight: bold;
      }
      .tabbable > .nav > li[class=active] > a {
        background-color: #2C3E50 !important;
        color: white !important;
      }
    "))
  ),
  
  titlePanel(
    div(
      h1("🧠📖 Reading Frequency vs. Age", style = "font-weight: bold; color: #2C3E50; margin-bottom: 0;"),
      p("What the Data Reveals", style = "font-size: 18px; color: #5d6d7e; margin-top: 0; font-weight: bold;"),
      p("Authored by Arman Manoukian & Mariam Manoukian", style = "font-size: 13px; color: gray; margin-top: -10px;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar-panel-custom",
          h4("🔎 Filter Options", style = "color: #2C3E50;"),
          sliderInput("age_range", "Select Age Range:", min = 10, max = 80, value = c(20, 40)),
          pickerInput(
            inputId = "selected_genres",
            label = "Select Genres:",
            choices = genre_choices,
            selected = genre_choices,
            options = list(`actions-box` = TRUE, `live-search` = TRUE),
            multiple = TRUE
          ),
          br(),
          downloadButton("download_all_data", "Download Data", class = "btn btn-primary")
      ),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("📊 Data Cleaning & Stats", 
                 h3("🧹 Cleaned Data Sample"),
                 DT::dataTableOutput("cleanedData"),
                 br(),
                 h3("📈 Summary Statistics"),
                 verbatimTextOutput("summaryStats"),
                 br(),
                 h3("📊 Skewness and Kurtosis"),
                 verbatimTextOutput("skewnessKurtosis")
        ),
        tabPanel("📊 Age Distribution", plotOutput("agePlot")),
        tabPanel("🎭 Top Genres", plotOutput("genreBar")),
        tabPanel("📚 Age by Top Genres", plotOutput("genreAge")),
        tabPanel("🎨 Boxplot by Top Genres", plotOutput("boxPlot")),
        tabPanel("🏢 Top Publishers", plotOutput("publisherPlot")),
        tabPanel("🗺️ Reader Map", plotOutput("mapPlot")),
        tabPanel("🥧 Top Countries (Pie Chart)", plotOutput("countryPie")),
        tabPanel("🌟 Treemap of Genres", plotOutput("treemapPlot")),
        tabPanel("👥 Facet by Age Group", plotOutput("facetPlot")),
        tabPanel("📖 Fiction Popularity by Age", plotOutput("fictionPlot")),
        tabPanel("🏆 Top Fiction Countries", plotOutput("topFictionPlot")),
        tabPanel("🌎 Genre Diversity by Country", plotOutput("diversityPlot")),
        tabPanel("👥 Genres Read per Reader", plotOutput("genreCountPlot")),
        tabPanel("🧓 Average Reader Age Map", plotOutput("avgAgePlot")),
        tabPanel("👶 Young Readers Map", plotOutput("youngReaderPlot")),
        tabPanel("📚 Age Distribution by Selected Genre", plotOutput("selectedGenreAge")),
        tabPanel("📦 Boxplot by Selected Genre", plotOutput("selectedGenreBox")),
        
        
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_df <- reactive({
    df %>%
      filter(
        Age >= input$age_range[1],
        Age <= input$age_range[2]
      ) %>%
      separate_rows(genres, sep = ",\\s*") %>%
      filter(genres %in% input$selected_genres)
  })
  
  output$download_all_data <- downloadHandler(
    filename = function() {
      paste("filtered_readers_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_df(), file, row.names = FALSE)
    }
  )
  
  output$agePlot <- renderPlot({
    ggplot(filtered_df(), aes(x = Age)) +
      geom_histogram(binwidth = 5, fill = "#4682B4", color = "black", alpha = 0.8) +
      geom_vline(aes(xintercept = mean(Age, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1) +
      labs(
        title = "Age Distribution of Readers",
        subtitle = "Grouped in bins of 5 years with mean age line",
        x = "Age (Years)", y = "Number of Readers"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # Load cleaned data separately
  df_clean <- read.csv("cleaned_data.csv", stringsAsFactors = FALSE)
  
  output$cleanedData <- DT::renderDataTable({
    head(df_clean, 100)  # show first 100 rows
  })
  
  output$summaryStats <- renderPrint({
    summary(df_clean)
  })
  
  output$skewnessKurtosis <- renderPrint({
    skew_val <- moments::skewness(df_clean$Age, na.rm = TRUE)
    kurt_val <- moments::kurtosis(df_clean$Age, na.rm = TRUE)
    cat("Skewness of Age:", round(skew_val, 2), "\n")
    cat("Kurtosis of Age:", round(kurt_val, 2))
  })
  
  output$genreBar <- renderPlot({
    df_genres <- df_clean %>%
      separate_rows(genres, sep = ",\\s*") %>%
      filter(genres != "") %>%
      group_by(genres) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    ggplot(df_genres[1:15,], aes(x = reorder(genres, count), y = count)) +
      geom_bar(stat = "identity", fill = "darkorange") +
      coord_flip() +
      labs(
        title = "Top 15 Most Common Genres",
        x = "Genre", y = "Count"
      ) +
      theme_minimal()
  })
  output$selectedGenreAge <- renderPlot({
    df_filtered <- filtered_df()
    
    ggplot(df_filtered, aes(x = Age, fill = genres)) +
      geom_histogram(binwidth = 5, position = "identity", alpha = 0.6) +
      facet_wrap(~genres, scales = "free_y") +
      labs(
        title = "Age Distribution by Selected Genres",
        x = "Age", y = "Count"
      ) +
      theme_minimal()
  })
  output$selectedGenreBox <- renderPlot({
    df_filtered <- filtered_df()
    
    ggplot(df_filtered, aes(x = genres, y = Age, fill = genres)) +
      geom_boxplot(show.legend = FALSE) +
      labs(
        title = "Boxplot of Age by Selected Genres",
        x = "Genre", y = "Age"
      ) +
      theme_minimal()
  })
  
  
  output$genreAge <- renderPlot({
    top_genres <- df %>%
      separate_rows(genres, sep = ",\\s*") %>%
      count(genres, sort = TRUE) %>%
      slice_max(n, n = 6) %>%
      pull(genres)
    
    df_filtered <- filtered_df() %>%
      separate_rows(genres, sep = ",\\s*") %>%
      filter(genres %in% top_genres)
    
    ggplot(df_filtered, aes(x = Age, fill = genres)) +
      geom_histogram(binwidth = 5, position = "identity", alpha = 0.6) +
      facet_wrap(~genres, scales = "free_y") +
      labs(title = "Age Distribution for Top Genres", x = "Age", y = "Count") +
      theme_minimal()
  })
  
  output$boxPlot <- renderPlot({
    top_genres <- df %>%
      separate_rows(genres, sep = ",\\s*") %>%
      count(genres, sort = TRUE) %>%
      slice_max(n, n = 6) %>%
      pull(genres)
    
    df_box <- filtered_df() %>%
      separate_rows(genres, sep = ",\\s*") %>%
      filter(genres %in% top_genres)
    
    ggplot(df_box, aes(x = genres, y = Age, fill = genres)) +
      geom_boxplot(show.legend = FALSE) +
      labs(title = "Age Distribution by Top Genres", x = "Genre", y = "Age") +
      theme_minimal()
  })
  
  output$publisherPlot <- renderPlot({
    filtered_df() %>%
      count(Publisher, sort = TRUE) %>%
      slice_max(n, n = 10) %>%
      ggplot(aes(x = reorder(Publisher, n), y = n)) +
      geom_col(fill = "purple") +
      coord_flip() +
      labs(title = "Top 10 Publishers", x = "Publisher", y = "Count") +
      theme_minimal()
  })
  
  output$mapPlot <- renderPlot({
    # Step 1: Clean and summarize
    df_location <- filtered_df() %>%
      mutate(Country = countrycode(Location, origin = "country.name", destination = "country.name")) %>%
      filter(!is.na(Country)) %>%
      group_by(Country) %>%
      summarise(Readers = n(), .groups = "drop")
    
    # Step 2: Load shapefile
    world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
      mutate(Country = suppressWarnings(
        countrycode(name, origin = "country.name", destination = "country.name")
      ))
    
    # Step 3: Merge and plot
    map_sf <- world_sf %>%
      left_join(df_location, by = "Country")
    
    ggplot(map_sf) +
      geom_sf(aes(fill = Readers)) +
      scale_fill_viridis_c(option = "C", na.value = "grey90") +
      labs(title = "Reader Count by Country", fill = "Readers") +
      theme_void() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  
  output$treemapPlot <- renderPlot({
    df_genres <- filtered_df() %>%
      separate_rows(genres, sep = ",\\s*") %>%
      count(genres, sort = TRUE) %>%
      slice_max(n, n = 20)
    
    ggplot(df_genres, aes(area = n, fill = genres, label = genres)) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
      scale_fill_viridis_d(option = "plasma") +
      labs(title = "Treemap of Top Genres") +
      theme_bw()
  })
  
  output$facetPlot <- renderPlot({
    df_faceted <- filtered_df() %>%
      mutate(AgeGroup = cut(Age, breaks = c(10,20,30,40,50,60,100), labels = c("10s","20s","30s","40s","50s","60+"))) %>%
      separate_rows(genres, sep = ",\\s*") %>%
      group_by(AgeGroup, genres) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(AgeGroup) %>%
      slice_max(order_by = count, n = 10) %>%
      ungroup()
    
    ggplot(df_faceted, aes(x = fct_reorder(genres, count), y = count, fill = AgeGroup)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      facet_wrap(~AgeGroup, scales = "free_y") +
      labs(title = "Top 10 Genres by Age Group") +
      theme_minimal(base_size = 13)
  })
  
  output$countryPie <- renderPlot({
    df_country_pie <- df_clean %>%
      filter(!is.na(Location), Location != "") %>%
      count(Location, sort = TRUE) %>%
      slice_max(n, n = 10)
    
    ggplot(df_country_pie, aes(x = "", y = n, fill = Location)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Top 10 Countries by Reader Count") +
      theme_void() +
      theme(legend.title = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            legend.text = element_text(size = 11))
  })
  
  
  output$fictionPlot <- renderPlot({
    df_fiction <- df %>%
      separate_rows(genres, sep = ",\\s*") %>%
      mutate(is_fiction = ifelse(genres == "Fiction", 1, 0)) %>%
      group_by(User.ID, Age) %>%
      summarise(reads_fiction = max(is_fiction), .groups = "drop")
    
    ggplot(df_fiction, aes(x = Age, y = reads_fiction)) +
      geom_jitter(width = 0.5, height = 0.1, alpha = 0.3, color = "gray50") +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      labs(title = "Fiction Reading Likelihood by Age", x = "Age", y = "Reads Fiction (1 = Yes)") +
      theme_minimal()
  })
  
  output$topFictionPlot <- renderPlot({
    df_top_fiction <- df %>%
      separate_rows(genres, sep = ",\\s*") %>%
      mutate(FictionReader = ifelse(genres == "Fiction", 1, 0)) %>%
      group_by(Location) %>%
      summarise(FictionReaders = sum(FictionReader), TotalReaders = n(), .groups = "drop") %>%
      filter(TotalReaders >= 20) %>%
      slice_max(order_by = FictionReaders, n = 10)
    
    ggplot(df_top_fiction, aes(x = reorder(Location, FictionReaders), y = FictionReaders)) +
      geom_col(fill = "#6A51A3") +
      coord_flip() +
      labs(title = "Top 10 Countries by Fiction Readers") +
      theme_minimal()
  })
  
  output$diversityPlot <- renderPlot({
    df_diversity <- df %>%
      separate_rows(genres, sep = ",\\s*") %>%
      group_by(Location) %>%
      summarise(UniqueGenres = n_distinct(genres), .groups = "drop") %>%
      filter(!is.na(Location))
    
    ggplot(df_diversity, aes(x = reorder(Location, UniqueGenres), y = UniqueGenres)) +
      geom_col(fill = "#FB6A4A") +
      coord_flip() +
      labs(title = "Genre Diversity by Country") +
      theme_minimal()
  })
  
  output$genreCountPlot <- renderPlot({
    df_genrecount <- df %>%
      separate_rows(genres, sep = ",\\s*") %>%
      group_by(User.ID, Age) %>%
      summarise(GenreCount = n_distinct(genres), .groups = "drop")
    
    ggplot(df_genrecount, aes(x = Age, y = GenreCount)) +
      geom_smooth(method = "loess", se = TRUE, color = "#00796B", fill = "#B2DFDB") +
      labs(title = "Average Genre Diversity by Age") +
      theme_minimal()
  })
  
  output$avgAgePlot <- renderPlot({
    df_age <- df %>%
      mutate(Country = countrycode(Location, origin = "country.name", destination = "country.name")) %>%
      filter(!is.na(Country), !is.na(Age)) %>%
      group_by(Country) %>%
      summarise(Average_Age = round(mean(Age), 1), .groups = "drop")
    
    # Step 2: Load and harmonize shapefile
    world_sf <- rnaturalearth::ne_countries(returnclass = "sf") %>%
      mutate(Country = suppressWarnings(
        countrycode(name, origin = "country.name", destination = "country.name")
      ))
    
    # Step 3: Merge with shapefile
    map_sf <- world_sf %>%
      left_join(df_age, by = "Country")
    
    # Step 4: Plot the map
    ggplot(map_sf) +
      geom_sf(aes(fill = Average_Age)) +
      scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "grey90") +
      labs(
        title = "Average Reader Age by Country",
        subtitle = "Based on reported reader ages per country",
        fill = "Avg. Age",
        caption = "Source: Cleaned Reading Dataset"
      ) +
      theme_void() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "gray30"),
        plot.caption = element_text(size = 9, face = "italic", color = "gray50")
      )
  })
  
  output$youngReaderPlot <- renderPlot({
    # Step 1: Prepare young reader data
    young_rate_by_country <- df %>%
      mutate(
        Country = countrycode(Location, origin = "country.name", destination = "country.name"),
        Is_Young = ifelse(Age < 25, 1, 0)
      ) %>%
      filter(!is.na(Country)) %>%
      group_by(Country) %>%
      summarise(Young_Rate = mean(Is_Young, na.rm = TRUE), .groups = "drop")
    
    # Step 2: Load world shapefile and harmonize names
    world_sf <- rnaturalearth::ne_countries(returnclass = "sf") %>%
      mutate(Country = suppressWarnings(
        countrycode(name, origin = "country.name", destination = "country.name")
      ))
    
    # Step 3: Merge
    map_sf <- world_sf %>%
      left_join(young_rate_by_country, by = "Country")
    
    # Step 4: Plot
    ggplot(map_sf) +
      geom_sf(aes(fill = Young_Rate)) +
      scale_fill_viridis_c(option = "inferno", na.value = "grey90", labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = "Proportion of Young Readers by Country",
        subtitle = "Share of readers under 25 years old",
        fill = "% Under 25",
        caption = "Source: Cleaned Reading Dataset"
      ) +
      theme_void() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "gray30"),
        plot.caption = element_text(size = 9, face = "italic", color = "gray50")
      )
  })
  
}

# Run app
shinyApp(ui = ui, server = server) 
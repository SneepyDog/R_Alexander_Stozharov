library(shiny)
library(shinydashboard)
library(ggplot2)

# Загрузка данных
data <- read.csv("C:/Users/ASUS/Documents/GitHub/R_Alexander_Stozharov/lesson_4/2_vgsales.csv")

# Предварительная обработка данных
data$Year <- as.numeric(data$Year)

# Удаление строк с NA в столбце "Year"
data <- data[!is.na(data$Year), ]

# Определение UI
ui <- dashboardPage(
  dashboardHeader(title = "Аналитическое веб-приложение"),
  dashboardSidebar(
    selectInput("platform_filter", "Фильтр по платформе", unique(data$Platform)),
    sliderInput("year_filter", "Фильтр по году", min(data$Year), max(data$Year), value = c(min(data$Year), max(data$Year)))
  ),
  dashboardBody(
    # Остальная часть UI остается без изменений
  )
)

# Определение UI
ui <- dashboardPage(
  dashboardHeader(title = "Аналитическое веб-приложение"),
  dashboardSidebar(
    selectInput("platform_filter", "Фильтр по платформе", unique(data$Platform)),
    sliderInput("year_filter", "Фильтр по году", min(data$Year), max(data$Year), value = c(min(data$Year), max(data$Year)))
  ),
  dashboardBody(
    fluidRow(
      infoBoxOutput("na_sales_kpi"),
      infoBoxOutput("eu_sales_kpi"),
      infoBoxOutput("jp_sales_kpi"),
      infoBoxOutput("other_sales_kpi")
    ),
    fluidRow(
      box(
        title = "Динамика продаж",
        plotOutput("sales_dynamic")
      ),
      box(
        title = "Рейтинг по жанрам",
        plotOutput("genre_sales")
      )
    ),
    fluidRow(
      box(
        title = "Таблица с данными",
        tableOutput("data_table")
      )
    )
  )
)

# Определение серверной части
server <- function(input, output) {
  
  # Фильтр по платформе
  filtered_data <- reactive({
    data %>% filter(Platform %in% input$platform_filter,
                    Year >= input$year_filter[1],
                    Year <= input$year_filter[2])
  })
  
  # KPI
  output$na_sales_kpi <- renderInfoBox({
    na_sales_mean <- mean(filtered_data()$NA_Sales)
    infoBox("Средние NA Sales", na_sales_mean)
  })
  
  output$eu_sales_kpi <- renderInfoBox({
    eu_sales_mean <- mean(filtered_data()$EU_Sales)
    infoBox("Средние EU Sales", eu_sales_mean)
  })
  
  output$jp_sales_kpi <- renderInfoBox({
    jp_sales_mean <- mean(filtered_data()$JP_Sales)
    infoBox("Средние JP Sales", jp_sales_mean)
  })
  
  output$other_sales_kpi <- renderInfoBox({
    other_sales_mean <- mean(filtered_data()$Other_Sales)
    infoBox("Средние Other Sales", other_sales_mean)
  })
  
  # Линейный график (динамика продаж)
  output$sales_dynamic <- renderPlot({
    ggplot(filtered_data(), aes(x = Year)) +
      geom_line(aes(y = NA_Sales, color = "NA Sales")) +
      geom_line(aes(y = EU_Sales, color = "EU Sales")) +
      geom_line(aes(y = JP_Sales, color = "JP Sales")) +
      geom_line(aes(y = Other_Sales, color = "Other Sales")) +
      labs(title = "Динамика продаж",
           x = "Год",
           y = "Продажи") +
      scale_color_manual(values = c("NA Sales" = "blue", "EU Sales" = "green", "JP Sales" = "red", "Other Sales" = "purple"))
  })
  
  # Столбчатая диаграмма (рейтинг по жанрам)
  output$genre_sales <- renderPlot({
    genre_data <- filtered_data() %>% group_by(Genre) %>% summarise(NA_Sales = sum(NA_Sales),
                                                                    EU_Sales = sum(EU_Sales),
                                                                    JP_Sales = sum(JP_Sales),
                                                                    Other_Sales = sum(Other_Sales))
    
    genre_data_long <- pivot_longer(genre_data, cols = -Genre, names_to = "Region", values_to = "Sales")
    
    ggplot(genre_data_long, aes(x = Genre, y = Sales, fill = Region)) +
      geom_bar(stat = "identity") +
      labs(title = "Рейтинг продаж по жанрам",
           x = "Жанр",
           y = "Продажи",
           fill = "Регион")
  })
  
  # Таблица с данными
  output$data_table <- renderTable({
    filtered_data()
  })
}

# Запуск приложения
shinyApp(ui, server)
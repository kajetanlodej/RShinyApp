# app.R --
library(shiny)
library(shinydashboard)
library(dplyr)
library(gapminder)
library(tidyr)
library(stringr)
library(ECharts2Shiny)
library(ggplot2)
library(scales)


ui <- dashboardPage(
  dashboardHeader(title = "Population"),
  dashboardSidebar(
    # menu
    sidebarMenu(
      menuItem(
        "Statistics",
        tabName = "Statistics",
        badgeLabel = "Statistics",
        badgeColor = "yellow"
      ),
      menuItem(
        "Continents",
        tabName = "Continents",
        badgeLabel = "Continents",
        badgeColor = "olive"
      ),
      menuItem(
        "Countries",
        tabName = "Countries",
        badgeLabel = "Countries",
        badgeColor = "light-blue"
      )
    ),
    # input
    selectInput(
      'year',
      h3("Select year"),
      choices = c(1800:2100),
      multiple = FALSE,
      selected = 2023
    ),
    selectInput(
      'continent',
      h4("Select continent"),
      choices = c("Asia", "Africa", "Americas", "Europe", "Oceania"),
      multiple = TRUE,
      selected = c("Europe","Asia","Americas")
    ),
    sliderInput("slider", "Number of countries to show:", 1, 20, 25)
    
  ),
  dashboardBody(
    fluidRow(tabItems(
      tabItem(tabName = "Statistics",
              h2("Select year and continents",style="text-align:center;"),
              box(
                width =12,
                column(12, align="center", tableOutput('op'),
                plotOutput("plot1")
              ), column(1, align="left", tableOutput("tab")))),
      tabItem(
        tabName = "Continents",
        h2("Select year",style="text-align:center;"),
        box(
          width =12,
          column(12, align="center",
        plotOutput("plot2", height = 600)
      ), column(1, align="left", tableOutput("tab2")))),
      tabItem(
        tabName = "Countries",
        h2("Select year, continent and number of countries", style="text-align:center;"),
        box(
          width =12,
          column(12, align="center",
        plotOutput("plot3", height = 600)
      ), column(1, align="left", tableOutput("tab3"))))
    )))
)

server <- function(input, output) {

  # data selection
  d <- read.csv(file = "population_total",
                head = T)
  long_d <- gather(d, year, val, -country)
  
  long_d$year <-
    substr(long_d$year, 2, 5)
  
  long_d$year <-
    as.integer(long_d$year)
  
  total <- inner_join(long_d, gapminder, by = c("country"))
  
  colnames(total) <- c("country", "year", "population", "continent")
  
  total <- total %>%
    select(c(1:4)) %>%
    distinct()
  
  mvar <- str_detect(total$population, "M")
  kvar <- str_detect(total$population, "k")
  bvar <- str_detect(total$population, "B")
  
  total$population <-
    ifelse(
      mvar,
      as.numeric(str_replace(total$population, "M", "")) * 1000000,
      ifelse(
        kvar,
        as.numeric(str_replace(total$population, "k", "")) * 1000,
        as.numeric(str_replace(total$population, "B", "")) * 1000000000
      )
    )
  
  url <- a("gapminder", href="https://www.gapminder.org/data/documentation/gd003/")
  output$tab <- renderUI({
    tagList("source:", url)
  })
  output$tab2 <- renderUI({
    tagList("source:", url)
  })
  output$tab3 <- renderUI({
    tagList("source:", url)
  })
  
  output$op <- renderTable({
    stats <- total %>%
      filter(year == input$year & continent %in% input$continent) %>%
      group_by(continent, year) %>%
      arrange(-population) %>%
      summarize(
        continent = continent,
        mean = mean(population),
        median = median(population),
        min = min(population),
        max = max(population),
        sd = sd(population)
      ) %>%
      arrange(-mean) %>%
      distinct()
  })
  
  output$plot1 <- renderPlot({
    
    stats <- total %>%
      filter(year == input$year & continent %in% input$continent & !(country %in% c("China","India"))) %>%
      group_by(continent, year) %>%
      distinct()
    
    ggplot(data = stats, aes(x = continent, y = population)) +
      geom_boxplot(
        aes(fill=continent),
        alpha = 0.5,
        frame = FALSE,
        border = "steelblue"
      ) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
      scale_fill_manual(
        values = c(
          Asia = "Purple",
          Africa = "Green",
          Americas = "Orange",
          Europe = "Pink",
          Oceania = "Yellow"
        ))+
      labs(x = "Continent", y = "Population", title =
             "Boxplot by continent") +
      theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold')) +  theme_bw() +
      theme(
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12,
                                  face = "bold"),
        plot.title = element_text(size = 18,
                                  face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.title = element_text(size = 14,
                                    face = "bold"),
        legend.text = element_text(size = 12)
      ) 
    
  })
  
  output$plot2 <- renderPlot({
    fin <- total %>%
      group_by(continent, year) %>%
      summarise(pop_total = sum(population)) %>%
      filter(year == input$year)
    
    ggplot(fin, aes(
      x = pop_total,
      y = reorder(continent, pop_total),
      fill = continent
    )) +
      labs(
        x = "",
        y = "",
        title = "Total population by continent",
        color = "Continents"
      ) +
      theme(plot.title = element_text(
        hjust = 0.5,
        size = 15,
        face = 'bold'
      )) +
      scale_x_continuous(labels = unit_format(unit = "B", scale = 1e-9)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(
        values = c(
          Asia = "Purple",
          Africa = "Green",
          Americas = "Orange",
          Europe = "Pink",
          Oceania = "Yellow"
        ))+
      theme_bw() +
      theme(
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12,
                                  face = "bold"),
        plot.title = element_text(size = 18,
                                  face = "bold"),
        plot.subtitle = element_text(size = 12),
      )
    
  })
  
  output$plot3 <- renderPlot({
    countries <- total %>%
      filter(continent %in% input$continent & year == input$year) %>%
      arrange(-population) %>%
      head(input$slider) %>%
      arrange(-population)
    
    ggplot(countries, aes(
      x = population,
      y = reorder(country, population),
      fill = continent
    )) +
      labs(
        x = "Population",
        y = "Country",
        title = "Population by country",
        color = "Continents"
      ) +
      scale_fill_manual(
        values = c(
          Asia = "Purple",
          Africa = "Green",
          Americas = "Orange",
          Europe = "Pink",
          Oceania = "Yellow"
        )
      ) +
      scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      theme(plot.title = element_text(
        hjust = 0.5,
        size = 15,
        face = 'bold'
      )) +
      geom_bar(stat = "identity") + 
      geom_text(
        aes(label = paste0(
          as.character(population / 1000000), "M"
        )),
        vjust = 0.4,
        color = "black",
        size = 3.8,
        hjust = -0.05
      ) +
      
      theme_bw() +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12,
                                  face = "bold"),
        plot.title = element_text(size = 18,
                                  face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.title = element_text(size = 14,
                                    face = "bold"),
        legend.text = element_text(size = 12)
      )
    
    
  })
}

shinyApp(ui, server)
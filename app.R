# An intro to shiny

# install necessary packages
# install.packages(c("shiny", "ggplot2", "scales"))

# require will load the packages if they're not already loaded
require(shiny)
require(ggplot2)
require(scales)

data <- read.csv("data/finalData.csv", head=T, sep=",")

data$GEO <- factor(data$GEO, 
                   levels=c("Canada", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC"))

ui <- fluidPage(
  
  titlePanel(title="Median income dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      
      # reference years input
      sliderInput(inputId = "refYears",
                  label = "Select a reference period:",
                  min = 1976,
                  max = 2018,
                  value = c(1976,2018),
                  sep = ""),
      
      # source of income input
      selectInput(inputId = "incomeSource",
                  label = "Select an income source:",
                  choices = c("Total income",
                              "Market income",
                              "Employment income",
                              "Wages, salaries and commissions",
                              "Self-employment income",
                              "Government transfers"),
                  selected = "Total income")
    ),
    mainPanel(
              plotOutput("yearsPlot"),
              plotOutput("geoPlot")
    )
  )
)

server <- function(input, output) {
  
  output$yearsPlot <- renderPlot({
    yearsData <- subset(data, Income.source == input$incomeSource &
                             input$refYears[1] <= Year & 
                             Year <= input$refYears[2] &
                             Sex == "Both sexes" &
                             Age.group == "Total age group" &
                             GEO == "Canada" )
    
    ggplot(yearsData) + 
      geom_line(aes(x=Year, y=VALUE), size = 1.5) +
      theme_classic() +
      scale_y_continuous(labels = comma, limits = c(0,NA)) +
      labs(y="Median income ($)",
           x="Year",
           title=paste0("Median ",input$incomeSource," by year"))
  })
  
  output$geoPlot <- renderPlot({
    geoData <- subset(data, Income.source == input$incomeSource &
                        Year == input$refYears[2] &
                        Sex == "Both sexes" &
                        Age.group == "Total age group")
    
    ggplot(geoData) +
      geom_col(aes(x=GEO, y=VALUE)) +
      theme_classic() +
      scale_y_continuous(labels = comma) +
      labs(y="Median income ($)", 
           x="Geography", 
           title=paste0("Median ", input$incomeSource, " by geography, ",input$refYears[2]))
  })
  
  
  output$plotid <- renderPlot({
    # R code to create plot
  })
  
}

shinyApp(ui, server)
  
  

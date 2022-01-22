library(shiny)
library(plotly)
source("FRS_Benefit_Model Function.R")


# Define UI for application that draws a benefit chart
ui <- fluidPage(

    # Application title
    titlePanel("Florida FRS Benefit Model"),
    #The app needs four inputs: DB_ARR, DC_return, DC_ER, ea
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "DB_ARR",
                        label = "DB Assumed Rate of Return",
                        min = 3,
                        max = 8,
                        step = 0.1,
                        post = "%",
                        value = 6.8),
            sliderInput(inputId = "DC_return",
                        label = "DC Expected Rate of Return",
                        min = 3,
                        max = 8,
                        step = 0.1,
                        post = "%",
                        value = 6),
            sliderInput(inputId = "DC_ER",
                        label = "DC Employer Contribution (New DC Plan)",
                        min = 0,
                        max = 10,
                        step = 0.1,
                        post = "%",
                        value = 6.3),
            selectInput(inputId = "ea",
                        label = "Entry Age",
                        choices = SalaryEntry$entry_age,
                        selected = 27)
        ),

        # Show the benefit plot
        mainPanel(
           plotlyOutput("BenefitPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

    output$BenefitPlot <- renderPlotly({
      DB <- benefit_cal(output = "DB", 
                        DB_ARR = input$DB_ARR/100, 
                        ea = input$ea) %>% 
        rename(DB = RealPenWealth)
      
      DC_current <- benefit_cal(output = "DC", 
                                DCreturn = input$DC_return/100, 
                                ea = input$ea) %>% 
        rename(`DC current` = RealDC_balance)
      
      DC_new <- benefit_cal(output = "DC", 
                            DCreturn = input$DC_return/100,
                            DC_ER = input$DC_ER/100,
                            ea = input$ea) %>% 
        rename(`DC new` = RealDC_balance)
      
      result <- DB %>% 
        left_join(DC_current, by = "Age") %>% 
        left_join(DC_new, by = "Age" ) %>% 
        pivot_longer(-Age, names_to = "Plan", values_to = "Wealth")
      
      p <- ggplot(result, aes(x= Age, y = Wealth, col = Plan, group = Plan,
                              text = paste("Age:", Age, "<br>",
                                           "Wealth:", dollar(Wealth), "<br>",
                                           Plan))) +
        geom_line(size = 1, show.legend = F) +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0, 80, by = 10),
                           name = "Age",
                           expand = c(0,0)) +
        scale_y_continuous(breaks = pretty_breaks(10),
                           labels = dollar_format(),
                           expand = c(0,0)) +
        labs(y = "Pension Wealth") +
        theme(text = element_text(size = 12),
              legend.title = element_blank())
      
      ggplotly(p, tooltip = c("text"))
        
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

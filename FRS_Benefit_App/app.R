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
    
    # Benefit accrual chart & Attrition chart
    mainPanel(
      plotlyOutput("Plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  #Benefit wealth accrual chart & Attrition chart
  output$Plot <- renderPlotly({
    
    #Benefit wealth accrual calculations
    #DB
    DB <- benefit_cal(output = "DB", 
                      DB_ARR = input$DB_ARR/100, 
                      ea = input$ea) %>% 
      rename(DB = RealPenWealth)
    
    #Current DC
    DC_current <- benefit_cal(output = "DC", 
                              DCreturn = input$DC_return/100, 
                              ea = input$ea) %>% 
      rename(`DC current` = RealDC_balance)
    
    #New DC with a different ER contribution rate
    DC_new <- benefit_cal(output = "DC", 
                          DCreturn = input$DC_return/100,
                          DC_ER = input$DC_ER/100,
                          ea = input$ea) %>% 
      rename(`DC new` = RealDC_balance)
    
    #Combine the above DB, Current DC, and New DC results
    result <- DB %>% 
      left_join(DC_current, by = "Age") %>% 
      left_join(DC_new, by = "Age" ) %>% 
      pivot_longer(-Age, names_to = "Plan", values_to = "Wealth")
    
    #Attrition rate calculation
    attrition <- benefit_cal(output = "attrition", ea = input$ea)
    
    #Plot benefit wealth accrual lines
    line_size = 3
    p1 <- plot_ly(result, 
                  x = ~Age, y = ~Wealth, 
                  color = ~Plan,
                  text = ~Plan,        #need to map Plan to text for the hovertemplate below. For some reason, referencing variable "color" doesn't work.
                  type = "scatter", 
                  mode = "lines",
                  line = list(width = line_size),    #set line thickness
                  colors = c("#F76262", "#216583", "#65C0BA"),
                  hovertemplate = "%{text}<br>Age: %{x}<br>Wealth: %{y}<extra></extra>",    #for hoverinfo
                  height = 700) %>%    #setting the height of the chart
      
      layout(yaxis = list(tickformat = "$0.2s",             #formatting the y axis number format
                          title = "Pension Wealth"),        #manually change the y axis title
             legend = list(x = 0.5,
                           y = 1, 
                           xanchor = "center",
                           orientation = "h"))              #positioning the legend
    
    #Plot attrition rate
    p2 <- plot_ly(attrition,
                  x = ~Age, y = ~RemainingProb,
                  type = "scatter",
                  mode = "lines",
                  line = list(width = line_size),
                  color = "#BB371A",
                  hovertemplate = "Age: %{x}<br>% remaining: %{y}<extra></extra>",
                  showlegend = F) %>%            #remove legend; otherwise it'll show up on the previous chart 
      layout(yaxis = list(tickformat = ".0%",
                          title = "Expected % of Members Remaining"))
    
    #Combine two charts into one 
    subplot(p1, p2, nrows = 2, margin = 0.03, titleY = T) %>%     #set titleY = T to retain the y axis titles
      layout(annotations = list(     #add two titles to two charts 
        list(x = 0, y = 1.03, text = "<b>Benefit Wealth Accrual</b>", showarrow = F, xref='paper', yref='paper'),
        list(x = 0, y = 0.47, text = "<b>Attrition</b>", showarrow = F, xref='paper', yref='paper')
      ))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)
library(ggthemes)

svi <- read.csv("https://raw.githubusercontent.com/petersemily/shiny-app-danl310/refs/heads/main/SVI2022_NEWYORK_tract.csv")

svi2 <- svi |>
  select(COUNTY, E_TOTPOP, E_NOVEH, E_UNEMP, E_NOHSDP, E_UNINSUR,
         E_AGE65, E_AGE17, E_DISABL, E_MINRTY, E_GROUPQ) |>
  filter(COUNTY %in% c('Dutchess County', 'Lewis County', 'Livingston County', 'Madison County', 
                       'Nassau County', 'Ontario County',
                       'Schoharie County', 'Tioga County', 
                       'Ulster County', 'Wayne County', 'Wyoming County'),
         E_TOTPOP >= 0,
         E_NOVEH >= 0, 
         E_UNEMP >= 0, 
         E_NOHSDP >= 0, 
         E_UNINSUR >= 0, 
         E_AGE65 >= 0, 
         E_AGE17 >= 0, 
         E_DISABL >= 0,
         E_MINRTY >= 0,
         E_GROUPQ >= 0) |>
  group_by(COUNTY) |>
  summarize(C_TOTPOP = sum(E_TOTPOP),
            C_NOVEH = sum(E_NOVEH),
            C_UNEMP = sum(E_UNEMP),
            C_NOHSDP = sum(E_NOHSDP),
            C_UNINSUR = sum(E_UNINSUR),
            C_AGE65 = sum(E_AGE65),
            C_AGE17 = sum(E_AGE17),
            C_DISABL = sum(E_DISABL),
            C_MINRTY = sum(E_MINRTY))

svi3 <- svi2 |>
  mutate(NOVEH = C_NOVEH / C_TOTPOP,
         UNEMP = C_UNEMP / C_TOTPOP,
         NOHSDP = C_NOHSDP / C_TOTPOP,
         UNINSUR = C_UNINSUR / C_TOTPOP,
         AGE65 = C_AGE65 / C_TOTPOP,
         AGE17 = C_AGE17 / C_TOTPOP,
         DISABL = C_DISABL / C_TOTPOP,
         MINRTY = C_MINRTY / C_TOTPOP)
factors <- c("NOVEH", "UNEMP", "NOHSDP", "UNINSUR", "AGE65", "AGE17", "DISABL", "MINRTY")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("SVI Factor Scatter Plot"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Select X-axis Variable:",
                  choices = factors,
                  selected = "NOVEH"),
      
      selectInput("yvar", "Select Y-axis Variable:",
                  choices = factors,
                  selected = "UNEMP")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    ggplot(svi3, aes_string(x = input$xvar, y = input$yvar, label = "COUNTY")) +
      geom_point(color = "steelblue", size = 3) +
      geom_text(vjust = -0.5, size = 3) +
      labs(
        x = input$xvar,
        y = input$yvar,
        title = paste("Scatter Plot of", input$yvar, "vs", input$xvar)
      ) +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
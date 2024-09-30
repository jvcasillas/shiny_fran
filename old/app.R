library("shiny")
library("lubridate")

calc_fran <- function(bw = 180, cj = 200, pu = 20) {

  # model coefficients
  a    <-  6.7610
  b_cj <- -0.0054
  b_pu <- -0.0106
  b_bw <-  0.0025

  # Linear predictor
  fran_secs <- round(exp(a + (b_cj * cj) + (b_pu * pu) + (b_bw * bw)))
  fran_mins <- ms(paste("0:", as.character(fran_secs)), roll = T)

  output <- list(seconds = fran_secs, minutes = fran_mins)
  return(output)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Fran time calculator"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           selectInput("bio_sex", 
                       "Biological sex",
                       c("male" = "male", 
                         "female" = "female")),
           numericInput("bw", 
                        "Body weight (lbs)", 
                        value = 100, 
                        min = 1, 
                        max = 500), 
           numericInput("cj", 
                        "Max clean and jerk (lbs)", 
                        value = 100, 
                        min = 1, 
                        max = 1000), 
           numericInput("pu", 
                        "Max pull ups", 
                        value = 10, 
                        min = 1, 
                        max = 100)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("fran_time")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$fran_time <- renderText({
        times <- calc_fran(bw = input$bw, cj = input$cj, pu = input$pu)
        paste0("<h2>Your estimated time is: </h2></br>", 
               "<h4>", 
               times$seconds, " Seconds </br></br>", 
               "or </br></br>", 
               times$minutes, "</h4>")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



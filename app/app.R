library("shiny")
library("lubridate")
library("bslib")

# shinylive::export("app", "docs")

theme <- bs_theme(
  # Controls the default grayscale palette
  bg = "#fff", fg = "#202123",
  # Controls the accent (e.g., hyperlink, button, etc) colors
  primary = "#cc0033", secondary = "#48DAC6",
  base_font = c("Grandstander", "sans-serif"),
  code_font = c("Courier", "monospace"),
  heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  # Can also add lower-level customization
  "input-border-color" = "#cc0033"
)

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
    theme = theme, 
    # Application title
    titlePanel("Fran time calculator", windowTitle = "Fran time calculator"),
    
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



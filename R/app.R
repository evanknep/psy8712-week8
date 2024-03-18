#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
data = read_csv("../data/week8_shiny_data.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("PSY 8712 Week 8 Project"), #creating a project title that matches what I have in my markdown file
    selectInput('shaded_error', label = 'Display shaded error bars?', choices = c('Display Error Band', 'Suppress Error Band'), selected = "Display Error Band"), #creating a input select for displaying shaded error bars
    selectInput('sex', label = "Select Sex", choices = c('Male', 'Female', 'All'), selected = 'All'), #creating input select for sex
    selectInput('date', label = "Exclude Participants from before July 1, 2017?", choices = c('Exclude', 'Do not exclude'), selected = "Do not exclude"), #creating input select for excluding pre 2017
    
    
    plotOutput("corr_plot"), #creating plot objects to reference in our server section below. Putting it in the main body here without tabs or paneling
    

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
      output$corr_plot <- renderPlot({
        data %>%
          ggplot(aes(x=q1_q6_mean, y=q8_q10_mean)) +
          geom_point() + 
          geom_smooth(method = "lm", color = "purple") + 
          labs(x = "q1-q6 mean", y = "q8-q10 mean", title = "Correlation of Q1-Q6 and Q8-Q10 Question Scores")
      })

   
}

# Run the application 
shinyApp(ui = ui, server = server)

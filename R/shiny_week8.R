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
    selectInput('sex', label = "Select Sex", choices = c('Male' = 'M', 'Female' = 'F', 'All'), selected = 'All'), #creating input select for sex
    selectInput('date', label = "Exclude Participants from before July 1, 2017?", choices = c('Exclude', 'Do not exclude'), selected = "Do not exclude"), #creating input select for excluding pre 2017
    
    
    plotOutput("corr_plot"), #creating plot objects to reference in our server section below. Putting it in the main body here without tabs or paneling
    

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # this function is to filter the data based on the users choice for gender/sex. 
  #If they select All, which is the default, no filtering occurs, otherwise it will take their choice of M/F and filter accordingly.
  # added a portion to filter the date as well. I wrote it this way after failing to find a way to incorporate the reactive plotting and 
  #filtering in the same pipeline. I had originally tried to write it so that my dataset filtered reactively and then still piped into my ggplot so that I could 
  #recycle my plotting code from the markdown file but I failed to get it to work. I settled on this method because even if it is perhaps not the most efficient
  #it gets the job done. It just filters a data set based on our user inputs, and then ultimately returns that dataset as our filtered_data, which then gets plugged
  #in to our ggplot. I set my inputs for sex/gender above so that Male and Female = M/F, which lets me filter by the input value specifically rather than adding 
  #separate if statements for each. This way I only need to account for if they selected All, our default value.
  filtered_data <- reactive({ 
    if (input$sex != 'All') {
      data <- data %>% filter(gender == input$sex) 
    }
    if (input$date == 'Exclude') { 
      data <- data %>% filter(timeEnd >= '2017-07-01')
    }
    data })
    
  
      
  
  output$corr_plot <- renderPlot({
    # rendering our ggplot in our corrplot spot assigned above. It's a basic scatterplot using our filtered_data (based on user input)
    # with the only difference being the if statement that gatekeeps our shaded error bar. If they select to display it a geom_smooth
    # call goes through, otherwise we live without it.
    ggplot(filtered_data(), aes(x=q1_q6_mean, y=q8_q10_mean)) +
      geom_point() + 
      labs(x = "q1-q6 mean", y = "q8-q10 mean", title = "Correlation of Q1-Q6 and Q8-Q10 Question Scores") +
      if (input$shaded_error == 'Display Error Band') {
        geom_smooth(method = "lm", color = "purple")
      }
        
        
        
      })

   
}

# Run the application 
shinyApp(ui = ui, server = server)

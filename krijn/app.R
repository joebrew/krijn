library(shiny)
library(plotly)
library(threejs)

x <- sample(seq(0, 1, length = 10000), 100)
y <- sample(seq(0, 1, length = 10000), 100)
z <- seq(0, 24, length = 100)
df <- data.frame(x, y, z)
# Make matrix version
df_mat <- matrix(sample(seq(0,24, length = 100), 100000, replace = TRUE),
                 nrow = 100,
                 ncol = 100)

# Function to define matrix based on input
make_matrix <- function(x1 = 0.3,
                        x2 = 0.8,
                        y1 = 0.2,
                        y2 = 0.5,
                        z1 = 5,
                        z2 = 7){
  x1 <- x1 * 1000
  x2 <- x2 * 1000
  y1 <- y1 * 1000
  y2 <- y2 * 1000
  the_matrix <- matrix(NA,
                       nrow = 1000,
                       ncol = 1000)
  the_matrix[x1:x2, y1:y2] <- z1
  the_matrix
  
}

# Make a scattering of points
make_scatter <- function(x1 = 0.3,
                         x2 = 0.8,
                         y1 = 0.2,
                         y2 = 0.5,
                         z1 = 5,
                         z2 = 7){

  the_df <- data.frame(x = sample(seq(x1, x2, length = 1000),1000),
                       y = sample(seq(y1, y2, length = 1000),1000),
                       z = sample(seq(z1, z2, length = 1000), 1000))
  the_df[1001,] <- c(0, 0, 0)
  the_df[1002,] <- c(1, 1, 24)
  return(the_df)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Interactive entomology: bugs in the cloud"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput('x', 
                     'Human blood index',
                     min = 0,
                     max = 1,
                     value = c(0.3, 0.6),
                     step = 0.01),
         sliderInput('y', 
                     'Place of biting',
                     min = 0,
                     max = 1,
                     value = c(0.1, 0.8),
                     step = 0.01),
         sliderInput('z', 
                     'Hours since noon',
                     min = 0,
                     max = 24,
                     value = c(5, 7),
                     step = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput('the_plot',
                     width = 500,
                     height = 500),
        helpText('Created by Krijn Paaijmans and Joe Brew.',
                 a('www.economicsofmalaria.com',
                   href = 'www.economicsofmalaria.com'))
        # webGLOutput("the_gl"),
        # uiOutput('scatter_plot')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$the_plot <- renderPlotly({
     # plot_ly(df, x = ~x, y = ~y, z = ~z, color = ~time) %>% add_lines()
     # plot_ly(z = ~volcano) %>% add_surface()
     # plot_ly(z = ~df_mat) %>% add_surface()
     # plot_ly(z = make_matrix()) %>% add_surface()
     
     the_data <- make_scatter(x1 = input$x[1],
                              x2 = input$x[2],
                              y1 = input$y[1],
                              y2 = input$y[2],
                              z1 = input$z[1],
                              z2 = input$z[2])
       plot_ly(the_data, 
             marker = list(color = 
                             c(rep('blue', 1000),
                               rep('white', 2)),
                           size = c(rep(5, 1000),
                                    rep(0, 2))),
             x = ~x, y = ~y, z = ~z) %>%
       add_markers() %>%
         layout(title = "",
                scene = list(
                  xaxis = list(title = "Human blood index"), 
                  yaxis = list(title = "Place of biting"), 
                  zaxis = list(title = "Hours since noon")))
   })

   # output$plott <- renderScatterplotThree({
   #   scatterplot3js(x = df$x, y = df$y, z = df$z)
   # })
   # output$scatter_plot <- renderUI({
   #   scatterplotThreeOutput("plott")
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)


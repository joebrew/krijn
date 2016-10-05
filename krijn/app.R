library(shiny)
library(plotly)
library(threejs)

x <- sample(seq(0, 1, length = 10000), 100)
y <- seq(0, 24, length = 100)
z <- sample(seq(0, 1, length = 10000), 100)
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
                         y1 = 5,
                         y2 = 7,
                         z1 = 0.2,
                         z2 = 0.5){
  x_length <- (x2 - x1) / 1
  y_length <- (y2 - y1) / 24
  z_length <- (z2 - z1) / 1
  the_area <- x_length * y_length * z_length
  n_points <- the_area * 50000

  the_df <- data.frame(x = sample(seq(x1, x2, length = 10000),n_points, replace = TRUE),
                       y = sample(seq(y1, y2, length = 10000),n_points, replace = TRUE),
                       z = sample(seq(z1, z2, length = 10000), n_points, replace = TRUE))
  the_df[n_points + 1,] <- c(0, 0, 0)
  the_df[n_points + 2,] <- c(1, 24, 1)
  return(the_df)
}

# Function for making color vectors
make_color <- function(the_data, color = 'blue'){
  c(rep(color, nrow(the_data) - 2),
    rep('white', 2))
}
make_size <- function(the_data, size = 5){
  c(rep(size, nrow(the_data) - 2),
    rep(0, 2))
}
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Interactive entomology: bugs in the cloud"),
   helpText('Created by Joe Brew, Krijn Paaijmans, and Carlos Chaccour.',
            a('www.economicsofmalaria.com',
              href = 'www.economicsofmalaria.com')),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h3('Species 1'),
        sliderInput('x', 
                     'Human blood index',
                     min = 0,
                     max = 1,
                     value = c(0.1, 0.8),
                     step = 0.01),
         sliderInput('y', 
                     'Hours since noon',
                     min = 0,
                     max = 24,
                     value = c(14,24),
                     step = 1),
         sliderInput('z', 
                     'Place of biting',
                     min = 0,
                     max = 1,
                     value = c(0.1, 0.3),
                     step = 0.01),
        br(), br(),
        h3('Species 2'),
        sliderInput('x2', 
                    'Human blood index',
                    min = 0,
                    max = 1,
                    value = c(0.3, 0.6),
                    step = 0.01),
        sliderInput('y2', 
                    'Hours since noon',
                    min = 0,
                    max = 24,
                    value = c(5, 7),
                    step = 1),
        sliderInput('z2', 
                    'Place of biting',
                    min = 0,
                    max = 1,
                    value = c(0.1, 0.8),
                    step = 0.01),
        br(), br(),
        h3('Species 3'),
        sliderInput('x3', 
                    'Human blood index',
                    min = 0,
                    max = 1,
                    value = c(0.5, 0.95),
                    step = 0.01),
        sliderInput('y3', 
                    'Hours since noon',
                    min = 0,
                    max = 24,
                    value = c(2, 10),
                    step = 1),
        sliderInput('z3', 
                    'Place of biting',
                    min = 0,
                    max = 1,
                    value = c(0.6, 0.8),
                    step = 0.01),
        br(), br()
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput('the_plot',
                     width = 500,
                     height = 500)
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
     
     the_data2 <- make_scatter(x1 = input$x2[1],
                              x2 = input$x2[2],
                              y1 = input$y2[1],
                              y2 = input$y2[2],
                              z1 = input$z2[1],
                              z2 = input$z2[2])
     
     the_data3 <- make_scatter(x1 = input$x3[1],
                               x2 = input$x3[2],
                               y1 = input$y3[1],
                               y2 = input$y3[2],
                               z1 = input$z3[1],
                               z2 = input$z3[2])
     
     the_data$color <- make_color(the_data = the_data, color = 'darkgreen')
     the_data2$color <- make_color(the_data = the_data2, color = 'darkred')
     the_data3$color <- make_color(the_data = the_data3, color = 'darkorange')
     the_data$size <- make_size(the_data = the_data)
     the_data2$size <- make_size(the_data = the_data2)
     the_data3$size <- make_size(the_data = the_data3)
     
     the_data <- rbind(the_data, the_data2, the_data3)
       plot_ly(the_data, 
             marker = list(color = 
                             the_data$color,
                           size = the_data$size),
             x = ~x, y = ~y, z = ~z) %>%
       add_markers() %>%
         layout(title = "",
                scene = list(
                  xaxis = list(title = "Human blood index"), 
                  yaxis = list(title = "Hours since noon"), 
                  zaxis = list(title = "Place of biting")))
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


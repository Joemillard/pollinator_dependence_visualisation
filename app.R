# shiny app for visualising change in pollination dependent vulnerability

library(shiny)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Change in global pollination dependence vulnerability"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("time",
                        "Number of bins:")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("maplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # read in the map data
    map_data <- readRDS("data/global_change_pollination_dependence.rds")
    
    output$maplot<- renderPlot({
        
        # plot the ggplot map for climate anomaly
        map_data[[1]] %>%
            ggplot() +
            geom_polygon(aes(x = long, y = lat, group = group), data = map_fort, fill = "grey", alpha = 0.3) +
            geom_tile(aes(x = x, y = y, fill = poll_vulnerability)) +
            ggtitle("2050") +
            scale_fill_viridis("Vulnerability-weighted \npollination dependence)",
                               na.value = "transparent", option = "plasma", direction = -1,
                               limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.5", "0.75", "1")) +
            coord_equal() +
            guides(fill = guide_colourbar(ticks = FALSE)) +
            theme(panel.background = element_blank(),
                  panel.bord = element_blank(),
                  panel.grid = element_blank(), 
                  axis.text = element_blank(),
                  axis.ticks = element_blank(), 
                  axis.title = element_blank(),
                  legend.position = "none")
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

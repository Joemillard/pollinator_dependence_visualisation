# shiny app for visualising change in pollination dependent vulnerability

library(shiny)
library(dplyr)
library(ggplot2)
library(viridis)

# read in the pollination dependence data
map_data <- readRDS("data/global_change_pollination_dependence.rds")

years <- seq(2006, 2050, 1)

names(map_data) <- 


# read in the grey background basemap
map_fort <- readRDS("data/plot_base_map.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Change in global pollination dependence vulnerability"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("maplot"),
           sliderInput("year",
                       "Year", min = 2006, 
                       max = 2050, value = 2006, 
                       animate = TRUE)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

        output$maplot<- renderPlot({
    
            # plot the ggplot map for climate anomaly
            ggplot() +
                geom_polygon(aes(x = long, y = lat, group = group), data = map_fort, fill = "grey", alpha = 0.3) +
                geom_tile(aes(x = x, y = y, fill = poll_vulnerability), data = map_data[[input$year]]) +
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

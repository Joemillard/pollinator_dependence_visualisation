# shiny app for visualising change in pollination dependent vulnerability

library(shiny)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggiraph)

# read in the grey background basemap
map_fort <- readRDS("data/plot_base_map.rds") %>%
    mutate(country_label = gsub("\\.\\d+", "", group))

# read in the production data
total_production <- readRDS("data/global_change_production.rds")

# read in the change in index data for each country
country_change <- readRDS("data/country_change_pollination_dependence.rds") %>%
    filter(!is.na(SOVEREIGNT)) %>%
    filter(!is.na(total))

# selection of years and empty year list
years <- 2048:2050
years_list <- c()

# set up list of years
for(i in 1:33){
    years <- years - 1
    years_list[i] <- years
}


# Define UI for application with map, slider for year, and change in vulnerability
ui <- shinyUI(fluidPage(
    tags$head(tags$style(
        HTML('
         #sidebar {
            background-color: #FFFFFF;
            border-color: #000000;
            box-shadow: inset 0 0px 0px rgb(0 0 0 / 5%);
        }
        body, label, input, button, select { 
          font-family: "Arial";
        }')
    )),
    
    # Application title
    titlePanel("Change in global pollination dependence vulnerability"),
    
    # nav bar with two panels for global and country plots
    navbarPage("Menu",
               # Global tab
               tabPanel("Global",
                        sidebarLayout(
                            sidebarPanel(id="sidebar",
                                         sliderInput("year",
                                                     "Select year", min = 2016, 
                                                     max = 2047, value = 2016, 
                                                     animate = TRUE, sep = ""),
                                         
                                         h5("Projected change in total vulnerability-weighted pollination dependent production under three RCP scenarios (8.5, 6.0, and 2.6), jack-knifed for each climate model.")),
                            mainPanel(
                                plotOutput("production_change"),                            )
                        )),
               # specific country tab
               tabPanel("Country",
                        sidebarLayout(
                            sidebarPanel(id="sidebar",
                                         ggiraphOutput("select_map")),
                                         
                            mainPanel(
                                plotOutput("country_change"),
                            )
                        )
               )
    )
))


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$select_map <- renderggiraph({
        
        # plot the ggplot map for climate anomaly
        global_map <- ggplot() +
            ggtitle("Select country") +
            geom_polygon_interactive(aes(x = long, y = lat, group = group, tooltip = country_label, data_id = group), data = map_fort, fill = "grey") +
            coord_equal() +
            theme(panel.background = element_blank(),
                  panel.bord = element_rect(colour = "black", fill=NA, size=2),
                  panel.grid = element_blank(), 
                  axis.text = element_blank(),
                  axis.ticks = element_blank(), 
                  axis.title = element_blank(),
                  legend.position = "right",
                  plot.title = element_text(face = "bold", size = 20, margin = margin(0,0,10,0)))
        
        ggiraph(code = print(global_map), selection_type = "single")
        
    }) %>% bindCache(input$year)
    
    # plot of total production vulnerability for each country
    output$country_change <-  renderPlot({
        country_change %>%
            filter(SOVEREIGNT %in% gsub("\\.\\d+", "", input$select_map_selected)) %>% 
            ggplot() +
            geom_ribbon(aes(x = year, ymin = lower_conf, ymax = upp_conf), fill = "white", colour = "grey", alpha = 0.2, linetype = "dashed") +
            geom_line(aes(x = year, y = total, colour = total), size = 0.8) +
            scale_colour_viridis("", na.value = "transparent", option = "plasma", direction = -1,
                                 limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.5", "0.75", "1")) +
            scale_y_continuous(limits = c(0, 1), labels = c("0", "0.25", "0.5", "0.75", "1"), expand = c(0, 0)) +
            scale_x_continuous(breaks = c(2020, 2030, 2040), labels = c(2020, 2030, 2040)) +
            ylab("Vulnerability-weighted \npollination dependence") +
            xlab("Year") +
            theme_bw() +
            guides(guide_colourbar(ticks = FALSE)) +
            theme(panel.grid = element_blank(),
                  legend.position = "none", strip.text = element_text(size = 10.5), text = element_text(size = 17))
    })
    
    output$production_change <-  renderPlot({
        
        total_production %>%
            filter(year <= input$year) %>%
            ggplot() +
            geom_line(aes(x = year, y = vulnerability, colour = model, alpha = model)) +
            geom_point(aes(x = year, y = vulnerability, colour = model, alpha = model)) +
            facet_wrap(~scenario, ncol = 2) +
            scale_y_continuous(limits = c(1700000, 4100000), expand = c(0, 0), 
                               breaks = c(2000000, 2500000, 3000000, 3500000, 4000000), 
                               labels = c("2,000,000", "2,500,000", "3,000,000", "3,500,000", "4,000,000")) +
            scale_x_continuous(limits = c(2015, 2050), expand = c(0, 0), breaks = c(2020, 2030, 2040, 2050)) +
            scale_colour_manual("Climate model", values = c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
            scale_alpha_manual("Climate model", values = c(1, 0.4, 0.4, 0.4, 0.4)) +
            ylab("Vulnerability-weighted pollination prod. (metric tonnes)") +
            xlab("") +
            theme_bw() +
            theme(panel.grid = element_blank(), legend.position = "right", text = element_text(size = 15))
        
    }) %>% bindCache(input$year)


}  

# Run the application 
shinyApp(ui = ui, server = server)

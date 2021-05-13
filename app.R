# shiny app for visualising change in pollination dependent vulnerability

library(shiny)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggiraph)
library(forcats)

# read in the grey background basemap
map_fort <- readRDS("data/plot_base_map.rds") %>%
    mutate(country_label = gsub("\\.\\d+", "", group))

# read in the production data
total_production <- readRDS("data/global_change_production.rds")

# read in the change in index data for each country
country_change <- readRDS("data/country_change_pollination_dependence.rds") %>%
    filter(!is.na(SOVEREIGNT)) %>%
    filter(!is.na(total))

# read in total production for histogram
crop_change <- readRDS("data/global_change_production_crops.rds")

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
    titlePanel("Change in global pollination risk"),
    
    # nav bar with two panels for global and country plots
    navbarPage("Menu",
               # Global tab
               tabPanel("Global scale",
                        sidebarLayout(
                            sidebarPanel(id="sidebar",
                                         sliderInput("year",
                                                     "Select a year", min = 2016, 
                                                     max = 2047, value = 2016, 
                                                     animate = TRUE, sep = "", tick = 1),
                                         plotOutput("crop_prod_change"),
                                         
                                         h5(style="text-align: justify;", ("Projected total production crop vulnerability for the top 20 crops by total pollination dependent production, for the years 2016 to x (where x = the selected year)."))),
                                
                                         
                            mainPanel(
                                plotOutput("production_change"),
                                h5(style="text-align: justify;","Projected change in total vulnerability-weighted pollination dependent production under RCP 8.5, for the years 2016- Colours refer to the climate model excluded in that jack-knife projection (orange, excluding GFDL; blue excluding HadHEM2, green, excluding IPSL; yellow, excluding MIROC5), with the projection for all models included in black.
                                   For each year into the future a standardised climate anomaly was projected globally, using a 3 year rolling average. For each annual projection of standardised climate anomaly, insect pollinator abundance on cropland was predicted according to a mixed effects linear model, and then adjusted to a percentage decline from cropland regions that have experienced no warming (i.e. standardised climate anomaly of 0). In each cell pollination dependent production was then adjusted for the percentage reduction in abundance at that cell, before summing pollination dependent production for all cells at each time step.")),
                        )),
               # specific country tab
               tabPanel("Country scale",
                        sidebarLayout(
                            sidebarPanel(id="sidebar",
                                         span(textOutput("selected_country"), style="font-size: 20px; font-weight: bold;"),
                                         ggiraphOutput("select_map")),
                                         
                            mainPanel(
                                plotOutput("country_change"),
                                h5(style="text-align: justify;", "Climate change vulnerability-weighted pollination dependence projected under RCP scenario 8.5 from the average of four climate models (GFDL, HadGEM2, IPSL, and MIROC5), for each selected country. Global standardised climate anomaly was projected for all areas of pollination-dependent cropland to 2050, using a 3 year rolling average. For each value of standardised climate anomaly, insect pollinator abundance was predicted according to a mixed effects model. Insect pollinator abundance at each cell at each time step was then adjusted to a percentage decline from cropland regions that have experienced no warming (i.e. standardised climate anomaly of 0). Pollination dependent production at each cell was then adjusted for the predicted loss in insect pollinator abundance, and then converted to a proportion of the total production at that cell.
                                   Colours correspond to the total vulnerability-weighted pollination dependence in each cell at that time (i.e. the y axis): 1, dark purple; 0.5, orange, and 0, yellow. A value of 1 indicates a hypothetical region in which all crop production in that cell is dependent on pollination, and predicted insect pollinator abundance loss is 100%. Grey dashed lines represent the 2.5th and 97.5th percentiles for the cells in that country, providing an indication of vulnerability variation within a country.")
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
            ggtitle("Select a country") +
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
            filter(scenario == "RCP 8.5") %>%
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
            ylab("Total crop production at risk (metric tonnes)") +
            xlab("") +
            theme_bw() +
            theme(panel.grid = element_blank(), legend.position = "right", text = element_text(size = 15), plot.caption = element_text(hjust = 0))
        
    }) %>% bindCache(input$year)

    output$crop_prod_change <-  renderPlot({
        
        crop_change %>%
            filter(year == input$year) %>%
            mutate(crop = fct_reorder(crop, -production_prop)) %>%
            ggplot() +
                geom_bar(aes(x = crop, y = production_prop), stat = "identity", fill = "black") +
                scale_y_continuous("Crop production at risk (metric tonnes)", expand = c(0, 0), limits = c(0, 52126437)) +
                xlab("Crop") +
                theme_bw() +
                theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
        
    })
    
    output$selected_country<- renderText({gsub("\\.\\d+", "", input$select_map_selected)})

}  

# Run the application 
shinyApp(ui = ui, server = server)

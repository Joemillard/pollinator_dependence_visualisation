# shiny app for visualising change in pollination dependent vulnerability

library(shiny)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggiraph)
library(forcats)
library(data.table)

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

# read in production for each country
country_production <- readRDS("data/country_pollination_dependent_production.rds")

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
    span(titlePanel("Global patterns in crop pollination vulnerability to land-use and climate change"), style="color: #000000"),
    
    # nav bar with two panels for global and country plots
    navbarPage("Menu",
               tabPanel("Overview",
                        fluidRow(
                            column(6,
                                   h3(style = "font-weight: bold;", "Overview"),
                                   h5(style="text-align: justify;", "This dashboard combines six datasets to predict temporal and spatial vulnerabilities of crop pollination that that may result from future climate change: the PREDICTS database (Hudson et al. 2017), a set of likely pollinating and non-pollinating species derived previously (Millard et al. 2021), the historical degree of global climate change (Harris et al. 2020), future scenarios of climate change, a set of rasters for global crop production (Monfreda et al. 2008), and ratios of crop pollination dependence (Klein et al. 2007)."),
                                   h5(style="text-align: justify;", "In the first tab ('Country scale') you'll find predictions for change in crop pollination risk for all countries of the world, projected under RCP 8.5. In the second tab ('Global scale') you'll find predictions for change in total pollination production at risk.
                                      All projections of risk to crop pollination are based on a space-for-time prediction of insect pollinator abundance losses resulting from climate change (see figure opposite).")),
                            column(6,
                                   img(style="display: block; margin-left: auto; margin-right: auto;", src = "Fig_1.png", height = 600, width = 450),
                                   h5(style="text-align: justify;", "Response of pollinating and non-pollinating total abundance to standardised climate anomaly on primary vegetation and cropland (note that abundance is plotted on a log scale). Each panel represents a linear mixed-effects model for pollinating or non-pollinating insects and vertebrates. Coloured lines represent median fitted estimates for each interaction, and shading 95% confidence intervals around that prediction: green, primary vegetation; orange, cropland.")
                            ))),
               tabPanel("Country scale",
                        sidebarLayout(
                            sidebarPanel(id="sidebar",
                                         span(textOutput("selected_country"), style="font-size: 24px; font-weight: bold;"),
                                         ggiraphOutput("select_map")),
                            mainPanel(
                                plotOutput("country_change"),
                                h5(style="text-align: justify;", "Climate change pollination dependence risk projected under RCP scenario 8.5 from the average of four climate models (GFDL, HadGEM2, IPSL, and MIROC5), for each selected country. Global standardised climate anomaly was projected for all areas of pollination-dependent cropland to 2050, using a 3 year rolling average. For each value of standardised climate anomaly, insect pollinator abundance was predicted according to a mixed effects linear model. Insect pollinator abundance at each cell at each time step was then adjusted to a percentage decline from cropland regions that have experienced no warming (i.e. standardised climate anomaly of 0). Pollination dependent production at each cell was then adjusted for the predicted loss in insect pollinator abundance, and then converted to a proportion of the total production at that cell.
                                   The coloured line here corresponds to the median pollination dependence risk for all cells in that country at that time step: 1, dark purple; 0.5, orange, and 0, yellow. A value of 1 indicates a hypothetical region in which all crop production in that cell is dependent on pollination, and predicted insect pollinator abundance loss is 100%. Grey dashed lines represent the 2.5th and 97.5th percentiles for the cells in that country at that time step, providing an indication of vulnerability variation within a country."),
                                plotOutput("country_production"),
                                h5(style="text-align: justify;", "Total pollination dependent production for the 20 crops with the highest pollination dependent production, ordered by magnitude for the selected country. Total production values are for the year 2000, taken from Monfreda et al. Pollination dependent production is calculated by multiplying total crop production for each crop by the pollination dependence ratios for that crop, as reported in Klein et al (2007). For any Monfreda crop represented by multiple dependence ratios, we took the pollination dependence to be the mean of the ratios for that crop. 
                                   Colours represent the mean pollination dependent production for that Monfreda et al crop (i.e. 0.95, essential; 0.65, great; modest/great, 0.45; modest, 0.25; little, 0.05). NE refers to any set of crops not included elsewhere.")
                            ))),
               tabPanel("Global scale",
                        sidebarLayout(
                            sidebarPanel(id="sidebar",
                                         sliderInput("year",
                                                     "Select a year:", min = 2016, 
                                                     max = 2047, value = 2016, 
                                                     animate = TRUE, sep = "", tick = 1),
                                         plotOutput("crop_prod_change"),
                                         h5(style="text-align: justify;", ("Predicted crop production risk for the 20 crops with the greatest pollination dependent production, for the years 2016 to x (where x = selected year). Pollination dependent production is calculated by multiplying total crop production for each crop by the pollination dependence ratios for that crop, as reported in Klein et al (2007) (i.e. 0.95, essential; 0.65, great; modest/great, 0.45; modest, 0.25; little, 0.05). For any Monfreda crop represented by multiple dependence ratios, we took the pollination dependence to be the mean of the ratios for that crop."))),
                            mainPanel(
                                plotOutput("production_change"),
                                h5(style="text-align: justify;","Predicted change in total crop production risk (for all crops) under RCP 8.5, for the years 2016 to x (where x = selected year). Colours refer to the climate model excluded in that jack-knife projection (orange, excluding GFDL; blue excluding HadHEM2, green, excluding IPSL; yellow, excluding MIROC5), with the projection for all models included in black.
                                   For each year into the future a standardised climate anomaly was projected globally, using a 3 year rolling average. For each annual projection of standardised climate anomaly, insect pollinator abundance on cropland was predicted according to a mixed effects linear model, and then adjusted to a percentage decline from cropland regions that have experienced no warming (i.e. standardised climate anomaly of 0). In each cell pollination dependent production was then adjusted for the percentage reduction in abundance at that cell, before summing pollination dependent production for all cells at each time step.")),
                        )
               )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # output the map for selecting countries
    output$select_map <- renderggiraph({
        global_map <- ggplot() +
            ggtitle("Select a country:") +
            geom_polygon_interactive(aes(x = long, y = lat, group = group, tooltip = country_label, data_id = group), data = map_fort, fill = "grey") +
            coord_equal() +
            theme(panel.background = element_blank(),
                  plot.background = element_blank(),
                  panel.bord = element_rect(colour = "black", fill=NA, size=2),
                  panel.grid = element_blank(), 
                  axis.text = element_blank(),
                  axis.ticks = element_blank(), 
                  axis.title = element_blank(),
                  legend.position = "right",
                  plot.title = element_text(face = "bold", size = 20, margin = margin(0,0,10,0)),
                  plot.margin = unit(c(0, 0, 0, 0), "cm"))
        
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
            ylab("Pollination dependence risk") +
            xlab("Year") +
            theme_bw() +
            guides(guide_colourbar(ticks = FALSE)) +
            theme(panel.grid = element_blank(),
                  legend.position = "none", strip.text = element_text(size = 10.5), text = element_text(size = 17))
    })
    
    # plot of change in global change in pollination dependent production
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
            ylab("Total crop production risk (metric tonnes)") +
            xlab(NULL) +
            theme_bw() +
            theme(panel.grid = element_blank(), legend.position = "right", text = element_text(size = 15), plot.caption = element_text(hjust = 0))
    }) %>% bindCache(input$year)

    # plot of total pollination dependent production for the top 20 crops in each country
    output$country_production <- renderPlot({
        country_production %>%
            filter(SOVEREIGNT %in% gsub("\\.\\d+", "", input$select_map_selected)) %>% 
            filter(total_production != 0) %>%
            mutate(crop = forcats::fct_reorder(crop, -total_production)) %>%
            ggplot() +
                geom_bar(aes(x = crop, y = total_production, fill = pollination_dependence), stat = "identity") +
                xlab("Crop") +
                scale_y_continuous("Pollination dependent prod. (m. tonnes)", 
                                   expand = c(0, 0), 
                                   limits = c(0, max(country_production$total_production[country_production$SOVEREIGNT == gsub("\\.\\d+", "", input$select_map_selected)], na.rm = TRUE) * 1.2)) +
                scale_fill_viridis("Average pollination \ndependence ratio") +
                theme_bw() +
                theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 14))
    })
    
    # plot for the change in total global crop production risk for each crop
    output$crop_prod_change <-  renderPlot({
        crop_change %>%
            filter(year == input$year) %>%
            mutate(crop = fct_reorder(crop, -production_prop)) %>%
            ggplot() +
                geom_bar(aes(x = crop, y = production_prop), stat = "identity", fill = "black") +
                scale_y_continuous("Crop production risk (metric tonnes)", expand = c(0, 0), limits = c(0, 52126437)) +
                xlab(NULL) +
                theme_bw() +
                theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$selected_country<- renderText({gsub("\\.\\d+", "", input$select_map_selected)})
}  

# Run the application 
shinyApp(ui = ui, server = server)

#author: "Hun Lee"
#date: "12/04/2021"
#NYC Map


library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(sf)
library(shinyWidgets)


map_dataset <- readRDS("map_dataset.RDS")
map_dataset_label <- readRDS("map_dataset_label.RDS")



#variable_type <-
#  tibble(variable_type = c("Hosp Rate 2020", "Death Rate 2020", "Hosp Rate 2021", "Death Rate 2021", "Vacc Rate"))

#variable_type <- variable_type %>% 
#  distinct(variable_type) %>% 
#  pull
#sidebarLayout(
#  sidebarPanel(
#    selectInput("variable_type", label = h3(""),
#                choices = variable_type, 
#                selected = "None")),

#variable_type <- ("None")

# Define UI for application that draws a histogram
ui = fluidPage(
        tags$h2(""),
        setBackgroundImage(
          src = "https://picstatio.com/download/3840x2160/-qdq-h/new-york-rockefeller-center-sky-sunrise-wallpaper.jpg"
        ),
    
    titlePanel("COVID-19 Trend in NYC"), 

        mainPanel(
          tabsetPanel(
                      tabPanel("Hospitalization Rate in 2020", leafletOutput("hosp_2020")),
                      tabPanel("Death Rate in 2020", leafletOutput("death_2020")),
                      tabPanel("Hospitalization Rate in 2021", leafletOutput("hosp_2021")),
                      tabPanel("Death Rate in 2021", leafletOutput("death_2021")),
                      tabPanel("Vaccination Rate", leafletOutput("vacc"))
                      )
          )
    )


      

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$hosp_2020 <- renderLeaflet({
        
    labels <- sprintf("<strong>%s</strong> <br/> %s <br/> %s <br/> %s <br/> %s <br/> %s <br/> Median Household Income: $%s <br/> Hospitalization Rate: %g per 100,000 people", 
                      map_dataset$puma_name, map_dataset_label$label1, map_dataset_label$label2, map_dataset_label$label3, map_dataset_label$label4, map_dataset_label$label5, 
                      map_dataset$median_household_income, map_dataset$covid_hosp_rate_2020) %>% lapply(htmltools::HTML)

    pal <- colorBin(palette = "OrRd", 9, domain = map_dataset$covid_hosp_rate_2020)
    
    map_dataset %>% st_transform(crs = "+init=epsg:4326") %>%
        leaflet() %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", 
                                   padding = "1px 2px"),
                      textsize = "11px",  sticky = TRUE,
                      opacity = 0.55
                    ),
                    stroke = FALSE,
                    opacity = 1,
                    smoothFactor = .5,
                    fillOpacity = .7,
                    fillColor = ~pal(covid_hosp_rate_2020),
                    highlightOptions = highlightOptions(weight = 5, 
                                                        fillOpacity = 1,
                                                        color = "black",
                                                        opacity = 1,
                                                        bringToFront = TRUE
                    )) %>%
        addLegend("bottomright",
                  pal = pal,
                  values = ~covid_hosp_rate_2020,
                  title = "Covid Hosp Rate per 100,000 in 2020",
                  opacity = 0.7,
        )
        
    })
    
    output$death_2020 <- renderLeaflet({
      labels <- sprintf("<strong>%s</strong> <br/> %s <br/> %s <br/> %s <br/> %s <br/> %s <br/> Median Household Income: $%s <br/> Covid Death Rate: %g per 100,000 people", 
                        map_dataset$puma_name, map_dataset_label$label1, map_dataset_label$label2, map_dataset_label$label3, map_dataset_label$label4, map_dataset_label$label5, 
                        map_dataset$median_household_income,map_dataset$covid_death_rate_2020) %>% lapply(htmltools::HTML)
      
      pal <- colorBin(palette = "OrRd", 9, domain = map_dataset$covid_death_rate_2020)
      
      map_dataset %>% st_transform(crs = "+init=epsg:4326") %>%
        leaflet() %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", 
                                   padding = "1px 2px"),
                      textsize = "11px",  sticky = TRUE,
                      opacity = 0.55
                    ),
                    stroke = FALSE,
                    opacity = 1,
                    smoothFactor = .5,
                    fillOpacity = .7,
                    fillColor = ~pal(covid_death_rate_2020),
                    highlightOptions = highlightOptions(weight = 5, 
                                                        fillOpacity = 1,
                                                        color = "black",
                                                        opacity = 1,
                                                        bringToFront = TRUE
                    )) %>%
        addLegend("bottomright",
                  pal = pal,
                  values = ~covid_death_rate_2020,
                  title = "Covid Death Rate per 100,000 in 2020",
                  opacity = 0.7
        )
      
    })
    
    
    
    output$hosp_2021 <- renderLeaflet({
      
      labels <- sprintf("<strong>%s</strong> <br/> %s <br/> %s <br/> %s <br/> %s <br/> %s <br/> Median Household Income: $%s <br/> Hospitalization Rate: %g per 100,000 people", 
                        map_dataset$puma_name, map_dataset_label$label1, map_dataset_label$label2, map_dataset_label$label3, map_dataset_label$label4, map_dataset_label$label5, 
                        map_dataset$median_household_income,map_dataset$covid_hosp_rate_2021) %>% lapply(htmltools::HTML)
      
      pal <- colorBin(palette = "Greens", 9, domain = map_dataset$covid_hosp_rate_2021)
      
      map_dataset %>% st_transform(crs = "+init=epsg:4326") %>%
        leaflet() %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", 
                                   padding = "1px 2px"),
                      textsize = "11px",  sticky = TRUE,
                      opacity = 0.55
                    ),
                    stroke = FALSE,
                    opacity = 1,
                    smoothFactor = .5,
                    fillOpacity = .7,
                    fillColor = ~pal(covid_hosp_rate_2021),
                    highlightOptions = highlightOptions(weight = 5, 
                                                        fillOpacity = 1,
                                                        color = "black",
                                                        opacity = 1,
                                                        bringToFront = TRUE
                    )) %>%
        addLegend("bottomright",
                  pal = pal,
                  values = ~covid_hosp_rate_2021,
                  title = "Covid Hosp Rate per 100,000 in 2021",
                  opacity = 0.7
        )
      
    })
    
    
    output$death_2021 <- renderLeaflet({
      labels <- sprintf("<strong>%s</strong> <br/> %s <br/> %s <br/> %s <br/> %s <br/> %s <br/> Median Household Income: $%s <br/> Covid Death Rate: %g per 100,000 people", 
                        map_dataset$puma_name, map_dataset_label$label1, map_dataset_label$label2, map_dataset_label$label3, map_dataset_label$label4, map_dataset_label$label5, 
                        map_dataset$median_household_income,map_dataset$covid_death_rate_2021) %>% lapply(htmltools::HTML)
      
      pal <- colorBin(palette = "Greens", 9, domain = map_dataset$covid_death_rate_2021)
      
      map_dataset %>% st_transform(crs = "+init=epsg:4326") %>%
        leaflet() %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", 
                                   padding = "1px 2px"),
                      textsize = "11px",  sticky = TRUE,
                      opacity = 0.55
                    ),
                    stroke = FALSE,
                    opacity = 1,
                    smoothFactor = .5,
                    fillOpacity = .7,
                    fillColor = ~pal(covid_death_rate_2021),
                    highlightOptions = highlightOptions(weight = 5, 
                                                        fillOpacity = 1,
                                                        color = "black",
                                                        opacity = 1,
                                                        bringToFront = TRUE
                    )) %>%
        addLegend("bottomright",
                  pal = pal,
                  values = ~covid_death_rate_2021,
                  title = "Covid Death Rate per 100,000 in 2021",
                  opacity = 0.7
        )
      
    })
    
    output$vacc <- renderLeaflet({
        labels <- sprintf("<strong>%s</strong> <br/> %s <br/> %s <br/> %s <br/> %s <br/> %s <br/> Median Household Income: $%s <br/> Covid Vaccination Rate: %g percent", 
                          map_dataset$puma_name, map_dataset_label$label1, map_dataset_label$label2, map_dataset_label$label3, map_dataset_label$label4, map_dataset_label$label5, 
                          map_dataset$median_household_income,map_dataset$covid_vacc_rate) %>% lapply(htmltools::HTML)
        
        pal <- colorBin(palette = "PuBu", 9, domain = map_dataset$covid_vacc_rate)
        
        map_dataset %>% st_transform(crs = "+init=epsg:4326") %>%
            leaflet() %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            addPolygons(label = labels,
                        labelOptions = labelOptions(
                          style = list("font-weight" = "normal", 
                                       padding = "1px 2px"),
                          textsize = "11px",  sticky = TRUE,
                          opacity = 0.55
                        ),
                        stroke = FALSE,
                        opacity = 1,
                        smoothFactor = .5,
                        fillOpacity = .7,
                        fillColor = ~pal(covid_vacc_rate),
                        highlightOptions = highlightOptions(weight = 5, 
                                                            fillOpacity = 1,
                                                            color = "black",
                                                            opacity = 1,
                                                            bringToFront = TRUE
                        )) %>%
            addLegend("bottomright",
                      pal = pal,
                      values = ~covid_vacc_rate,
                      title = "Covid Vaccination Rate as of 11/16/2021",
                      opacity = 0.7
            )
        
    })
}        

# Run the application 
shinyApp(ui = ui, server = server)

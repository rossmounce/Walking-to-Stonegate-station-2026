
getwd()
setwd("C:\\Users\\RossMounce\\Downloads\\observations-751718.csv")



library(leaflet)
library(leaflet.extras2)
library(sf)
library(shiny)
library(tidyverse)
library(lubridate)


obs <- read_csv("2026only.csv", show_col_types = FALSE) %>%
  mutate(
    obs_time = ymd_hms(time_observed_at, tz = "UTC")
  ) %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(obs_time)
  ) %>%
  arrange(obs_time) %>%
  mutate(obs_number = row_number())


# Preload all images into browser cache
preload_html <- paste0(
  "<img src='",
  obs$image_url,
  "' style='display:none;'>",
  collapse = "\n"
)



ui <- fluidPage(
  
  tags$head(
    
    tags$script(HTML(
      "
document.addEventListener('keydown', function(e) {

  if(e.code === 'Space') {

    e.preventDefault();

    var btn =
      document.querySelector('.slider-animate-button');

    if(btn) {
      btn.click();
    }
  }

});
"
    ))
  ),
  
  
  # Hidden image cache
  tags$div(
    HTML(preload_html),
    style = "display:none;"
  ),
  
  
  titlePanel('Things I saw on my walk to Stonegate'),
  
  
  fluidRow(
    
    # LEFT COLUMN
    column(
      width = 5,
      
      uiOutput("obs_image"),
      
      br(),
      
      uiOutput("species_info"),
      
      br(),
      
      sliderInput(
        "n",
        "Observation",
        min = 1,
        max = nrow(obs),
        value = 1,
        step = 1,
        animate = animationOptions(
          interval = 2000,
          loop = FALSE
        )
      )
    ),
    
    # RIGHT COLUMN
    column(
      width = 7,
      
      leafletOutput(
        "map",
        height = "750px"
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    leaflet(obs) %>%
      addProviderTiles(providers$Stadia.Outdoors) %>%
      fitBounds(
        min(obs$longitude),
        min(obs$latitude),
        max(obs$longitude),
        max(obs$latitude)
      )
  })
  
  
  current_data <- reactive({
    obs[1:floor(input$n), ]
  })
  
  current_obs <- reactive({
    obs[floor(input$n), ]
  })
  
  observe({
    
    dat <- current_data()
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      
      addPolylines(
        lng = dat$longitude,
        lat = dat$latitude,
        color = "steelblue",
        weight = 3
      ) %>%
      
      addCircleMarkers(
        lng = dat$longitude,
        lat = dat$latitude,
        radius = 5,
        color = "darkgreen",
        fillOpacity = 0.7,
        popup = paste0(
          "<b>", dat$common_name, "</b><br>",
          dat$scientific_name
        )
      ) %>%
      
      addCircleMarkers(
        lng = tail(dat$longitude, 1),
        lat = tail(dat$latitude, 1),
        radius = 10,
        color = "red",
        fillColor = "red",
        fillOpacity = 1
      )
  })
  
  
  output$species_info <- renderUI({
    
    x <- current_obs()
    
    tagList(
      
      tags$h3(x$common_name),
      
      tags$p(
        tags$em(x$scientific_name)
      ),
      
      tags$p(
        strong("Observed: "),
        format(x$obs_time, "%d %b %Y %H:%M")
      ),
      
      tags$p(
        strong("Location: "),
        x$place_guess
      )
    )
  })
  
  
  output$obs_image <- renderUI({
    
    x <- current_obs()
    
    tags$a(
      href = x$url,
      target = "_blank",
      
      tags$img(
        src = x$image_url,
        style = paste(
          "width:100%;",
          "max-height:550px;",
          "object-fit:contain;",
          "border:2px solid #cccccc;",
          "border-radius:8px;"
        )
      )
    )
  })
  
  
}

shinyApp(ui, server)


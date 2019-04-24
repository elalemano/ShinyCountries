#### ==== SET UP ====####

require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(shinydashboardPlus)
require(httr)
require(tidyverse)
require(plotly)
require(leaflet)
require(viridis)
require(DT)


#### ==== GLOBAL ====####

source("utils/namedListToFrame.R")

res <- GET("https://restcountries.eu/rest/v2/all")
data <- res %>%
  content() %>%
  lapply(unlist) %>%
  lapply(namedListToFrame) %>%
  bind_rows() %>%
  dplyr::filter(
    cioc != "" &
      !is.na(area)
  ) %>%
  select(
    name, alpha3Code, callingCodes, capital,
    region, subregion, population, latlng1,
    latlng2, demonym, area, gini, numericCode,
    currencies.code, currencies.name, languages.name,
    flag, regionalBlocs.acronym, regionalBlocs.name,
    cioc
  ) %>%
  mutate_at(c("population", "latlng1", "latlng2", "area", "gini"), as.numeric) %>%
  mutate(
    name = name %>% gsub("\\([^\\)]+\\)", "", .),
    density = (population / area) %>% round(2),
    country_names_pops = paste0("<b>", name, "</b>"),
    country_images_pops = paste0("<img src='", flag, "' style='width:100%;height:100%;'>"),
    pop_ups = paste(country_images_pops, country_names_pops, sep = "<br>")
  )

#### ==== UI ====####
ui <- dashboardPagePlus(
  skin = "green-light",
  dashboardHeaderPlus(
    title = tagList(
      span(class = "logo-lg", "Shiny Countries"),
      img(src = "mini_world.svg")
    ),

    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears",
    left_menu = tagList(
      dropdownBlock(
        id = "dd1",
        title = "Region",
        icon = icon("sliders"),
        badgeStatus = NULL,
        prettyCheckboxGroup("regions", label = NULL, thick = TRUE, animation = "pulse", status = "success", choices = data$region %>% unlist() %>% unique() %>% sort(), selected = data$region %>% unlist() %>% unique() %>% sort())
      ),
      dropdownBlock(
        id = "dd2",
        title = "Country",
        icon = icon("sliders"),
        badgeStatus = NULL,
        selectizeInput("countries", multiple = T, label = NULL, choices = data$name %>% unlist() %>% unique() %>% sort(), selected = NULL)
      ),
      dropdownBlock(
        id = "dd3",
        title = "Population",
        icon = icon("sliders"),
        badgeStatus = NULL,
        sliderInput("population", label = NULL, min = min(data$population), max = max(data$population), value = c(min(data$population), max(data$population)))
      ),
      dropdownBlock(
        id = "dd4",
        title = "Area",
        icon = icon("sliders"),
        badgeStatus = NULL,
        sliderInput("area", label = NULL, min = min(data$area), max = max(data$area), value = c(min(data$area), max(data$area)))
      ),
      dropdownBlock(
        id = "dd5",
        title = "Country Code",
        icon = icon("sliders"),
        badgeStatus = NULL,
        selectizeInput("country_code", multiple = T, label = NULL, choices = data$alpha3Code %>% unlist() %>% unique() %>% sort(), selected = NULL)
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Map",
        tabName = "map",
        icon = icon("map")
      ),
      menuItem(
        text = "Comparisons",
        tabName = "comparison",
        icon = icon("chart-bar")
      ),
      menuItem(
        text = "Country Details",
        tabName = "details",
        icon = icon("compress")
      )
    )
  ),
  dashboardBody(

    #### ---- CSS ----####
    tags$head(
      tags$style(
        HTML("")
      ),
      #### ---- JS ----####
      tags$script(src = "enter_button.js")
    ),

    tabItems(
      tabItem(
        tabName = "map",
        box(
          width = 12, height = 820, status = "success",
          leafletOutput("worldMap", width = "100%", height = 800)
        )
      ),
      tabItem(
        tabName = "comparison",
        box(
          width = 6, status = "success", height = "800px",
          plotlyOutput("bar_area", height = 750)
        ),
        box(
          width = 6, status = "success", height = "800px",
          plotlyOutput("scatter_dens", height = 750)
        )
      ), tabItem(
        tabName = "details",
        box(
          width = 12, status = "success",
          dataTableOutput("detail_table")
        )
      )
    )
  ),
  rightSidebar(
    background = "light",
    rightSidebarTabContent(
        id = 1,
        icon = "info",
        active = TRUE,
        
              a(actionButton('mail',"Send feedback", width = '100%', icon = shiny::icon("envelope")), href='mailto:l_busswinkel@hotmail.de'),
      actionButton('github',"Check the code on Github",width = '100%', icon = shiny::icon("github"), onclick ="window.open('https://github.com/elalemano/ShinyCountries', '_blank')"),
        actionButton('twitter',"Follow me on Twitter",width = '100%', icon = shiny::icon("twitter"),  onclick ="window.open('https://twitter.com/LycopersiconLBB', '_blank')"), 
         actionButton('linked_in', 'Get in touch on LinkedIn',width = '100%',icon = shiny::icon("linkedin"), onclick ="window.open('https://linkedin.com/in/lukas-busswinkel', '_blank')")
        ),
    rightSidebarTabContent(
        id = 2,
        icon = "database",
        active = F,
         actionButton('database', 'Data source',width = '100%',icon = shiny::icon("external-link"), onclick ="window.open('https://restcountries.eu/', '_blank')")
  )),
  dashboardFooter(left_text = "Created 2019", right_text = "by Lukas Busswinkel")
)

#### ==== SERVER ====####

server <- function(input, output, session) {

  #### ---- Filter Data ----####
  filterData <- reactive({
    newData <- data

    if (!is.null(input$countries)) {
      newData <- newData %>% dplyr::filter(name %in% input$countries)
    }
    if (!is.null(input$country_code)) {
      newData <- newData %>% dplyr::filter(alpha3Code %in% input$country_code)
    }

    newData <- newData %>% dplyr::filter(region %in% input$regions &
      population >= input$population[1] &
      population <= input$population[2] &
      area >= input$area[1] &
      area <= input$area[2])
    return(newData)
  })




  #### ---- World Map ----####
  output$worldMap <- renderLeaflet({
    leaflet(data = filterData()) %>%
      setView(15, 30, 3) %>%
      addProviderTiles(providers$Stamen.TonerLite, providerTileOptions(maxZoom = 6)) %>%
      addMarkers(lng = ~latlng2, lat = ~latlng1, popup = ~pop_ups, popupOptions = popupOptions(maxWidth = 150, minWidth = 150))
  })


  #### ---- Area bar chart ----####
  output$bar_area <- renderPlotly({
    plotData <- filterData() %>% dplyr::filter(!is.na(area))

    plotData$name <- factor(plotData$name,
      levels = unique(plotData$name[order(plotData$area)]),
      ordered = T
    )

    plot_ly(data = plotData, y = ~name, x = ~area, type = "bar", marker = list(
      color = "rgb(5, 104, 4)"
    )) %>%
      layout(title = "Country total area", xaxis = list(title = "Area in km²"), yaxis = list(title = ""))
  })

  #### ---- Density scatter chart ----####
  output$scatter_dens <- renderPlotly({
    plotData <- filterData() %>%
      dplyr::filter(!is.na(area) & !is.na(population))

    plot_ly(
      data = plotData, y = ~population, x = ~area, text = ~name,
      type = "scatter", mode = "markers",
      marker = list(color = ~ log(density), colorscale = "Viridis", size = 15, opacity = 0.75, colorbar = list(
        title = "log(Density)"
      ))
    ) %>%
      layout(
        title = "Population vs Area (log scales)", xaxis = list(title = "Area in km²", type = "log"),
        yaxis = list(title = "Total population", type = "log"), showlegend = F
      )
  })

  #### ---- Data table with details ----####
  output$detail_table <- renderDT({
    details <- filterData() %>%
      select(
        name, alpha3Code, callingCodes, capital, region, subregion,
        population, demonym, area, gini, currencies.code,
        currencies.name, languages.name, regionalBlocs.acronym,
        density
      ) %>%
      rename(
        Country = name, Code = alpha3Code, Phone_Code = callingCodes,
        Capital = capital, Region = region, Subregion = subregion,
        Population = population, Demonym = demonym, Area = area, Gini_Coefficient = gini,
        Currency = currencies.code, Currency_Name = currencies.name,
        Language = languages.name, Regional_Block = regionalBlocs.acronym,
        Density = density
      )

    datatable(
      details,
      filter = "top",
      class = "row-border stripe hover", rownames = F, style = "bootstrap",
      extensions = c("ColReorder", "Buttons", "FixedHeader"),
      options = list(
        autoWidth = TRUE,
        scrollX = TRUE, dom = "ltirpB", fixedHeader = TRUE,
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        pageLength = 10,
        lengthMenu = list(c(5, 10, 15, 20, 50, -1), list("5", "10", "15", "20", "50", "All"))
      )
    )
  })
}

#### ==== END ====####
shinyApp(ui, server)
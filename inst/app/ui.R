library(shiny)

shinyUI(function(request){
  fluidPage(title = "",
            sidebarPanel(selectInput(inputId = "kartlag",
                                     label = "Velg et tema:",
                                     choices = c("Personer til fastlege/legevakt",
                                                 "Personer poliklinikk",
                                                 "Akuttinnlagte personer"))
            ),
            mainPanel(
              tabsetPanel(
                         tabPanel("leaflet", leaflet::leafletOutput("map1")),
                         tabPanel("plot", shiny::plotOutput("map2")),
                         tabPanel("ggplot2", shiny::plotOutput("map3")),
                         tabPanel("Kols histogram", plotOutput(outputId = "kolshisto"))
              )
            )
  )
})

library(shiny)
library(magrittr)

#download.file(url = 'http://biogeo.ucdavis.edu/data/diva/adm/NOR_adm.zip',
#              destfile = 'norway.zip')
#download.file(url = 'http://norgeskart.no/json/norge/kommuner.json', destfile = 'inst/app/maps/kommuner.json')
#unzip(zipfile = 'inst/app/maps/Basisdata_0000_Norge_25833_Kommuner_GEOJSON.zip', exdir = "inst/app/maps")
#unzip(zipfile = 'inst/app/maps/Basisdata_0000_Norge_25833_Kommuner_SOSI.zip', exdir = "inst/app/maps")

norway <- maps::map(database = "world", regions = "Norway(?!:Svalbard)", fill = TRUE, col = 1, plot = F)
norway2 <- maps::map(database = "world", regions = "Norway", fill = TRUE, col = 1, plot = F)

topodata <- readLines("maps/kommuner.json") %>% paste(collapse = "\n")

#norway3 <- rgdal::readOGR('maps/Kommune_FLATE.shp')
norway3 <- rgdal::readOGR('maps/eldre.shp')


shinyServer(

  function(input, output) {


    kartlagInput <- reactive({
      switch(input$kartlag,
             "Personer til fastlege/legevakt" = kols$FLLV_pers_Rate,
             "Personer poliklinikk" = kols$Poli_Pers_Rate,
             "Akuttinnlagte personer" = kols$Akutt_pers_Rate)
    })

    output$kolstabell<-renderTable(
      kartlagInput()
    )

    output$kolshisto <- renderPlot({

      kolsdata <- data.frame(bohf=kols$Opptaksomr, kartlag=kartlagInput())

      # barplot
      ggplot(data=kolsdata, aes(x=reorder(bohf, kartlag), y=kartlag)) +
        geom_bar(stat="identity", fill="#95BDE6") +
        labs(x = "Opptaksområde", y = input$kartlag) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(panel.background = element_blank())

    })

    output$map1 <- leaflet::renderLeaflet({

      leaflet::leaflet(norway2) %>% leaflet::setView(lng = 10.0, lat = 65.0, zoom = 4) %>%
        leaflet::addTiles() %>%
        #        leaflet::addTopoJSON(topodata, weight = 1, color = "#444444", fill = FALSE) %>%
        leaflet::addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
    })

    output$map2 <- shiny::renderPlot({
      plot(norway3)
    })

    output$map3 <- shiny::renderPlot({
      return(ggplot2::ggplot(ggplot2::geom_map(map = ggplot2::map_data("world", region = "Norway"))))
    })
  }

)

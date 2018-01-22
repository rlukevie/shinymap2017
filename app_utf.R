########################################################################
#                           PREPARATION                                #
########################################################################

# =============================================================================
# imports
# =============================================================================

library(shiny)
library(leaflet)
library(rgdal)
library(lattice)
library(ggplot2)
library(dplyr)
library(scales)
library(networkD3)
library(dygraphs)



# =============================================================================
# read data
# =============================================================================

gemeinde <-
  readOGR(
    "./shp/simplified",
    "gemeindeergebnisse_klein_mit_wiener_bezirken_mit_attributen_mit_lon_lat_mit_absolut",
    use_iconv = TRUE,
    encoding = "UTF-8"
  )

bundeslaender <- readOGR("./shp/simplified",
                         "l_bundeslaender",
                         use_iconv = TRUE,
                         encoding = "UTF-8")

Wahlen  <- read.csv("./csv/WahlenSeit45.csv", ";", header = TRUE, encoding = "UTF-8")

# =============================================================================
# convert factors to character / numeric
# =============================================================================

convert_to_character = c('gebietsnam',
                         'gewinner')
for (column in convert_to_character) {
  gemeinde@data[[column]] <- as.character(gemeinde@data[[column]])
}

convert_to_numeric = c(
  'iso',
  'wahlberech',
  'a_oevp',
  'a_fpoe',
  'a_gruene',
  'a_spoe',
  'a_neos',
  'a_pilz',
  'a_weisse',
  'a_kpoe',
  'a_gilt',
  'a_floe',
  'a_unguelti',
  'oevp',
  'fpoe',
  'gruene',
  'spoe',
  'neos',
  'pilz',
  'weisse',
  'kpoe',
  'gilt',
  'floe',
  'abgegebene',
  'ungueltige',
  'gueltige',
  'wahlbet'
)
for (column in convert_to_numeric) {
  gemeinde@data[[column]] <-
    round(as.numeric(as.character(gemeinde@data[[column]])), digits = 1)
}

# =============================================================================
# define javascript functions for responsive elements
# =============================================================================

jsGetWindowDimensions <-
    'var dimension = [0, 0];
  $(document).on("shiny:connected", function(e) {
  dimension[0] = window.innerWidth;
  dimension[1] = window.innerHeight;
  Shiny.onInputChange("dimension", dimension);
  });
  $(window).resize(function(e) {
  dimension[0] = window.innerWidth;
  dimension[1] = window.innerHeight;
  Shiny.onInputChange("dimension", dimension);
  });
  '
# =============================================================================
# define palettes
# =============================================================================

pal_spoe <- colorBin(
  "Reds",
  gemeinde$a_spoe,
  c(0, 10, 20, 30, 40, 50, 70),
  pretty = FALSE,
  na.color = NA
)
pal_oevp <-
  colorBin(
    c("#eeeeee", "#cccccc", "#777777", "#444444", "#111111"),
    gemeinde$a_oevp,
    c(0, 10, 30, 50, 70, 90),
    pretty = FALSE,
    na.color = NA
  )
pal_fpoe <- colorBin(
  "Blues",
  gemeinde$a_spoe,
  c(0, 15, 30, 40, 50, 60),
  pretty = FALSE,
  na.color = NA
)
pal_gruene <- colorBin(
  "Greens",
  gemeinde$a_gruene,
  c(0, 2, 3, 5, 8, 14),
  pretty = FALSE,
  na.color = NA
)
pal_neos <-
  colorBin(
    c("#fee3f6", "#dc68a2", "#d25a97", "#c14f89", "#b4417c"),
    gemeinde$a_neos,
    c(0, 4, 8, 12, 16, 20),
    pretty = FALSE,
    na.color = NA
  )
pal_pilz <- colorBin(
  colorRampPalette(c('#f7ecc9', '#aaa38b'))(5),
  gemeinde$a_pilz,
  c(0, 2, 3, 5, 8, 14),
  pretty = FALSE,
  na.color = NA
)
pal_weisse <-
  colorBin(
    colorRampPalette(c('#cccccc', '#333333'))(5),
    gemeinde$a_weisse,
    c(0, 1, 2, 3, 4, 5),
    pretty = FALSE,
    na.color = NA
  )
pal_kpoe <- colorBin(
  colorRampPalette(c('#d5a8a8', '#550000'))(3),
  gemeinde$a_kpoe,
  c(0, 1, 2, 3),
  pretty = FALSE,
  na.color = NA
)
pal_gilt <- colorBin(
  colorRampPalette(c('#d5a8a8', '#990000'))(5),
  gemeinde$a_gilt,
  c(0, 2, 4, 6, 10),
  pretty = FALSE,
  na.color = NA
)

pal_floe <- colorBin(
  colorRampPalette(c('#dddd9d', '#888800'))(5),
  gemeinde$a_floe,
  c(0, 2, 4, 6, 10),
  pretty = FALSE,
  na.color = NA
)
pal_gewinner <- colorFactor(c("Blue", "Black", "Red"),
                            gemeinde$gewinner,
                            na.color = NA)
pal_wahlbet <- colorBin(
  "Purples",
  gemeinde$wahlbet,
  c(30, 40, 50, 60, 70, 80, 90),
  pretty = FALSE,
  na.color = NA
)
pal_ungueltig <- colorBin(
  "Oranges",
  gemeinde$a_unguelti,
  c(0, 1, 2, 3, 4),
  pretty = FALSE,
  na.color = NA
)
pal_wahlberech <- colorBin('Purples',
                           gemeinde$wahlberech,
                           c(0, 1000, 10000, 100000, 200000),
                           na.color = NA)


########################################################################
#                           USER INTERFACE                             #
########################################################################

ui <- shinyUI(navbarPage(theme = "style_general.css",
  "Wahl '17",
  id = "nav",
  collapsible = TRUE,
  
  # =============================================================================
  # map panel
  # =============================================================================
  tabPanel("",
           icon = icon("map"),
           fluidPage(
             tags$script(jsGetWindowDimensions),
             div(
               class = "outer",
               tags$style(
                 type = "text/css",
                 ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"
               ),
               includeCSS("style.css"),
               
               # select input: shape fill elements (parties)
               absolutePanel(
                 id = "select",
                 class = "panel panel-default",
                 fixed = FALSE,
                 draggable = TRUE,
                 top = 20,
                 left = 0,
                 right = 0,
                 bottom = "auto",
                 width = 200,
                 height = "auto",
                 style = "margin: auto;",
                 selectInput(
                   "choreo",
                   label = NA,
                   choices = c(
                     "Gewinner",
                     "Anteil SPÖ",
                     "Anteil ÖVP",
                     "Anteil FPÖ",
                     "Anteil GRÜNE",
                     "Anteil NEOS",
                     "Anteil PILZ",
                     "Anteil WEISSE",
                     "Anteil KPÖ",
                     "Anteil GILT",
                     "Anteil FLÖ",
                     "Wahlbeteiligung",
                     "Ungültig"#,
                     #"Wahlberechtigte"
                   ),
                   selected = "Gewinner"
                 )
               ),
               
               # leaflet
               leafletOutput("mymap", height = "100%", width = "100%"),
               
               # histo & scatter
               conditionalPanel(
                 condition = "input.dimension[0] >= 1100 && input.choreo != 'Gewinner'",
                 absolutePanel(
                   class = "panel panel-default semitransparent-panel tooltip",
                   fixed = FALSE,
                   draggable = FALSE,
                   top = "auto",
                   left = 10,
                   right = "auto",
                   bottom = 20,
                   width = "auto",
                   height = "auto",
                   
                   fluidRow(column(
                     12,
                     plotOutput("histo", height = 250, width = 250),
                     span(class = "tooltiptext", "Histogramm: Zeigt die Streuung der Stimmenanteile der jeweiligen Partei über alle Gemeinden.")
                     
                   )),
                   fluidRow(column(
                     12,
                     span(
                       class = "tooltiptext",
                       "Scatterplot: Zeigt die Verteilung der Stimmenanteile der jeweiligen Partei über alle Gemeinden in Abhängigkeit von der Anzahl der Wahlberechtigten pro Gemeinde."
                     ),
                     plotOutput("scatter", height = 250))))),
               
               # barplot
               conditionalPanel(
                 condition = "input.dimension[0] >= 900",
                 absolutePanel(
                   id = "controls",
                   class = "panel panel-default",
                   fixed = FALSE,
                   draggable = FALSE,
                   top = "auto",
                   left = "auto",
                   right = 10,
                   bottom = 20,
                   width = "auto",
                   height = "auto",

                   fluidRow(column(6,
                                   plotOutput("context", height = 300, width = 450)
                                   ),
                            column(6,
                                  plotOutput("bar", height = 300, width = 450)
                                  ))))))),

  # =============================================================================
  # charts panel
  # =============================================================================
  
  tabPanel("",
           icon = icon("bar-chart-o"),
           tabsetPanel(type = "tabs",
                       tabPanel("Wählerströme", 
                                fluidPage(
                                  fluidRow(
                                    column(12,
                                           h1("Wählerstromanalyse", align = "center"))),
                                  fluidRow(
                                    column(12, sankeyNetworkOutput("waehlerstrom"))))),
                       tabPanel("Gemeindegröße", 
                                fluidPage(
                                  fluidRow(
                                    column(12,
                                           h1("Stimmenverteilung und Gemeindegröße", align = "center"))),
                                  fluidRow(
                                    column(8, offset = 2, plotOutput("gemeinde_size"))),
                                  fluidRow(
                                    column(8, offset = 2, sliderInput("sizeSlider", h4("Anzahl Wahlberechtigte:"),
                                                          min = 0, 
                                                          max = 200000, 
                                                          value = c(50000, 100000), 
                                                          step = 500, 
                                                          dragRange = TRUE, 
                                                          animate = animationOptions(interval = 100)))))),
                       tabPanel("Wahlen seit 1945", 
                                fluidPage(
                                  fluidRow(
                                    column(8, offset = 2,
                                           h1("Wahlen seit 1945", align = "center"))),
                                  fluidRow(
                                    column(8, offset = 2, dygraphOutput("dygraph"))))))),
  
  # =============================================================================
  # about panel
  # =============================================================================
  
  tabPanel("",
           icon = icon("info-sign", lib = "glyphicon"),
           fluidPage(
             titlePanel("Nationalratswahl 2017"),
             sidebarLayout(         
               sidebarPanel(
                 h3("Über"),
                 # p("Dieses Projekt ist eine Zusammenarbeit von Antonio Jurina, Roland Lukesch, Christof Nachtigal und Sladjan Zivojinovic im Rahmen der Lehrveranstaltung",
                 #   em ("Projektseminar aus Geoinformation"), "unter der Leitung", 
                 #   a("Univ.-Prof. Dipl.-Ing. Dr. Wolfgang Kainz",
                 #     href = "https://carto.univie.ac.at/personal/wolfgang-kainz/"),
                 #   "an der Universität Wien im Wintersemester 2017/18 entstanden." ,  align = "justify"
                 # ),
                 p("Dieses Projekt ist in Zusammenarbeit von Antonio Jurina, Roland Lukesch und Christof Nachtigal im Rahmen der Lehrveranstaltung",
                   em ("Proseminar Big Data in der Geoinformationsverarbeitung"), 
                   "unter der Leitung von", em("Markus Reitmair, BSc MSc"), "an der Universität Wien im Wintersemester 2017/18 entstanden." ,  align = "justify"),
                 
                 h3("Copyright"),
                 p("Die Ergebnisse der Nationalratswahl 2017 sind auf",
                   a("Open Government Data", 
                     href = "https://www.data.gv.at/katalog/dataset/3179c5b2-9bb5-4a7f-a573-5491ccb0110b"), "verfügbar.",  align = "justify"
                 ),
                 p("Der R-Source-Code ist auf", 
                   a("GitHub",
                     href = "https://github.com/rlukevie/shiny_prototypes"),  "verfügbar.",  align = "justify"
                 )
               ),
               mainPanel(
                 h1("Web App"),
                 p("Die Web-App Nationalratswahl 2017 stellt die Ergebnisse der 26. Nationalratswahl von Österreich dar. 
                   Die Umsetzung erfolgt mit der Open-Source-Software R, eine Programmiersprache für die statistische und visuelle Analyse von Daten. 
                   Um die interaktive Webapplikation zu erstellen, wurde die Programmbibliothek", em("Shiny"), "verwendet. 
                   Shiny ist ein auf R basierendes Framework, mit dem Analysen in interaktive Web-Applikationen (HTML/CSS/JS) verwandelt werden können.", align = "justify"),
                 h2("Karte"),
                 p("Die interaktive Karte zeigt auf Gemeindeebene (für Wien auf Bezirksebene) die Ergebnisse der Nationalratswahl 2017. Dabei stellt die beim Start der Applikation ausgewählte Karte die Gewinner der Nationalratswahl und eine Übersicht der Ergebnisse dar.",  align = "justify"),
                 p("Mit Hilfe der Dropdown-Liste können die einzelnen Parteien im Detail ausgewählt werden, hierdurch erscheinen ein Streudiagramm und Histogramm von der ausgewählten Partei. Durch Anklicken einer Gemeinde werden die Ergebnisse in einem Balkendiagramm von der jeweiligen Gemeinde sichtbar.",  align = "justify"),
                 p("Ein weiteres Diagramm zeigt in Abhängigkeit vom aktuell sichtbaren Kartenausschnitt die Stimmenverteilung für die gerade sichtbaren Gemeinden. 
                   Damit lässt sich beispielsweise die Stimmenverteilung einer ausgewählten Gemeinde mit ihrer Umgebung vergleichen.", align = "justify"),
                 h2("Diagramme"),
                 p("Die Applikation umfasst außer der Karte interaktive Diagramme in einem zweiten Tab: ", align = "justify"),
                 HTML("<ul><li>Eine Visualisierung der Wählerstromanalyse.</li>
                   <li>Ein Balkendiagramm, das anzeigt, wie die Stimmenverteilung aussehen würde, wenn nur jene Gemeinden mit einer definierbaren Anzahl an Wahlberechtigten berücksichtigt würden.</li>
                   <li>Ein Liniendiagramm mit den historischen Nationalratswahlergebnissen seit 1945.</li></ul>")
               ))))))


########################################################################
#                               SERVER                                 #
########################################################################


server <- function(input, output, session) {
  
  # ============================================================================= 
  # =============================================================================
  # TAB: MAP
  # =============================================================================
  # =============================================================================
  
  # get data for clicks on polygons
  data_of_click <- reactiveValues()
  observeEvent(input$mymap_shape_click, {
    data_of_click$clickedShape <- input$mymap_shape_click
  })
  
  # =============================================================================
  # chart for data in map extent
  # =============================================================================
  
  # get data in bounds 
  data_of_bounds <- reactiveValues()
  observeEvent(input$mymap_bounds, {
    data_of_bounds$bounds <- input$mymap_bounds
  })
  
  # output function
  # TODO: get rid of error when moving map away from austria
  output$context <- renderPlot({
    bounds = data_of_bounds$bounds
    if (is.null(input$mymap_bounds)) {
      return(gemeinde[FALSE,])
    }
    
    # prepare data
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    gemSubset <- subset(gemeinde,
                        lat >= latRng[1] & lat <= latRng[2] &
                          lon >= lngRng[1] & lon <= lngRng[2])

    my_gemeinde <- gemSubset@data %>%
      na.omit() %>%
      select(a_spoe, a_oevp, a_fpoe, a_gruene, a_neos, a_pilz, a_weisse, a_kpoe, a_gilt, a_floe, a_unguelti)

    gemeinde_plot <- gemSubset@data %>%
      select(
        spoe,
        oevp,
        fpoe,
        gruene,
        neos,
        pilz,
        weisse,
        kpoe,
        gilt,
        floe,
        ungueltige,
        gueltige
      )
    gemeinde_plot <- colSums(gemeinde_plot, na.rm = TRUE)
    gemeinde_plot <- gemeinde_plot / gemeinde_plot["gueltige"] * 100
    gemeinde_plot <- gemeinde_plot[1:11]
    
    # plot data
    barplot(gemeinde_plot,
            # beside = FALSE,
            main = "aktueller Kartenausschnitt (pan / zoom)",
            space = 0.1,
            width = 0.9,
            beside = TRUE,
            col = c('red', 'black', 'blue', 'green',
                    'pink', '#e6dcbc', 'gray', '#550000',
                    '#990000', '#cccc00', '#fd8d3c'
            ),
            border = NA,
            horiz = FALSE,
            names.arg = c('SPÖ', 'ÖVP', 'FPÖ', 'GRÜNE',
                          'NEOS', 'PILZ', 'WEISSE', 'KPÖ',
                          'GILT', 'FLÖ', 'ungültig'
            ),
            cex.names = 1.1,
            font.axis = 1,
            xlim = c(0, 11),
            ylim = c(0, 60),
            las = 2)
    partei_farben <-
      c('red', 'black', 'blue', 'green',
        'pink', '#e6dcbc', 'gray', '#550000',
        '#990000', '#cccc00', '#fd8d3c'
      )
    gemeinde_plot <- round(gemeinde_plot, digits = 1)
    for (i in 1:11) {
      if (gemeinde_plot[i] > 10) {
        # print(gemeinde_plot[i])
        text(i - 0.5, 4, gemeinde_plot[i], cex = 1.0, font = 2, col = 'white'
        )
      } else {
        text(i - 0.5, gemeinde_plot[i] + 4, gemeinde_plot[i], cex = 1.0, font = 2, col = partei_farben[i]
        )
      }
    }
  })
  
  # =============================================================================
  # prepare and draw map
  # =============================================================================
  
  # switch colorpal depending on what to draw
  colorpal_reactive <- reactive({
    switch(
      input$choreo,
      "Gewinner" = pal_gewinner,
      "Anteil SPÖ" = pal_spoe,
      "Anteil ÖVP" = pal_oevp,
      "Anteil FPÖ" = pal_fpoe,
      "Anteil GRÜNE" = pal_gruene,
      "Anteil NEOS" = pal_neos,
      "Anteil PILZ" = pal_pilz,
      "Anteil WEISSE" = pal_weisse,
      "Anteil KPÖ" = pal_kpoe,
      "Anteil GILT" = pal_gilt,
      "Anteil FLÖ" = pal_floe,
      "Wahlbeteiligung" = pal_wahlbet,
      "Ungültig" = pal_ungueltig,
      "Wahlberechtigte" = pal_wahlberech
    )
  })
  
  # switch solors depending on what to draw
  color_reactive <- reactive({
    switch(
      input$choreo,
      "Gewinner" = "",
      "Anteil SPÖ" = 'red',
      "Anteil ÖVP" = 'black',
      "Anteil FPÖ" = 'blue',
      "Anteil GRÜNE" = 'green',
      "Anteil NEOS" = '#e55b83',
      "Anteil PILZ" = '#e6dcbc',
      "Anteil WEISSE" = 'gray',
      "Anteil KPÖ" = '#550000',
      "Anteil GILT" = '#990000',
      "Anteil FLÖ" = '#cccc00',
      "Ungültig" = '#fd8d3c',
      "Wahlbeteiligung" = 'purple'
    )
  })
  
  # switch data depending on what to draw
  data_reactive <- reactive({
    switch(
      input$choreo,
      "Gewinner" = gemeinde@data$gewinner,
      "Anteil SPÖ" = gemeinde@data$a_spoe,
      "Anteil ÖVP" = gemeinde@data$a_oevp,
      "Anteil FPÖ" = gemeinde@data$a_fpoe,
      "Anteil GRÜNE" = gemeinde@data$a_gruene,
      "Anteil NEOS" = gemeinde@data$a_neos,
      "Anteil PILZ" = gemeinde@data$a_pilz,
      "Anteil WEISSE" = gemeinde@data$a_weisse,
      "Anteil KPÖ" = gemeinde@data$a_kpoe,
      "Anteil GILT" = gemeinde@data$a_gilt,
      "Anteil FLÖ" = gemeinde@data$a_floe,
      "Wahlbeteiligung" = gemeinde@data$wahlbet,
      "Ungültig" = gemeinde@data$a_unguelti,
      "Wahlberechtigte" = gemeinde@data$wahlberech
    )
  })
  
  # draw the map
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 12)) %>%
      setView(13.6, 47.0, 7, zoom = 7) %>%
      # addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = tileOptions(minZoom = 0, maxZoom = 14, continuousWorld = T)) 
  })
  
  # draw the reactive map content
  observe({
    if (input$choreo == "Gewinner") {
      suffix <- ""
    } else {
      suffix <- "%"
    }
    pal <- colorpal_reactive()
    data <- data_reactive()
    leafletProxy("mymap", data = gemeinde) %>%
      clearGroup(group = "gemeinden") %>%
      clearControls() %>%
      addPolygons(
        group = "gemeinden",
        weight = 1,
        layerId = ~ iso,
        smoothFactor = 0.2,
        color = "white",
        fillOpacity = 0.8,
        fillColor = ~ pal(data)
      ) %>%
      addPolylines(data = bundeslaender,
                   color = "white",
                   weight = 1.7,
                   opacity = 0.8) %>%
      addLegend(
        "topleft",
        pal = pal,
        values = ~ data,
        title = input$choreo,
        opacity = 1,
        labFormat = labelFormat(suffix = suffix),
        na.label = NA
      )
    })
  
  # =============================================================================
  # scatterplot
  # =============================================================================
  
  output$scatter <- renderPlot({
    if (!(input$choreo %in% c("Gewinner"))) {
      data <- data_reactive()
      pal <- colorpal_reactive()
      color <- color_reactive()
      ggplot(gemeinde@data, ylim = c(0, 60), aes(x = wahlberech, y = data)) +
        geom_point(fill = color, color = color) +
        geom_smooth(fill = color,
                    color = color,
                    se = FALSE) +
        scale_x_continuous(trans = log10_trans(),
                           labels = c('1', '1000', '100000')) +
        scale_y_continuous(limits = c(0, 80)) +
        theme(panel.background = element_rect(fill = 'white', colour = 'white'),
              panel.grid.major = element_line(colour = "#cccccc", size = 0.5, linetype = "dashed"),
              plot.margin = unit(c(2,0,2,0), "mm")) + 
        labs(title = "Scatterplot", x = "Wahlberechtigte", y = "Stimmenanteil") +
        theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.2))
        # 
    }
    
  }, bg = "transparent")
  
  # =============================================================================
  # histogram
  # =============================================================================
  
  output$histo <- renderPlot({
    data <- data_reactive()
    color <- color_reactive()
    if (!(input$choreo %in% c("Gewinner"))) {
      hist(
        data,
        nclass = 200,
        main = NULL,
        xlab = input$choreo,
        ylab = "Häufigkeit",
        ylim = c(0, 120),
        xlim = c(0, 80),
        col = color,
        border = color
      )
      title(main = "Histogramm", adj = 0)
    }
    
  }, bg = "transparent")
  
  # =============================================================================
  # barplot for clicked municipality
  # =============================================================================
  
  output$bar <- renderPlot({
    my_iso = data_of_click$clickedShape$id
    if (!is.null(my_iso)) {
      my_gemeinde <- gemeinde@data %>%
        filter(iso == my_iso) %>%
        select(
          gebietsnam,
          a_spoe,
          a_oevp,
          a_fpoe,
          a_gruene,
          a_neos,
          a_pilz,
          a_weisse,
          a_kpoe,
          a_gilt,
          a_floe,
          a_unguelti,
          wahlbet,
          wahlberech
        )
      my_gebietsname <- my_gemeinde$gebietsnam
      my_gemeinde$gebietsnam <- NULL
      titel <-
        paste0(my_gebietsname,
               " (",
               my_gemeinde$wahlbet,
               " % / ",
               my_gemeinde$wahlberech,
               ")")
      my_gemeinde$wahlbet <- NULL
      my_gemeinde$wahlberech <- NULL
    } else {
      my_gemeinde <- rep(0, 11)
      titel <- "Detailinformationen durch Klick auf Gemeinde"
    }
    
    barplot(
      rev(rev(as.matrix(my_gemeinde))),
      main = titel,
      space = 0.1,
      width = 0.9,
      beside = TRUE,
      col = c('red', 'black', 'blue', 'green', 
              'pink', '#e6dcbc', 'gray', '#550000', 
              '#990000', '#cccc00', '#fd8d3c'
      ),
      border = NA,
      horiz = FALSE,
      names.arg = c('SPÖ', 'ÖVP', 'FPÖ', 'GRÜNE',
                    'NEOS', 'PILZ', 'WEISSE', 'KPÖ',
                    'GILT', 'FLÖ', 'ungültig'
      ),
      cex.names = 1.1,
      font.axis = 1,
      xlim = c(0, 11),
      ylim = c(0, 60),
      las = 2
    )
    
    partei_farben <-
      c('red', 'black', 'blue', 'green',
        'pink', '#e6dcbc', 'gray', '#550000',
        '#990000', '#cccc00', '#fd8d3c'
      )
    
    for (i in 1:11) {
      if (my_gemeinde[i] > 10) {
        text(i - 0.5, 4, my_gemeinde[i], cex = 1.0, font = 2, col = 'white'
        )
      } else {
        text(i - 0.5, my_gemeinde[i] + 4, my_gemeinde[i], cex = 1.0, font = 2, col = partei_farben[i]
        )
      }
    }
    
  }, bg = "transparent")
  
  # ============================================================================= 
  # =============================================================================
  # TAB: CHARTS
  # =============================================================================
  # =============================================================================  

  
  # =============================================================================
  # barplot with slider input for number of eligible voters
  # =============================================================================
  
  output$gemeinde_size <- renderPlot({
    data <- gemeinde@data %>%
      filter(wahlberech >= input$sizeSlider[1] & wahlberech <= input$sizeSlider[2])
    num_gemeinden <- nrow(data)
    gemeindenamen <- data$gebietsnam
    if (nrow(data) > 0) {
      data <- select(data, 
        spoe,
        oevp,
        fpoe,
        gruene,
        neos,
        pilz,
        weisse,
        kpoe,
        gilt,
        floe,
        ungueltige,
        abgegebene,
        wahlberech,
        gueltige)
      data <- colSums(data, na.rm = TRUE)
      data <- data / data["gueltige"] * 100
      data <- data[1:11]
      
      } else {
        data <- c(0,0,0,0,0,0,0,0,0,0,0)
      }

    titel = paste0("Stimmenverteilung der ", 
                   format(num_gemeinden, big.mark = "."), 
                   " Gemeinden / Wiener Bezirke mit ", 
                   format(input$sizeSlider[1], big.mark = "."), 
                   " bis ", 
                   format(input$sizeSlider[2], big.mark = "."), 
                   " Wahlberechtigten")
    par(bg = '#dddddd')
    barplot(data,
            main = titel,
            space = 0.1,
            width = 0.9,
            beside = TRUE,
            col = c('red', 'black', 'blue', 'green',
                    'pink', '#e6dcbc', 'gray', '#550000',
                    '#990000', '#cccc00', '#fd8d3c'),
            border = NA,
            horiz = FALSE,
            names.arg = c('SPÖ', 'ÖVP', 'FPÖ', 'GRÜNE',
                          'NEOS', 'PILZ', 'WEISSE', 'KPÖ',
                          'GILT', 'FLÖ', 'ungültig'),
            cex.names = 1.1,
            font.axis = 1,
            xlim = c(0, 11),
            ylim = c(0, 60),
            las = 2)
    partei_farben <-
      c('red', 'black', 'blue', 'green',
        'pink', '#e6dcbc', 'gray', '#550000',
        '#990000', '#cccc00', '#fd8d3c')
    data <- round(data, digits = 1)
    for (i in 1:11) {
      if (data[i] > 10) {
        # print(data[i])
        text(i - 0.5, 4, data[i], cex = 1.0, font = 2, col = 'white'
        )
      } else {
        text(i - 0.5, data[i] + 4, data[i], cex = 1.0, font = 2, col = partei_farben[i]
        )}}})
  
  # =============================================================================
  # voter transition analysis
  # =============================================================================
  
  output$waehlerstrom <- renderSankeyNetwork({

    # 1 ------ CONNECTION DATA FRAME
    
    # Usually what you have is a connection data frame: a list of flows with intensity for each flow
    links=data.frame(source=c("SPÖ 2013","SPÖ 2013", "SPÖ 2013", "SPÖ 2013", "SPÖ 2013", "SPÖ 2013", "SPÖ 2013", "SPÖ 2013",
                              "ÖVP 2013", "ÖVP 2013", "ÖVP 2013", "ÖVP 2013", "ÖVP 2013", "ÖVP 2013", "ÖVP 2013", "ÖVP 2013",
                              "FPÖ 2013", "FPÖ 2013", "FPÖ 2013", "FPÖ 2013", "FPÖ 2013", "FPÖ 2013", "FPÖ 2013", "FPÖ 2013",
                              "Grüne 2013", "Grüne 2013", "Grüne 2013", "Grüne 2013", "Grüne 2013", "Grüne 2013", "Grüne 2013", "Grüne 2013",
                              "NEOS 2013", "NEOS 2013", "NEOS 2013", "NEOS 2013", "NEOS 2013", "NEOS 2013", "NEOS 2013", "NEOS 2013",
                              "Frank 2013", "Frank 2013", "Frank 2013", "Frank 2013", "Frank 2013", "Frank 2013", "Frank 2013", "Frank 2013",
                              "BZÖ 2013", "BZÖ 2013", "BZÖ 2013", "BZÖ 2013", "BZÖ 2013", "BZÖ 2013", "BZÖ 2013", "BZÖ 2013",
                              "Sonstige 2013", "Sonstige 2013", "Sonstige 2013","Sonstige 2013", "Sonstige 2013","Sonstige 2013","Sonstige 2013","Sonstige 2013",
                              "Nichtwähler 2013", "Nichtwähler 2013", "Nichtwähler 2013", "Nichtwähler 2013", "Nichtwähler 2013", "Nichtwähler 2013", "Nichtwähler 2013", "Nichtwähler 2013"), 
                     target=c("SPÖ 2017","ÖVP 2017", "FPÖ 2017", "Grüne 2017", "NEOS 2017", "Pilz 2017", "Sonstige 2017", "Nichtwähler 2017",
                              "SPÖ 2017", "ÖVP 2017", "FPÖ 2017", "Grüne 2017","NEOS 2017", "Pilz 2017", "Sonstige 2017", "Nichtwähler 2017",
                              "SPÖ 2017", "ÖVP 2017", "FPÖ 2017","Grüne 2017", "NEOS 2017", "Pilz 2017", "Sonstige 2017", "Nichtwähler 2017",
                              "SPÖ 2017", "ÖVP 2017", "FPÖ 2017","Grüne 2017", "NEOS 2017", "Pilz 2017", "Sonstige 2017", "Nichtwähler 2017",
                              "SPÖ 2017", "ÖVP 2017", "FPÖ 2017","Grüne 2017", "NEOS 2017", "Pilz 2017", "Sonstige 2017", "Nichtwähler 2017",
                              "SPÖ 2017", "ÖVP 2017", "FPÖ 2017","Grüne 2017", "NEOS 2017", "Pilz 2017", "Sonstige 2017", "Nichtwähler 2017",
                              "SPÖ 2017", "ÖVP 2017", "FPÖ 2017","Grüne 2017", "NEOS 2017", "Pilz 2017", "Sonstige 2017", "Nichtwähler 2017",
                              "SPÖ 2017", "ÖVP 2017", "FPÖ 2017","Grüne 2017", "NEOS 2017", "Pilz 2017", "Sonstige 2017", "Nichtwähler 2017",
                              "SPÖ 2017", "ÖVP 2017", "FPÖ 2017","Grüne 2017", "NEOS 2017", "Pilz 2017", "Sonstige 2017", "Nichtwähler 2017"), 
                     value=c(953000,43000, 155000, 2000, 5000, 32000, 13000, 55000,
                             10000, 948000, 96000, 4000, 32000, 10000, 6000, 19000, 
                             14000,168000, 700000, 1000, 18000, 12000, 7000,41000,
                             161000, 84000, 24000, 147000, 57000, 67000, 11000, 30000,
                             15000, 60000, 12000, 2000, 101000, 31000, 2000, 11000,
                             23000, 114000, 97000, 1000, 8000, 8000, 13000, 5000,
                             6000, 44000, 94000, 0, 8000, 6000, 1000, 7000,
                             15000, 5000, 7000, 6000, 6000, 20000, 28000, 8000,
                             156000, 121000, 122000, 30000, 23000, 31000, 26000, 1198000))
    
    # From these flows we need to create a node data frame: it lists every entities involved in the flow
    nodes=data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())
    
    # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
    links$IDsource=match(links$source, nodes$name)-1 
    links$IDtarget=match(links$target, nodes$name)-1
    
    # prepare color scale: I give one specific color for each node.
    my_color <- 'd3.scaleOrdinal() .domain(["SPÖ", "ÖVP","FPÖ", "Grüne", "NEOS", "Frank", "Pilz", "BZÖ", "Sonstige", "Nichtwähler"]) .range(["red", "black" , "blue", "green", " #B4417C", "yellow", "#AAA38B", "orange", "grey", "8AA8A1"])'
    
    # Make the Network
    sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "IDsource", Target = "IDtarget",
                  Value = "value", NodeID = "name", 
                  colourScale=my_color,
                  nodePadding = 10,
                  fontSize= 14,
                  sinksRight=FALSE)})
  
  # =============================================================================
  # historical line plot
  # =============================================================================
  
    output$dygraph <- renderDygraph({
      dygraph(Wahlen,
              ylab = "in %") %>%
        dySeries("SPÖ", color = "red",strokeWidth=3,drawPoints = TRUE, pointSize = 4) %>%
        dySeries("ÖVP", color = "black", strokeWidth=3,drawPoints = TRUE, pointSize = 4) %>%
        dySeries("FPÖ", color = "blue", strokeWidth=3,drawPoints = TRUE, pointSize = 4) %>%
        dySeries("Grüne", color = "green", strokeWidth=3,drawPoints = TRUE, pointSize = 4) %>%
        dySeries("BZÖ", color = "orange",strokeWidth=3,drawPoints = TRUE, pointSize = 4) %>%
        dySeries("NEOS", color = "purple", strokeWidth=3,drawPoints = TRUE, pointSize = 4) %>%
        dySeries("PILZ", color = "#e6dcbc", strokeWidth=3, drawPoints = TRUE, pointSize = 4)%>%
        dyOptions(drawGrid = FALSE)  %>%
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) 
    })
  }

shinyApp(ui, server)
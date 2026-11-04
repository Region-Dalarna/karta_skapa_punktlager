# app.R
library(shiny)
library(leaflet)
library(leaflet.extras2)
library(dplyr)
library(tibble)
library(sf)
library(shinyjs)
library(shinyWidgets)
library(rdshinyappar)
library(bslib)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R", encoding = "utf-8", echo = FALSE)

# ---- Telemetri (användningsstatistik, se Adminportalen) --------------------
telemetry <- skapa_telemetry("karta_skapa_punktlager")

APP_BESKRIVNING <- "Rita ut och namnge punkter på en karta och exportera dem som en GIS-fil."

# Formaten som visas i filformat-väljaren (korta GDAL/OGR-drivrutinsnamn).
FILFORMAT_VAL <- c(
  "GeoPackage (.gpkg)"    = "GPKG",
  "ESRI Shapefile (.zip)" = "ESRI Shapefile",
  "GeoJSON (.geojson)"    = "GeoJSON"
)


ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  useShinyjs(),
  useSweetAlert(),

  # ---- Header (matchar .rd-header i regiondalarna_ruf.css) --------------
  tags$div(
    class = "rd-header",
    tags$div(class = "rd-header__title", "Skapa GIS-punktlager"),
    tags$a(
      class  = "rd-header__right",
      href   = "https://www.regiondalarna.se",
      target = "_blank",
      tags$img(src = "logo_liggande_fri_vit.png", alt = "Region Dalarna")
    )
  ),

  br(),
  h5("Vänsterklicka för att skapa och namnge punkter i kartan. Högerklicka på en punkt för att ändra dess namn eller ta bort den.",
     style = "color:#666; font-weight: normal; margin-top:-10px; padding: 0 24px;"),
  tags$head(
      tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),         # ikon i fliken i webbläsaren
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'regiondalarna_ruf.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'app.css'),
      telemetri_ui(telemetry),

  tags$script(HTML("
  Shiny.addCustomMessageHandler('focus_and_enter', function(message) {
    var input = document.getElementById(message.id);
    if (input) {
      // vänta lite så att modalen hinner visas
      setTimeout(function() {
        input.focus();
        input.select();  // markera all text
      }, 200);

      // tryck på Enter = klicka på knappen
      input.addEventListener('keypress', function(e) {
        if (e.key === 'Enter') {
          e.preventDefault();
          var btn = document.getElementById(message.btn);
          setTimeout(function() {
            if (btn) btn.click();
          }, 50);
        }
      });
    }
  });
  ")),
  # ersätt din nuvarande enable_on_input med denna
  tags$script(HTML("
  Shiny.addCustomMessageHandler('enable_on_input', function(m) {
    function bind() {
      var input = document.getElementById(m.id);
      var btn   = document.getElementById(m.btn);
      if (!input || !btn) { setTimeout(bind, 100); return; }  // vänta tills modalen finns

      // init: sätt disabled beroende på nuvarande värde
      btn.disabled = (input.value.trim() === '');

      // aktivera/deaktivera vid inmatning
      input.addEventListener('input', function() {
        btn.disabled = (this.value.trim() === '');
      });
    }
    setTimeout(bind, 0);  // kicka igång bindningen
  });
"))
    ),
  fluidRow(
    column(
      width = 6,
      leafletOutput("map", height = 600)
    ),
    column(
      width = 3,
      textInput("gpkg_name", "Filnamn (utan ändelse):", value = "Nytt_punktlager"),
      shinyWidgets::pickerInput(
        inputId = "file_format",
        label   = "Filformat:",
        choices = FILFORMAT_VAL,
        selected = "GPKG"
      ),
      br(),
      selectInput(
        "crs_choice", "Koordinatsystem (CRS):",
        choices = c("SWEREF 99 TM (EPSG:3006)" = 3006, "WGS84 (EPSG:4326)" = 4326),
        selected = 3006
      ),
      br(), br(), br(),
      downloadButton("dl_gpkg", paste("Ladda ner filen"), width = "100%"),
      br(), br(), br(), br(),
      actionButton("clear", "Ta bort alla punkter", class = "btn-danger"),

      br(), br(),
      checkboxInput(
        "show_kommun",
        label = "Visa kommungränser",
        value = FALSE
      ),
      checkboxInput(
        "show_lan",
        label = "Visa länsgränser",
        value = FALSE
      )
    ),
    column(
      width = 3,
      h4("Valda punkter"),
      tableOutput("pts_tbl")
    )
  ),

  # ---- Footer (matchar .rd-footer i regiondalarna_ruf.css) --------------
  tags$div(
    class = "rd-footer",
    "Samhällsanalys, Region Dalarna · ",
    tags$a(
      href = "mailto:samhallsanalys@regiondalarna.se",
      "samhallsanalys@regiondalarna.se"
    )
  )
)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R", encoding = "utf-8", echo = FALSE)

kommuner_sf <- NULL                 # ges inget värde här, utan laddas när sidan har renderats klart
lan_sf      <- NULL

server <- function(input, output, session) {
  rdshinyappar::telemetri_server(telemetry, navigation_id = "_ingen_flik")

  pts <- reactiveVal(tibble(id = integer(), namn = character(), lng = numeric(), lat = numeric()))
  id_counter <- reactiveVal(0)

  # Sen laddning av lan och kommuner
  session$onFlushed(function() {
    con <- shiny_uppkoppling_las("geodata")                      # skapa anslutning
    if (is.null(con)) {
      showNotification(
        "Kunde inte ansluta till geodatabasen -- kommun- och länsgränser går inte att visa just nu.",
        type = "warning", duration = NULL
      )
      return(invisible())
    }
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    kommuner_sf <<- st_read(con, layer = Id(schema = "karta", table = "kommun_scb")) %>%
      st_transform(4326)

    lan_sf <<- st_read(con, layer = Id(schema = "karta", table = "lan_scb")) %>%
      st_transform(4326)
  }, once = TRUE)


  # --- JavaScript: fånga klick och fråga efter namn direkt i webbläsaren ---
  # --- KARTA ---
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions()) |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      fitBounds(12.2, 60.0, 16.5, 62.3) |>
      htmlwidgets::onRender("
    function(el, x) {
      var map = this;

      // --- Vänsterklick: skapa ny punkt ---
      map.on('click', function(e) {
        Swal.fire({
          title: 'Ange namn på ny punkt',
          input: 'text',
          inputPlaceholder: 'Skriv namn på punkten',
          showCancelButton: true,
          confirmButtonText: 'Spara',
          cancelButtonText: 'Avbryt',
          focusConfirm: false,
          inputValidator: (value) => {
            if (!value) return 'Du måste ange ett namn';
          }
        }).then((result) => {
          if (result.isConfirmed) {
            Shiny.setInputValue('new_point', {
              lng: e.latlng.lng,
              lat: e.latlng.lat,
              name: result.value
            }, {priority: 'event'});
          }
        });
      });

      // --- Högerklick: visa textbaserad meny för ändra / ta bort ---
      map.on('layeradd', function(e) {
        var layer = e.layer;
        if (layer && layer.options && layer.options.layerId) {
          layer.on('contextmenu', function(ev) {
            L.DomEvent.preventDefault(ev);

            var popupHtml = `
              <div style='font-size:14px;'>
                <a href='#' id='renameItem' style='color:#007bff; text-decoration:none;'>Ändra namn</a><br>
                <a href='#' id='removeItem' style='color:#dc3545; text-decoration:none;'>Ta bort punkt</a>
              </div>`;

            var popup = L.popup({ closeButton: true })
              .setLatLng(ev.latlng)
              .setContent(popupHtml)
              .openOn(map);

            setTimeout(function() {
              // Ändra namn
              document.getElementById('renameItem').onclick = function(e) {
                e.preventDefault();
                map.closePopup();
                Swal.fire({
                  title: 'Ändra namn på vald punkt',
                  input: 'text',
                  inputValue: layer.options.label || '',
                  showCancelButton: true,
                  confirmButtonText: 'Spara',
                  cancelButtonText: 'Avbryt',
                  inputValidator: (value) => {
                    if (!value) return 'Du måste ange ett namn';
                  }
                }).then((result) => {
                  if (result.isConfirmed) {
                    Shiny.setInputValue('rename_point', {
                      id: layer.options.layerId,
                      name: result.value
                    }, {priority: 'event'});
                  }
                });
              };

              // Ta bort punkt
              document.getElementById('removeItem').onclick = function(e) {
                e.preventDefault();
                map.closePopup();
                Shiny.setInputValue('remove_point', layer.options.layerId, {priority: 'event'});
              };
            }, 50);
          });
        }
      });
    }
  ")
  })

  observe({
    proxy <- leafletProxy("map")

    # Kommungränser – tunnare linje
    if (isTRUE(input$show_kommun) && !is.null(kommuner_sf)) {
      proxy <- proxy %>%
        clearGroup("kommuner") %>%
        addPolygons(
          data        = kommuner_sf,
          color       = "#444444",  # mörkgrå linje
          weight      = 1,          # tunnare än län
          fill        = FALSE,
          fillOpacity = 0,          # ingen fyllning
          group       = "kommuner"
        )
    } else {
      proxy <- proxy %>% clearGroup("kommuner")
    }

    # Länsgränser – lite tjockare linje
    if (isTRUE(input$show_lan) && !is.null(lan_sf)) {
      proxy <- proxy %>%
        clearGroup("lan") %>%
        addPolygons(
          data        = lan_sf,
          color       = "#000000",  # svart linje
          weight      = 2.5,        # tjockare än kommun
          fill        = FALSE,
          fillOpacity = 0,
          group       = "lan"
        )
    } else {
      proxy <- proxy %>% clearGroup("lan")
    }
  })

  # --- Ta emot punkter från JavaScript ---
  # Ny punkt från JavaScript
  observeEvent(input$new_point, {
    np <- input$new_point
    req(np$lng, np$lat)

    id_counter(id_counter() + 1)
    new_id <- id_counter()

    new_pts <- pts() |>
      add_row(id = new_id,
              namn = as.character(np$name),
              lng = np$lng,
              lat = np$lat)
    pts(new_pts)
    redraw_points(new_pts)
  })

  # Ta bort punkt
  observeEvent(input$remove_point, {
    id_rm <- as.integer(input$remove_point)
    pts(pts() |> filter(id != id_rm))
    redraw_points(pts())
  })

  # Ändra namn
  observeEvent(input$rename_point, {
    rp <- input$rename_point
    cur <- pts()
    cur <- cur |>
      mutate(namn = if_else(id == as.integer(rp$id),
                            as.character(rp$name),
                            as.character(namn)))
    pts(cur)
    redraw_points(cur)
  })

  # --- Rensa alla ---
  observeEvent(input$clear, {
    pts(tibble(id = integer(), namn = character(), lng = numeric(), lat = numeric()))
    leafletProxy("map") |> clearGroup("clicked_pts")
  })

  # --- Rita om kartan med contextmeny ---
  redraw_points <- function(df) {
    leafletProxy("map") |>
      clearGroup("clicked_pts") |>
      addCircleMarkers(
        data = df,
        lng = ~lng, lat = ~lat,
        radius = 6, color = "#2c7fb8", fillColor = "#41b6c4",
        fillOpacity = 0.8, stroke = TRUE, weight = 2,
        layerId = ~id,
        group = "clicked_pts",
        label = ~namn,
        options = markerOptions(
          contextmenu = TRUE,
          contextmenuWidth = 200,
          contextmenuItems = context_markermenuItems(
            context_menuItem(
              text = "Ändra namn",
              callback = "function(e){}"   # ingen egen Shiny.setInputValue här
            ),
            context_menuItem(
              text = "Ta bort punkt",
              callback = "function(e){}"   # ingen egen Shiny.setInputValue här
            )
          )
        )
      )
  }

  # --- Tabell ---
  output$pts_tbl <- renderTable({
    pts() |> select(id, namn, lng, lat)
  })

  # --- Export till valfritt vektor-format ---
  output$dl_gpkg <- downloadHandler(
    filename = function() {
      nm <- input$gpkg_name
      fmt <- input$file_format
      if (is.null(fmt) || fmt == "") fmt <- "GPKG"
      if (is.null(nm) || nm == "") nm <- "punktlager"

      if (fmt == "ESRI Shapefile") {
        paste0(nm, ".zip")
      } else {
        ext <- switch(tolower(fmt),
                      "gpkg" = "gpkg",
                      "geojson" = "geojson",
                      tolower(fmt))
        paste0(nm, ".", ext)
      }
    },

    content = function(file) {
      fmt <- input$file_format
      cur <- pts()
      validate(need(nrow(cur) > 0, "Inga punkter att spara."))

      sf_pts <- st_as_sf(cur, coords = c("lng", "lat"), crs = 4326, remove = FALSE)
      sf_pts <- st_transform(sf_pts, as.numeric(input$crs_choice))

      tmpdir <- tempfile("export_")
      dir.create(tmpdir)
      layer_name <- input$gpkg_name

      # skriv till temporär plats
      dsn_path <- file.path(tmpdir, layer_name)

      tryCatch({
        st_write(sf_pts,
                 dsn = dsn_path,
                 layer = layer_name,
                 driver = fmt,
                 delete_dsn = TRUE,
                 quiet = TRUE)
      }, error = function(e) {
        showNotification(paste("Fel vid export:", e$message), type = "error")
        unlink(tmpdir, recursive = TRUE, force = TRUE)
        return(NULL)
      })

      written_files <- list.files(tmpdir, full.names = TRUE, recursive = TRUE)

      if (fmt == "ESRI Shapefile") {
        # flera filer → zip
        old <- setwd(tmpdir)
        zip(zipfile = file, files = list.files(".", recursive = FALSE, full.names = FALSE))
        setwd(old)

      } else if (length(written_files) == 1) {
        file.copy(written_files, file, overwrite = TRUE)
      } else {
        showNotification("Inga filer skapades vid export.", type = "warning")
      }

      unlink(tmpdir, recursive = TRUE, force = TRUE)
    }
  )



  # Aktivera / inaktivera "Ladda ner"-knappen beroende på om det finns punkter
  observe({
    if (nrow(pts()) > 0) {
      shinyjs::enable("dl_gpkg")
    } else {
      shinyjs::disable("dl_gpkg")
    }
  })


}

shinyApp(ui, server)

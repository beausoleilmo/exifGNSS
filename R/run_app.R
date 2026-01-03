# Make sure to install the required packages before running the app:
# install.packages(c("shiny", "shinyFiles", "sf", "mapview", "dplyr", "exif", "DT"))

library(shiny)
library(shinyFiles)
library(sf)
library(mapview)
library(leaflet)
library(dplyr)
library(exif)
library(DT)

# UI
ui <- fluidPage(
  titlePanel("Geotag Photos with GPX"),
  sidebarLayout(
    sidebarPanel(
      h4("Photo Folder"),
      shinyDirButton("dir", "Select a folder", "Please select a folder"),
      verbatimTextOutput("dir_path"),
      br(),
      fileInput("gpx_file", "Choose GPX File",
                multiple = FALSE,
                accept = c(".gpx")),
      actionButton("run_map", "Visualize GPX"),
      br(),
      br(),
      actionButton("modify_exif", "Modify EXIF")
    ),
    mainPanel(
      h4("Exiftool Output"),
      verbatimTextOutput("exif_output"),
      h4("Photo EXIF Data"),
      dataTableOutput("photo_table"),
      h4("Map Output"),
      leaflet::leafletOutput("map"),
      textOutput("success_message")
    )
  )
)

# Server
server <- function(input, output, session) {

  volumes <- getVolumes()()

  shinyDirChoose(
    input,
    'dir',
    roots = volumes,
    session = session
  )

  dir_path_reactive <- reactive({
    if (is.integer(input$dir)) {
      "No folder selected"
    } else {
      parseDirPath(volumes, input$dir)
    }
  })

  output$dir_path <- renderPrint({
    dir_path_reactive()
  })

  gpx_file_path <- reactiveVal(NULL)

  observeEvent(input$run_map, {
    req(input$gpx_file)

    gpx_path <- input$gpx_file$datapath
    gpx_file_path(gpx_path) # Store the path

    tryCatch({
      piste <- sf::st_read(dsn = gpx_path, layer = 'track_points')

      output$map <- leaflet::renderLeaflet({
        mapview(piste)@map
      })

      output$success_message <- renderText({
        "Success! The GPX track has been visualized."
      })

    }, error = function(e) {
      output$success_message <- renderText({
        paste("An error occurred:", e$message)
      })
    })
  })

  # Modify the photos exif (exiftool) --------
  observeEvent(input$modify_exif, {
    path2photos <- dir_path_reactive()
    path2GPX <- input$gpx_file$datapath

    if (path2photos == "No folder selected" || is.null(path2GPX)) {
      output$exif_output <- renderText({
        "Please select a photo folder and a GPX file first."
      })
      return()
    }

    exiftool_path <- Sys.which("exiftool")
    if (nchar(exiftool_path) == 0) {
      output$exif_output <- renderText({
        "Exiftool not found. Please make sure it is installed and in your PATH."
      })
      return()
    }

    output$exif_output <- renderText({
      "Running exiftool... this might take a while."
    })

    # Removes 1 hour from photo (if daylight saving hasn't been properly set)
    # exiftool_chg_date <- sprintf("exiftool \"-AllDates-=1:0:0\" '%s'", path2photos)

    exiftool_cmd <- sprintf("exiftool -geotag '%s' '%s'", path2GPX, path2photos)

    print(exiftool_cmd)

    tryCatch({
      exif_output <- system(exiftool_cmd, intern = TRUE, ignore.stderr = FALSE)

      output$exif_output <- renderText({
        paste(exif_output, collapse = "\n")
      })
    }, error = function(e) {
      output$exif_output <- renderText({
        paste("An error occurred while running exiftool:", e$message)
      })
    })
  })


  photo_data <- reactive({
    path2photos <- dir_path_reactive()
    if (path2photos == "No folder selected") {
      return(NULL)
    }

    # Read all photos
    vec.jpg = list.files(path = path2photos,
                         pattern = '.JPG|.jpeg|.jpg',
                         full.names = TRUE)

    if (length(vec.jpg) == 0) {
      return(NULL)
    }

    res = lapply(X = vec.jpg,
                 FUN =  exif::read_exif)

    names(res) = basename(vec.jpg)

    time_format = '%Y:%m:%d %H:%M:%S'

    # Extract the exif in a dataframe
    photos = res |>
      dplyr::bind_rows(.id = 'file') |>
      dplyr::mutate(
        time = as.POSIXct(timestamp,
                          tz = Sys.timezone(location = TRUE),
                          format = time_format)) |>
      dplyr::select(file, model,
                    # copyright,
                    time = timestamp,
                    longitude, latitude,
                    exp_t = exposure_time,
                    f = f_stop,
                    iso = iso_speed,
                    f_lght = focal_length_35mm
      )

    return(photos)
  })

  output$photo_table <- DT::renderDT({
    photo_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

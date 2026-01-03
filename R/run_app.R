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
  # Main title of the Shiny app
  titlePanel("Add geographical coordinates to photos' metadata"),

  # Side bar
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      # Testing different tags
      # tags$cite('test1'),
      # tags$caption('test2'),
      # tags$code('test3'),
      # tags$pre('test4'),
      # tags$article('test5'),
      # tags$abbr('test6'),
      # tags$address('test7'),
      # tags$view('test8'),
      # tags$nav('test9'),
      # tags$u('test10'),
      # tags$strong('test11'),


      h3("Procedure"),
      tags$ol(
        tags$li("Select a folder with photos"),
        tags$li("Choose the GPX file (remove spaces in the file name before uploading)."),
        tags$li(HTML("Install exiftool <i>before</i> running the 'Modify EXIF' button."))
      ),

      # Add horizontal line
      HTML("<hr style=\"border: 1px solid black; width: 100%;\">"),



      # Big title
      h3("File inputs"),

      # Folder with photos
      h4("Photo Folder"),
      shinyDirButton(id = "dir",
                     label = "Select a folder",
                     title = "Please select a folder"),
      verbatimTextOutput(outputId = "dir_path"),

      br(),

      # GPX file
      fileInput(inputId = "gpx_file",
                label = "Choose GPX File",
                multiple = FALSE,
                accept = c(".gpx")),

      # Add horizontal line
      HTML("<hr style=\"border: 1px solid black; width: 100%;\">"),

      # Big title
      h3("Take action"),

      # Map the GPX file
      h4("Map the GPX data"),
      actionButton(inputId = "run_map",
                   label = "Visualize GPX"),
      p("Creates a map with the provided GPX data."),

      br(),

      # Run command
      h4("Run exiftool"),
      actionButton(inputId = "modify_exif",
                   label = "Modify EXIF"),
      p("Use exiftool to modify the photos' metadata.")
    ),
    mainPanel(
      # Make 2 tabs
      tabsetPanel(
        id = "main_tabs", # ID to refer
        # Table
        tabPanel(title = "Photo EXIF metadata table",
                 value = "photo_table_viz_tab",
                 dataTableOutput("photo_table")),
        # Map
        tabPanel(title = "Map Output",
                 value = "map_gpx_tab",
                 leaflet::leafletOutput("map"),
                 # Success message
                 textOutput("success_message")
        ),
        # Exif output
        tabPanel(title = "exiftool log",
                 value = 'exiftool_output_tab',
                 h4("exiftool command: "),
                 # Format the code as 'code box'
                 tags$pre(textOutput("exif_cmd")),
                 h4("exiftool ouput: "),
                 # Prints the output of exiftool
                 verbatimTextOutput("exif_output")
        )
      ) # End tabsetPanel
    ) # End mainPanel
  ) # End sidebarLayout
) # end fluidpage

# Server
server <- function(input, output, session) {


  # Triggered when button is clicked
  ## For GPX loading
  observeEvent(input$dir, {
    updateTabsetPanel(session, "main_tabs", selected = "photo_table_viz_tab")
  })
  ## For map
  observeEvent(input$run_map, {
    updateTabsetPanel(session, "main_tabs", selected = "map_gpx_tab")
  })
  ## For modification EXIF
  observeEvent(input$modify_exif, {
    updateTabsetPanel(session, "main_tabs", selected = "exiftool_output_tab")
  })


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
        ''
        # "Success! The GPX track has been visualized."
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

    output$exif_cmd <- renderText({
      exiftool_cmd
    })

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
                    longitude,
                    latitude#,
                    # exp_t = exposure_time,
                    # f = f_stop,
                    # iso = iso_speed,
                    # f_lght = focal_length_35mm
      )

    return(photos)
  })

  output$photo_table <- DT::renderDT({
    photo_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# app.R

library(shiny)
library(shinyFiles)
library(EGM)
library(ggplot2)
library(dplyr)
library(scales)
library(shinyjqui)
library(shinyjs)
library(DT)

# UI elements for the Shiny app
ui <- fluidPage(
  useShinyjs(), # Initialize shinyjs
  titlePanel("EGM Signal Visualization and Annotation"),
  tags$head(
    tags$style(HTML("
      .active-mode {
        background-color: #4CAF50; /* Green */
        color: white;
      }
    "))
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("fileType", "Select File Type",
        choices = c("LS Pro (.txt)" = "lspro", "ECG (.xml)" = "ecg", "WFDB (.dat and .hea)" = "wfdb")
      ),
      conditionalPanel(
        condition = "input.fileType == 'lspro'",
        fileInput("fileLspro", "Upload LS Pro File", accept = ".txt")
      ),
      conditionalPanel(
        condition = "input.fileType == 'ecg'",
        fileInput("fileEcg", "Upload ECG File", accept = ".xml")
      ),
      conditionalPanel(
        condition = "input.fileType == 'wfdb'",
        fileInput("fileDat", "Upload WFDB .dat File", accept = ".dat"),
        fileInput("fileHea", "Upload WFDB .hea File", accept = ".hea"),
        fileInput("fileAnnotators", "Upload Additional WFDB Annotation Files (Optional)", multiple = TRUE, accept = c(".ecgpuwave", ".atr"))
      ),
      actionButton("plotData", "Plot Data"),
      selectInput("annotation", "Annotation", choices = c("H = His signal", "A = atrial signal", "V = ventricular signal")),
      selectInput("annotatorType", "Annotator Type", choices = c("Intracardiac" = "intracardiac")),
      actionButton("zoomMode", "Zoom/Pan Mode", class = "mode-button"),
      actionButton("annotationMode", "Annotation Mode", class = "mode-button"),
      actionButton("deleteAnnotation", "Delete Selected Annotations"),
      conditionalPanel(
        condition = "input.fileType != 'wfdb' && input.downloadFormat == 'WFDB'",
        textInput("recordName", "Record Name", "")
      ),
      selectInput("downloadFormat", "Download Format", choices = c("CSV", "WFDB")), # Format selection
      actionButton("writeAnnotations", "Write Annotations"), # Replaced download button with write button
      uiOutput("channelCheckboxes") # Dynamic checkboxes for channels
    ),
    mainPanel(
      jqui_resizable(
        plotOutput("plot", brush = "plotBrush", click = "plotClick", dblclick = "plotDblClick", height = "600px")
      ),
      DTOutput("annotationsTable") # Use DT for interactive table
    )
  )
)


server <- function(input, output, session) {
  # Placeholder for reactive data
  egmData <- reactiveVal(NULL)
  annotationData <- reactiveVal(data.frame(Sample = integer(), Type = character(), Channel = character(), stringsAsFactors = FALSE))
  ranges <- reactiveValues(x = NULL)
  mode <- reactiveVal("zoom") # Default mode is zoom/pan
  brushInput <- reactiveVal(NULL) # Store the brush input
  selectedChannels <- reactiveVal(NULL) # Store selected channels
  wfdbDir <- reactiveVal(NULL)
  wfdbRecord <- reactiveVal(NULL)

  # Observe file input and read data
  observeEvent(input$fileLspro, {
    req(input$fileLspro)
    data <- read_lspro(input$fileLspro$datapath)
    egmData(data)
    wfdbDir(NULL)
    wfdbRecord(NULL)
    updateChannelCheckboxes(data)
  })

  observeEvent(input$fileEcg, {
    req(input$fileEcg)
    data <- read_muse(input$fileEcg$datapath)
    egmData(data)
    wfdbDir(NULL)
    wfdbRecord(NULL)
    updateChannelCheckboxes(data)
  })

  observeEvent(input$fileDat, {
    req(input$fileDat, input$fileHea)
    annotatorFiles <- input$fileAnnotators
    filePaths <- c(input$fileDat$datapath, input$fileHea$datapath)
    if (!is.null(annotatorFiles)) {
      filePaths <- c(filePaths, annotatorFiles$datapath)
    }
    data <- read_wfdb(filePaths)
    egmData(data)
    wfdbDir(dirname(input$fileDat$datapath))
    wfdbRecord(tools::file_path_sans_ext(basename(input$fileDat$name)))
    updateChannelCheckboxes(data)
  })

  # Function to update channel checkboxes
  updateChannelCheckboxes <- function(data) {
    channels <- names(data$signal)[-1] # Exclude 'sample' column
    selectedChannels(channels) # Default to all channels selected
    output$channelCheckboxes <- renderUI({
      checkboxGroupInput("selectedChannels", "Select Channels to Display",
        choices = channels, selected = channels
      )
    })
  }

  # Observe selected channels
  observeEvent(input$selectedChannels, {
    selectedChannels(input$selectedChannels)
  })

  # Switch to Zoom/Pan Mode
  observeEvent(input$zoomMode, {
    mode("zoom")
    updateButtonStyles()
  })

  # Switch to Annotation Mode
  observeEvent(input$annotationMode, {
    mode("annotation")
    updateButtonStyles()
  })

  # Function to update button styles
  updateButtonStyles <- function() {
    if (mode() == "zoom") {
      shinyjs::removeClass(selector = "#annotationMode", class = "active-mode")
      shinyjs::addClass(selector = "#zoomMode", class = "active-mode")
    } else {
      shinyjs::removeClass(selector = "#zoomMode", class = "active-mode")
      shinyjs::addClass(selector = "#annotationMode", class = "active-mode")
    }
  }

  # Plot data
  output$plot <- renderPlot({
    req(egmData())
    data <- egmData()
    channels <- selectedChannels()
    plotObj <- ggm(data, channels = channels)
    plotObj <- plotObj +
      geom_vline(data = annotationData(), aes(xintercept = Sample), linetype = "dashed", size = 1, color = alpha("lightgoldenrod1", 0.6))

    if (!is.null(ranges$x)) {
      plotObj <- plotObj + coord_cartesian(xlim = ranges$x, expand = FALSE)
    }

    plotObj
  })

  # Capture plot brush for zoom
  observeEvent(input$plotBrush, {
    if (mode() == "zoom") {
      brush <- input$plotBrush
      brushInput(brush)
    }
  })

  # Apply zoom and reset brush
  observeEvent(brushInput(), {
    brush <- brushInput()
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      brushInput(NULL) # Reset the brush input
    } else {
      ranges$x <- NULL
    }
  })

  # Capture plot clicks to set annotation
  observeEvent(input$plotClick, {
    if (mode() == "annotation") {
      click <- input$plotClick
      req(click)
      channelNames <- names(egmData()$signal)[-1] # Exclude 'sample' column
      panelvar <- click$panelvar1 # Get the panel variable to identify the channel
      channelName <- panelvar # Use the panel variable directly as the channel name
      newAnnotation <- data.frame(Sample = click$x, Type = input$annotation, Channel = channelName, stringsAsFactors = FALSE)
      annotationData(rbind(annotationData(), newAnnotation))
    }
  })

  # Delete selected annotations
  observeEvent(input$deleteAnnotation, {
    selected <- input$annotationsTable_rows_selected
    if (!is.null(selected)) {
      annotations <- annotationData()
      annotations <- annotations[-selected, ]
      annotationData(annotations)
    }
  })

  # Double-click to reset zoom
  observeEvent(input$plotDblClick, {
    if (mode() == "zoom") {
      ranges$x <- NULL
    }
  })

  # Render annotations table
  output$annotationsTable <- renderDT({
    datatable(annotationData(), selection = "multiple", options = list(pageLength = 5))
  })

  # Write annotations
  observeEvent(input$writeAnnotations, {
    annotations <- annotationData()
    annot_data <- annotation_table(
      sample = annotations$Sample,
      type = annotations$Type,
      channel = annotations$Channel,
      frequency = attributes(egmData()$header)$record_line$frequency
    )
    if (input$downloadFormat == "CSV") {
      write.csv(annotationData(), file.path(getwd(), "annotations.csv"), row.names = FALSE)
    } else {
      if (is.null(wfdbRecord())) {
        record <- gsub("[^A-Za-z0-9]", "", input$recordName)
        write_wfdb(egmData(), record = record, record_dir = getwd())
        write_annotation(data = annot_data, record = record, record_dir = getwd(), annotator = input$annotatorType)
      } else {
        write_annotation(data = annot_data, record = wfdbRecord(), record_dir = wfdbDir(), annotator = input$annotatorType)
      }
    }
  })
}

shinyApp(ui, server)

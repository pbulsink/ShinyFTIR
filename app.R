#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(PlotFTIR)
library(FTIRtools)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("PlotFTIR Tools"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          shiny::fileInput(inputId = "inputfile", label = "Select a .csv file", 
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
          tags$hr(),
          shiny::textInput(inputId = "plottitle", label = "Plot Title", placeholder = "FTIR Spectra", value = "FTIR Spectra"),
          shiny::textInput(inputId = "subtitle", label = "Subtitle (Optional)", placeholder = "Plotted with PlotFTIR"),
          tags$hr(),
          shiny::textInput(inputId = "samplename", label = "Sample Name", placeholder = "FTIR Sample", value = "Sample"),
          shiny::textInput(inputId = "legendtitle", label = "Legend Title", placeholder = "Sample ID", value = "Sample ID"),
          shiny::radioButtons(inputId = "legendloc", label = "Legend Location", choices = c("Right Side", "Bottom"), inline = TRUE),
          tags$hr(),
          shiny::radioButtons(inputId = "lang", label = "Language/Langue", choices = c("English", "Français"), inline = TRUE),
          tags$hr(),
          shiny::radioButtons(inputId = "units", label = "Intensity Units", choices = c("Absorbance", "Transmittance"), inline = TRUE),
          tags$hr(),
          shiny::textInput(inputId = "filename", label = "Download Filename (.png will be appended)", placeholder = "plot", value = "plot.png"),
          shiny::downloadButton("downloadPlot", "Download Plot")
        ),

        # Show a plot of the generated ftir
        mainPanel(
           plotOutput("ftirPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$ftirPlot <- renderPlot(plotInput())

    plotInput <- reactive({
        if(!is.null(input$inputfile)){
          file1 <- input$inputfile
          ftir <- read_ftir(file1$datapath, sample_name = input$samplename)
        } else {
          ftir <- PlotFTIR::biodiesel
        }
        # generate bins based on input from ui.R
       title <- c(input$plottitle, input$subtitle)
       if(!(tolower(input$units) %in% colnames(ftir))) {
         if(input$units == "Absorbance"){
           ftir <- transmittance_to_absorbance(ftir)
         } else {
           ftir <- absorbance_to_transmittance(ftir)
         }
       }
       p <- plot_ftir(ftir, plot_title = title, legend_title = input$legendtitle, lang = ifelse(input$lang == "English", "en", "fr"))
       if(input$legendloc == "Bottom") {
         p <- move_plot_legend(p, position = "bottom")
       }
       p
    })
    
    output$downloadPlot <- downloadHandler(
      contentType = "image/png",
      filename = function(){
        if(substr(input$filename,nchar(input$filename)-3,nchar(input$filename)) == ".png"){
          input$filename
        } else {
          paste(input$filename, ".png", sep = "")
        }
      },
      content = function(file) {
        save_plot(plotInput(), file)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

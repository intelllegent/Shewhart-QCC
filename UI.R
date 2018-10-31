library(shiny)
library(shinyjs)
library(shinythemes)
library(plotly)

# Define UI for data upload app ----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinytheme("readable"),
  
  # App title ----
  titlePanel("Shewhart Quality Control Charts"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose File",
                multiple = FALSE),

      tags$hr(),
      
      uiOutput("vars"),
      
      shinyjs::hidden(
        
            div(id = "plottools",
                
                actionButton("toggle_advanced", "Show/hide advanced menu",
                             icon = icon("cog", lib = "glyphicon")),
                actionButton("help", "Help info",
                             icon = icon("question-sign", lib = "glyphicon"))
                
                ),
            
            div(id = "tools",
                
                hr(),
                p('Make Shewhart tests'),
                actionButton("add_shewhart", "Make QCC"),

                hr(),
                p('Click to add 3-sigm bands'),
                actionButton("sigm", "Three-sigma rule"),
                
                hr(),
                p('Click to change plot layout'),
                actionButton("labels", "Show/hide layout menu")
                
                ),
            
            div(id = "layout",
                
                hr(),
                textInput("plot_title", "Title", "Title"),
                textInput("x_label", "X axis label", "Point"),
                textInput("y_label", "Y axis label", "Value"),
                
                hr(),
                p('Click to change X values'),
                actionButton("show_x_val", "Load column names"),
                
                hr(),
                actionButton("get_label", "Apply changes")
                
                ),
            
            div (id = "x_val_menu",
                 radioButtons("x_val", "Choose data for X axis",
                              choices = c("1", "2")))
            )
      ),
     
      
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotlyOutput("plotly")),
   #     shinyjs::hidden(
        tabPanel("Shewhart Tests", 
                 h4(textOutput("Titl")),
                 h5(textOutput("tr_1")),
                 tableOutput("trash_1"), hr(),
                 h5(textOutput("tr_2")),
                 tableOutput("trash_2"), hr(),
                 h5(textOutput("tr_3")),
                 tableOutput("trash_3"), hr(),
                 h5(textOutput("tr_4")),
                 tableOutput("trash_4"), hr(),
                 h5(textOutput("tr_5")),
                 tableOutput("trash_5"), hr(),
                 h5(textOutput("tr_6")),
                 tableOutput("trash_6"), hr(),
                 h5(textOutput("tr_7")),
                 tableOutput("trash_7"), hr(),
                 h5(textOutput("tr_8")),
                 tableOutput("trash_8"), hr()
                 ),
        tabPanel("Summary Table", dataTableOutput("tbl"))
      )
    )
  )
)
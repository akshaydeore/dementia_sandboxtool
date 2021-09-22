library(shiny)

#interactive is used to update select input values
if (interactive()) {
  
  #Rshiny Interface
  ui <- fluidPage(
    h1("Dementia Care Pathway Model Simulation App
    "),
    
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(12,
                 #textInput("addinput","Add/Delete Node"),
                 selectInput("addinput", "Select Node to Add:",
                             c("GP","EW","DCT","SP","Blood","MRI","PET"))
          )),
        
        fluidRow(
          column(6,
                 actionButton("addNode", "Add Node" , style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          column(6,
                 actionButton("deleteNode", "Delete Node" , style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        ),
        
        fluidRow(
          column(6,
                 selectInput("nodeStart", "Select Start Node:",
                             c(states)
                 )
          ),
          column(6,
                 selectInput("nodeEnd", "Select End Node:",
                             c(states)
                 )
          )
        ),
        fluidRow(
          #textInput("startNode","Start Node"),
          #textInput("endNode","End Node"),
          column(12,
                 sliderInput(inputId = "probability", label = "Probability", value = 0.5 ,min = 0.0, max = 1.0),
                 #numericInput(inputId = "probability", "Probability", value = 0.5)
          ),
          column(7,
                 # h5("To Edit Node Links"),
                 #tags$head(
                 #  tags$style(HTML('#run{background-color:blue}'))
                 #),
                 actionButton("editLink","Edit Node Links", icon("submit"), 
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        )
      ),
      mainPanel(
        fluidRow(
          column(6,
                 actionButton("calculate","Steady States", icon("submit"), 
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
          ,column(2,
                  textOutput("text"),
                  verbatimTextOutput("verb")
                  
          )
        ),  
        plotOutput("plot")
        
      ),
    ))
  
  server <- function(input, output,session){
    # reactiveValues can be updated by many different inputs
    # in this case different buttons control the Markov chain object which is dynamically plotted
    # it is initialised here
    v <- reactiveValues(data = disc_trans)
    states <- reactiveValues(data = disc_trans@states)
    total <- 0
    
    observeEvent(input$addNode, {
      v$data <- addNode(mc=v$data,nodename=input$addinput)
      updateSelectInput(session, "nodeStart",
                        choices = v$data@states,
      )
      updateSelectInput(session, "nodeEnd",
                        choices = v$data@states,
      )
    })
    
    observeEvent(input$deleteNode, {
      v$data <- deleteNode(mc=v$data,nodename=input$addinput)
      updateSelectInput(session, "nodeStart",choices = v$data@states,
      )
      updateSelectInput(session, "nodeEnd",choices = v$data@states,
      )
    }
    )  
    
    observeEvent(input$editLink, {
      v$data <- editLink(startNode=input$nodeStart, endNode=input$nodeEnd,mc=v$data,P=input$probability)
    })
    
    observeEvent(input$calculate, {
      total <- calculate(mc=v$data)
      output$text <- renderText({ total })
      
    })
    
    output$table <- renderTable({ total })
    
    
    output$plot <- renderPlot({
      plot(v$data,main = "Dementia Care Node Graph")
    },
    width = "auto",
    height = "auto",
    res = 72)
  }
  
  shinyApp(ui, server)
}



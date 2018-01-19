# Shiny App UI
library(shiny)
library(networkD3)

ui <- fluidPage(
   
  style = "background-color: #18325b; color: #ffffff;",

  # App title 'Name_Assignment'
  titlePanel("Indrajit_Haridas_Social Network Analysis"),
  
  sidebarLayout(position = 'right',
    
  # Sidebar layout to upload file and select input
  sidebarPanel(
    
    style = "background-color: #2d599e;",
  
    # Upload file
    fileInput('file1', 'Upload Link Files',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    
    fileInput('file2', 'Upload Nodes File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),

  
    numericInput('n', 'How many connections do you want to see?', value = 3)
  ),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("Data_Snippet", tableOutput("table")),
      tabPanel("Network of n Connections", simpleNetworkOutput("plot")),
      tabPanel("Number of emails sent by each person", tableOutput("esent")),
      tabPanel("Number of emails received by each person", tableOutput("erecv"))
      #tabPanel("Summary", verbatimTextOutput("summary")))
      
)
)
)
)

server <- function(input, output) {
  
  output$table <- renderTable({ 
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    head(
    read.table(inFile$datapath)
    )
    
    })
  
  output$plot <- renderSimpleNetwork({
    linksdf <- read.table(input$file1$datapath, col.names = c('Source', 'Target'))
    nodesdf <- read.table(input$file2$datapath, col.names = c('NodeID', 'DepartmentID'))
    
    simpleNetwork(linksdf[1:input$n,], zoom = TRUE, nodeColour = '#ffffff')
    # forceNetwork(Links = linksdf[1:input$n], Nodes = nodesdf, Source = "Source", 
    #              Target = "Target", NodeID = "NodeID", Group = "DepartmentID", 
    #              opacity = 0.8)
    # 

  })
  
  output$esent <- renderTable({ 
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    linksdf <- read.table(inFile$datapath, col.names = c('Source', 'Target'))
    table(linksdf['Source'])
  })
  
  output$erecv <- renderTable({ 
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    linksdf <- read.table(inFile$datapath, col.names = c('Source', 'Target'))
    table(linksdf['Target'])
  })
  
}

shinyApp(ui, server)
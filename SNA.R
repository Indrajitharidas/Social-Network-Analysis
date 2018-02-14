# Shiny App UI
library(shiny)
#library(network)
library(igraph)
library(dplyr)
library(sna)
library(DT)
library(shinythemes)
library(visNetwork)
library(data.table)
library(ggplot2)
#####################
## Helper Functions##
#####################


##########################
###Get 2-hop data frame###
##########################

# 2 hop data frame
hopdf <- function(linksdf, user = as.numeric(esent[esent['Mails'] == max(esent$Mails)][1]), n = 6){
  
  # Get the lest of 1-hope targets and make a dataframe with all the 1-hop users and the original user
  
  list_user <- filter(linksdf, (from == user) & (to != user))
  list_user <- data.frame(rbind(user, unique(list_user['to'])))
  colnames(list_user) <-  'list_user'
  list_user <- list_user[1:n,] # By default show only 5 2-hop connections
  
  
  # Remove '1-hop only' connections
  df <- linksdf[linksdf$from %in% list_user,]
  #df <- filter(df, from != user) # mails from all users except the selected user
  
  #test: get links for every user in list_user upto 10 links
  temp2 <- filter(df, from == list_user[2])[1:10,]
  temp3 <- filter(df, from == list_user[3])[1:10,]
  temp4 <- filter(df, from == list_user[4])[1:10,]
  temp5 <- filter(df, from == list_user[5])[1:10,]
  temp6 <- filter(df, from == list_user[6])[1:10,]
  # temp7 <- filter(df, from == list_user[7])[1:10,]
  # temp8 <- filter(df, from == list_user[8])[1:10,]
  
  temp <- linksdf[linksdf$from == user,] # mails from selected user
  temp <- temp[temp$to %in% list_user,]
  
  df <- rbind(temp, temp2, temp3, temp4, temp5, temp6)#, temp7, temp8)
  
  df
}

#####################
###Plot the 2-hops###
#####################
# 2-Hop Neighbor Plot Function
hopn <- function(linksdf, nodesdf, user = as.numeric(esent[esent['Mails'] == max(esent$Mails)][1]), n = 6){
  
  # Get the lest of 1-hope targets and make a dataframe with all the 1-hop users and the original user
  
  list_user <- filter(linksdf, (from == user) & (to != user))
  list_user <- data.frame(rbind(user, unique(list_user['to'])))
  colnames(list_user) <-  'list_user'
  list_user <- list_user[1:n,] # By default show only 4 2-hop connections
  
  
  # Remove '1-hop only' connections
  df <- linksdf[linksdf$from %in% list_user,]
  #df <- filter(df, from != user) # mails from all users except the selected user
  
  #test: get links for every user in list_user upto 10 links
  temp2 <- filter(df, from == list_user[2])[1:10,]
  temp3 <- filter(df, from == list_user[3])[1:10,]
  temp4 <- filter(df, from == list_user[4])[1:10,]
  temp5 <- filter(df, from == list_user[5])[1:10,]
  temp6 <- filter(df, from == list_user[6])[1:10,]
  # temp7 <- filter(df, from == list_user[7])[1:10,]
  # temp8 <- filter(df, from == list_user[8])[1:10,]
  
  temp <- linksdf[linksdf$from == user,] # mails from selected user
  temp <- temp[temp$to %in% list_user,]
  
  #df <- rbind(temp, df)
  df <- rbind(temp, temp2, temp3, temp4, temp5, temp6)#, temp7, temp8)
  
  nodesdf$group <- ifelse(nodesdf$id == user, 1, 2)
  nodesdf <- subset(nodesdf, ((id %in% df$from) | (id %in% df$to)))
  nodesdf['title'] <- nodesdf['id']
  
  visNetwork(nodes = nodesdf, edges = df) %>% visEdges(arrows = 'to',
                                                       color = list(color = "lightblue", highlight = "red"))%>% visInteraction(tooltipDelay = 0)
  
  # forceNetwork(Links = df, Nodes = nodesdf, Source = "Source",
  #              Target = "Target",  NodeID = "NodeID",
  #              Group = "Group", opacity = 1, zoom = TRUE, bounded = T, 
  #              colourScale = JS("'d3.scaleOrdinal(d3.schemeCategory10);"))

}

#######################
###Degree Centrality###
#######################

## Degree Centrality
centrality <- function(linksdf){
  df <- linksdf # load data
  df <- graph_from_data_frame(df)
  dat <- data.frame(igraph::degree(df, mode = 'total')) # find out in-degre centrality
  cendf <- data.frame(id = row.names(dat), value = dat$igraph..degree.df..mode....total..)
  cendf
  
}

#######################
###In-Degree Centrality###
#######################
## in-degree Centrality
indeg <- function(linksdf ){
  df <- linksdf # load data
  df <- graph_from_data_frame(df)
  xat <- data.frame(igraph::degree(df, mode = "in")) # find out in-degre centrality
  idcendf <- data.frame(id = row.names(xat), value = xat$igraph..degree.df..mode....in..)
  idcendf
}

###########################
###Betweeness Centrality###
###########################

## Betweenes Centrality
betweenness_cen <- function(linksdf ){
  df <- linksdf # load data
  df <- graph_from_data_frame(df)
  dat <- data.frame(igraph::betweenness(df, normalized = TRUE)) # find out betweeness centrality
  cendf <- data.frame(id = row.names(dat), value = dat$igraph..betweenness.df.)
  cendf
}

###########################
###Dept. mails###
###########################
dep_mail <- function(linksdf, nodesdf){

  linksdf <- merge(x = linksdf, y = nodesdf, by.x = "from", by.y = "id", all.x = TRUE)
  colnames(linksdf) <- c('from','to' , 'Source_Dep')
  linksdf <- merge(x = linksdf, y = nodesdf, by.x = "to", by.y = "id", all.x = TRUE)
  colnames(linksdf) <- c('from','to' , 'Source_Dep', 'Target_Dep')
  linksdf <- linksdf[,c('Source_Dep', 'Target_Dep')]
  colnames(linksdf) <- c('from','to')
  linksdf <- rename(count(linksdf, from, to), Freq = n)
  linksdf
  }


######
##UI##
######
ui <- fluidPage(
  
  #shinythemes::themeSelector(),
  theme = shinytheme('slate'),
  
  # App title 'Name_Assignment'
  titlePanel("IndrajitHaridas_Social_Network_Analysis"),
  
  sidebarLayout(position = 'right',
    
  # Sidebar layout to upload file and select input
  sidebarPanel(
    
    #style = "background-color: #2d599e;",
  
    # Upload file
    fileInput('file1', 'Upload Link Files',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), placeholder = "email-Eu-core.txt"),
    
    fileInput('file2', 'Upload Nodes File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), placeholder = "email-Eu-core-department-labels.txt"),
    width = 3

  
    ),
  
  mainPanel(
    width = 9,
    
    tabsetPanel(
      tabPanel("Instructions", uiOutput("instructions")),
     tabPanel("Data_Snippet", fluidRow( 
                                        column(4, dataTableOutput("table1")),
                                        column(4, dataTableOutput("table2")),
                                        column(12, plotOutput("gg1")),
                                        column(12, plotOutput("gg2"))
                                        # column(12, ggvisOutput("gg3"))
                                        )),
      tabPanel("Network of n Connections", visNetworkOutput("plot"), 
               sidebarPanel(numericInput('n', 'How many connections do you want to see?', value = 25))),
      
     tabPanel('Number of emails...', tabsetPanel(
                              tabPanel("sent by each person", column(5, dataTableOutput("esent"))),
                              tabPanel("received by each person", column(5, dataTableOutput("erecv"))))),
      tabPanel("2 Hop Neighbors", tabsetPanel(
                                         tabPanel('For Top Senders', visNetworkOutput("plot1")),
                                         tabPanel('For Top Receivers', visNetworkOutput("plot2"))),
               sidebarPanel(sliderInput("r", "Select rank for which you want the graph!",min= 1, max= 10, value= 1))),
    

      tabPanel('Centralities', tabsetPanel(
                                tabPanel("Degree Centrality",visNetworkOutput("dc1")),
                                tabPanel("Betweeness Centrality",visNetworkOutput("bc1")),
                                tabPanel("In-Degree Centrality",visNetworkOutput("ic1"))),
                                sidebarPanel(sliderInput("r1", "Select rank for which you want the graph!",min= 1, max= 10, value= 1))),
     
     tabPanel("Department Wise Mails",fluidRow(
                                                column(3,dataTableOutput("dpc1")),
                                                column(9,visNetworkOutput("dpc2")), 
                                                sidebarPanel(numericInput('dep', 'Select the Department you want to see?', value = 0)))),
     tabPanel("Observations", uiOutput("summary"))
)
)
)
)

server <- function(input, output) {
  
  output$instructions <- renderUI(HTML("<p><ul><li> <b><b>Data_Snippet:</b></b> Gives data overview and distribution. </li>
                                  <p><li> <b><b> Network of n connections:</b></b> It simply shows first 'n' connections from the links dataset.</li></p>
                                  <p><li> <b><b>2 Hop neighbors:</b></b> This tab displays 5 '1-hops' and 10 '2-hops' corresponding to each 1-hop.</li></p>
                                  <p><li> <b><b>Centralities:</b></b> This will display 5 '1-hops' with highest centrality and respective 2-hops with top centralities. Color indicates towards the department whereas node size is the magnitude of the centrality.</li></p>
                                  <p><li> <b><b>Rank:</b></b> Rank of node when arranged from top to bottom using the key parameter, e.g. Centrality or number of emails.</li></p>
                                  <p><li> <b><b>Department wise mails:</b></b> Displays tabulated magnitude of number of intere\action between the departments. Graph shows 2-hop connections originating from the top 5 1-hops, width of the link is the magnitude. </li></p>
                                  </ul>
                                  "))
  
  
  
  # Welcome page output

  output$table1 <- renderDataTable({

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    datatable(head(
    read.table(inFile$datapath, col.names = c('from', 'to')),class = 'cell-border stripe')
    ) %>% formatStyle('from', color = 'black')%>% formatStyle('to', color = 'black')

    })
  output$table2 <- renderDataTable({
    
    inFile <- input$file2
    
    if (is.null(inFile))
      return(NULL)
    
    datatable(head(
      read.table(inFile$datapath, col.names = c('id', 'Department')), class = 'cell-border stripe')
    ) %>% formatStyle('id', color = 'black') %>% formatStyle('Department', color = 'black')
    
  })
  
  output$gg1 <- renderPlot({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    linksdf <- read.table(inFile$datapath, col.names = c('from', 'to'))
    
    ggplot(data=linksdf) + geom_histogram(aes(linksdf$from), color = 'white') + 
      theme(plot.background = element_rect(fill = '#272B30'), panel.background = element_rect(fill = '#272B30'), 
            plot.title = element_text(colour = "#FFFFFF"), axis.title = element_text(colour = "#FFFFFF"), 
            plot.subtitle = element_text(colour = "#FFFFFF"), axis.text = element_text(colour = "#FFFFFF")) +
      ggtitle("Links", subtitle = 'Bars represents sending frequency whereas line represents receiving frequency') +
      labs(x = 'Node', y = 'Frequency') +
      geom_freqpoly(data=linksdf, aes(linksdf$to), color="firebrick", size = 2)
    
    
  })
  
  output$gg2 <- renderPlot({
    
    inFile <- input$file2
    
    if (is.null(inFile))
      return(NULL)
    
    nodesdf <- read.table(inFile$datapath, col.names = c('id', 'group'))
    
    ggplot(data=nodesdf) + geom_histogram(aes(nodesdf$group), color = 'white') + 
      theme(plot.background = element_rect(fill = '#272B30'), panel.background = element_rect(fill = '#272B30'), 
            plot.title = element_text(colour = "#FFFFFF"), axis.title = element_text(colour = "#FFFFFF"), 
            plot.subtitle = element_text(colour = "#FFFFFF"), axis.text = element_text(colour = "#FFFFFF")) +
      ggtitle("Departments", subtitle = 'Bars represents department population') +
      labs(x = 'Department', y = 'Population')
    
  })

#####################################################################################################
  
  # n-connections output
  output$plot <- renderVisNetwork({
    
    if ((is.null(input$file1)) | (is.null(input$file2)) )
      return(NULL)
    
    linksdf <- read.table(input$file1$datapath, col.names = c('from', 'to'))
    nodesdf <- read.table(input$file2$datapath, col.names = c('id', 'group'))
    #nodesdf$group <- NULL
    linksdf <- linksdf[1:input$n,]
    nodesdf <- filter(nodesdf, id %in% linksdf$from | id %in% linksdf$to)
    
    #nodesdf['title'] <- a + nodesdf['id']
    nodesdf$title <- paste0("Node: ", nodesdf$id, " & Dept. ", nodesdf$group)
    
    visNetwork(edges = linksdf, nodes = nodesdf) %>% visEdges(arrows = 'to',
                                                              color = list(color = "lightblue", highlight = "red"))%>% visInteraction(tooltipDelay = 0)

  })
  #####################################################################################################
  
  # Email sent count output
  output$esent <- renderDataTable({ 
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    linksdf <- read.table(inFile$datapath, col.names = c('from', 'to'))
    linksdf <- data.frame(table(linksdf['from']))
    colnames(linksdf) <-  c('from', 'Number of Emails Sent')
    linksdf <- linksdf[order(-linksdf$`Number of Emails Sent`),]
    datatable(linksdf, class = 'cell-border stripe')  %>% formatStyle('from', color = 'black') %>% formatStyle('Number of Emails Sent',
                      background = styleColorBar(range(linksdf['Number of Emails Sent']), 'lightblue'),
                      backgroundSize = '98% 88%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'center',
                      color = 'black')
  })
#####################################################################################################
    
  # Email received count output
  output$erecv <- renderDataTable({ 
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    linksdf <- read.table(inFile$datapath, col.names = c('from', 'to'))
    linksdf <- data.frame(table(linksdf['to']))
    colnames(linksdf) <-  c('to', 'Number of Emails Received')
    linksdf <- linksdf[order(-linksdf$`Number of Emails Received`),]
    datatable(linksdf, class = 'cell-border stripe') %>% formatStyle('to', color = 'black') %>% formatStyle('Number of Emails Received',
                                                                     background = styleColorBar(range(linksdf['Number of Emails Received']), 'lightblue'),
                                                                     backgroundSize = '98% 88%',
                                                                     backgroundRepeat = 'no-repeat',
                                                                     backgroundPosition = 'center',
                                                                     color = 'black') 
  })
#####################################################################################################  
  
  # 2-hop output: Top Senders
  # Plots are in order from max to min: plot 1
  output$plot1 <- renderVisNetwork({
    
    if ((is.null(input$file1)) | (is.null(input$file2)) )
      return(NULL)
    
    # reading the files
    linksdf <- read.table(input$file1$datapath, col.names = c('from', 'to'))
    nodesdf <- read.table(input$file2$datapath, col.names = c('id', 'group'))
    
    # Sorting the max mails sent
    x <- count(linksdf, from)
    colnames(x) <-  c('from', 'to')
    x <- x[order(-x$to),]
    rownames(x) <- NULL
    
    user = as.numeric(x[input$r,1])
    
  hopn(linksdf = linksdf, nodesdf = nodesdf, user = user)
  })
  
  # 2-hop output: Top receivers
  output$plot2 <- renderVisNetwork({
    
    if ((is.null(input$file1)) | (is.null(input$file2)) )
      return(NULL)
    
    # reading the files
    linksdf <- read.table(input$file1$datapath, col.names = c('from', 'to'))
    nodesdf <- read.table(input$file2$datapath, col.names = c('id', 'group'))
    
    # Sorting the max mails sent
    x <- count(linksdf, to)
    colnames(x) <-  c('from', 'to')
    x <- x[order(-x$to),]
    rownames(x) <- NULL
    
    user = as.numeric(x[input$r,1])
    
    hopn(linksdf = linksdf, nodesdf = nodesdf, user = user)
  })
  
  
#####################################################################################################
  
  # Degree Centrality
  output$dc1 <- renderVisNetwork({
    
    if ((is.null(input$file1)) | (is.null(input$file2)) )
      return(NULL)
    
    linksdf <- read.table(input$file1$datapath, col.names = c('from', 'to'), stringsAsFactors = FALSE)
    nodesdf <- read.table(input$file2$datapath, col.names = c('id', 'group'),stringsAsFactors = FALSE)
    
    cendf <- centrality(linksdf)
    
    user <- cendf[order(-cendf$value),] # get top 10
    rownames(user) <- NULL
    user <- as.numeric(as.character(user$id))
    linksdf<- merge(x = linksdf, y = cendf, by.x = "to", by.y = "id", all.x = TRUE)
    linksdf <- arrange(linksdf, desc(value))
    linksdf <- linksdf[,order(colnames(linksdf),decreasing=FALSE)]
    linksdf$value <- NULL
    
    df <- hopdf(linksdf = linksdf, user = user[input$r1])
    #df <- hopdf(linksdf = linksdf, user[1])
    rownames(df) <- NULL
    
    #Degree centrality
    nodesdf <- merge(x = nodesdf, y = cendf, by.x = "id", by.y = "id", all.x = TRUE)
    nodesdf <- subset(nodesdf, ((id %in% df$from) | (id %in% df$to)))
    colnames(nodesdf) <-  c('id', 'group', 'value')
    
    v <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500) # for node size
    nodesdf$value <- findInterval(nodesdf$value, v)
    nodesdf['value'] <- nodesdf['value']*10
    #nodesdf['title'] <- a + nodesdf['id']
    nodesdf$title <- paste0("Node: ", nodesdf$id, " & Dept. ", nodesdf$group)
    
    nodesdf$shape <- ifelse(nodesdf$id == user[input$r1], 'square', 'dot')
    
    # 2 hop network
    visNetwork(nodes = nodesdf, edges = df) %>% 
      visEdges(arrows = 'to',
               color = list(color = "lightblue", highlight = "red")) %>% visInteraction(tooltipDelay = 0)

    
  })

  #####################################################################################################
  
  # Betweeness Centrality
  output$bc1 <- renderVisNetwork({
    
    if ((is.null(input$file1)) | (is.null(input$file2)) )
      return(NULL)
    
    linksdf <- read.table(input$file1$datapath, col.names = c('from', 'to'), stringsAsFactors = FALSE)
    nodesdf <- read.table(input$file2$datapath, col.names = c('id', 'group'),stringsAsFactors = FALSE)

    bet_cendf <- betweenness_cen(linksdf)
    
    user <- bet_cendf[order(-bet_cendf$value),] # get top 10
    rownames(user) <- NULL
    user <- as.numeric(as.character(user$id))
    
    linksdf<- merge(x = linksdf, y = bet_cendf, by.x = "to", by.y = "id", all.x = TRUE) # sorting by centrality
    linksdf <- arrange(linksdf, desc(value))
    linksdf <- linksdf[,order(colnames(linksdf),decreasing=FALSE)]
    linksdf$value <- NULL
    
    df <- hopdf(linksdf = linksdf, user[input$r1])
    rownames(df) <- NULL
    
    #Betweenes centrality
    nodesdf <- merge(x = nodesdf, y = bet_cendf, by.x = "id", by.y = "id", all.x = TRUE)
    nodesdf <- subset(nodesdf, ((id %in% df$from) | (id %in% df$to)))
    
    v <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500) # for node size
    nodesdf$value <- findInterval(nodesdf$value, v)
    nodesdf['value'] <- nodesdf['value']*10
    #nodesdf['title'] <- a + nodesdf['id']
    nodesdf$title <- paste0("Node: ", nodesdf$id, " & Dept. ", nodesdf$group)
    
    nodesdf$shape <- ifelse(nodesdf$id == user[input$r1], 'square', 'dot')
    
    
    # 2 hop network
    visNetwork(nodes = nodesdf, edges = df) %>% 
      visEdges(arrows = 'to',
               color = list(color = "lightblue", highlight = "red"))%>% visInteraction(tooltipDelay = 0)
  })
  
  #####################################################################################################
  
  # in-degree Centrality
  output$ic1 <- renderVisNetwork({
    
    if ((is.null(input$file1)) | (is.null(input$file2)) )
      return(NULL)
    
    linksdf <- read.table(input$file1$datapath, col.names = c('from', 'to'), stringsAsFactors = FALSE)
    nodesdf <- read.table(input$file2$datapath, col.names = c('id', 'group'),stringsAsFactors = FALSE)
    
    in_cendf <- indeg(linksdf)
    
    user <- in_cendf[order(-in_cendf$value),] # get top 10
    rownames(user) <- NULL
    user <- as.numeric(as.character(user$id))
    
    linksdf<- merge(x = linksdf, y = in_cendf, by.x = "to", by.y = "id", all.x = TRUE) # sorting by centrality
    linksdf <- arrange(linksdf, desc(value))
    linksdf <- linksdf[,order(colnames(linksdf),decreasing=FALSE)]
    linksdf$value <- NULL
    
    df <- hopdf(linksdf = linksdf, user[input$r1])
    rownames(df) <- NULL
    
    #in-degree centrality
    nodesdf <- merge(x = nodesdf, y = in_cendf, by.x = "id", by.y = "id", all.x = TRUE)
    nodesdf <- subset(nodesdf, ((id %in% df$from) | (id %in% df$to)))

    v <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500) # for node size
    nodesdf$value <- findInterval(nodesdf$value, v)
    nodesdf['value'] <- nodesdf['value']*10
    #nodesdf['title'] <- a + nodesdf['id']
    nodesdf$title <- paste0("Node: ", nodesdf$id, " & Dept. ", nodesdf$group)
    
    nodesdf$shape <- ifelse(nodesdf$id == user[input$r1], 'square', 'dot')
    
    # 2 hop network
    visNetwork(nodes = nodesdf, edges = df) %>% 
      visEdges(arrows = 'to',
               color = list(color = "lightblue", highlight = "red"))%>% visInteraction(tooltipDelay = 0)
  })
  
  #####################################################################################################
  
  # Dept. wise mails!

   output$dpc1 <- renderDataTable({
     
     if ((is.null(input$file1)) | (is.null(input$file2)) )
       return(NULL)
     
     linksdf <- read.table(input$file1$datapath, col.names = c('from', 'to'), stringsAsFactors = FALSE)
     nodesdf <- read.table(input$file2$datapath, col.names = c('id', 'group'),stringsAsFactors = FALSE)
  
     x <- data.frame(dep_mail(linksdf,nodesdf))
     datatable(x) %>% formatStyle( 'from', color = 'black')  %>% 
       formatStyle( 'to', color = 'black')  %>% formatStyle('Freq',
                                                            background = styleColorBar(range(x['Freq']), 'lightblue'),
                                                            backgroundSize = '98% 88%',
                                                            backgroundRepeat = 'no-repeat',
                                                            backgroundPosition = 'center',
                                                            color = 'black')

  })

  output$dpc2 <- renderVisNetwork({
    
    if ((is.null(input$file1)) | (is.null(input$file2)) )
      return(NULL)
    
    linksdf <- fread(input$file1$datapath, col.names = c('from', 'to'), stringsAsFactors = FALSE)
    nodesdf <- read.table(input$file2$datapath, col.names = c('id', 'group'),stringsAsFactors = FALSE)

    edges_df = data.frame(dep_mail(linksdf,nodesdf))
    colnames(edges_df) <-  c('from', 'to', 'value')

    hopn(linksdf = edges_df, nodesdf = data.frame(id = unique(nodesdf$id)), user = input$dep)
    
    # visNetwork(nodes = unique(nodesdf_nodes), edges = edges_df) %>%
    #   visEdges(arrows = 'to',
    #            color = list(color = "lightblue", highlight = "red")) %>% visLegend()
  })
  
  output$summary <- renderUI(HTML("<p><ul><li> Node 160 has highest degree, betweeness and in-degree centrality. Clearly, it is the most active and important spoc of the organization. <br> This can be an example of PA of CEO in company or Affairs Minister in a government. </li></p>
                                  <p><li> 6 out of top 10 degree centrality belong to department 36, 6 out of top 10 betweenes centrality belong to department 36 and Top 5 in-degrees belong to department 36. Most busy and important department.</li></p>
                                  <p><li> Node 160 sends mail to others in department 36 who then connect(send mail) to nodes in other department.</li></p>
                                  <p><li> 377 is 9th ranked in case of betweeness centrality but not in top 10 of degree or in-degree centrality. This means that 377 holds an important position as it bridges gap between multiple departments.</li></p>
                                  <p><b><b>----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------</b></b></p>
                                  <p><li> <b>Most Connected Node:</b> 160</li></p>
                                  <p><li> <b>Most Important Node:</b> 160</li></p>
                                  <p><li> <b>Most Important Node with Least Connections:</b> 377</li></p>
                                  <p><li> <b>Most Communicating Department:</b> 36</li></p>
                                  <p><li> <b>Departments with Maximum Internal Mails:</b> 14 (1562 Mails!)</li></p>
                                  <p><li> <b>Departments with Maximum Mails Between Them:</b> 36 and 4 (229 Mails!)</li></p>
                                  
                                  </ul>"))
}

shinyApp(ui, server)
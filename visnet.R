library(igraph)
library(network)
library(networkD3)
library(dplyr)
library(sna)
library(visNetwork)

linksdf <- read.table('email-Eu-core.txt', col.names = c('from', 'to'), colClasses=c("character", "character"), stringsAsFactors = FALSE)
nodesdf <- read.table('email-Eu-core-department-labels.txt', col.names = c('id', 'group'), colClasses=c("character", "character"), stringsAsFactors = FALSE)
nodesdf$group <- NULL

head(linksdf)
head(nodesdf)


hopdf <- function(linksdf, user = as.numeric(esent[esent['Mails'] == max(esent$Mails)][1]), n = 5){
  
  # Get the lest of 1-hope targets and make a dataframe with all the 1-hop users and the original user
  
  list_user <- filter(linksdf, from == user)
  list_user <- data.frame(rbind(user, unique(list_user['to'])))
  colnames(list_user) <-  'list_user'
  list_user <- list_user[1:n,] # By default show only 5 2-hop connections
  
  
  # Remove '1-hop only' connections
  df <- linksdf[linksdf$from %in% list_user,]
  df <- filter(df, from != user) # mails from all users except the selected user
  temp <- linksdf[linksdf$from == user,] # mails from selected user
  temp <- temp[temp$to %in% list_user,]
  
  df <- rbind(temp, df)
  
  df
}


###############################################2-hop plot#########################################################
# Sorting the max mails sent
x <- count(linksdf, from)
colnames(x) <-  c('from', 'to')
x <- x[order(-x$to),]
rownames(x) <- NULL

user = as.numeric(x[1,1])


# Get the lest of 1-hope targets and make a dataframe with all the 1-hop users and the original user

list_user <- filter(linksdf, from == user)
list_user <- data.frame(rbind(user, unique(list_user['to'])))
colnames(list_user) <-  'list_user'
list_user <- list_user[1:5,] # By default show only 5 2-hop connections


# Remove '1-hop only' connections
df <- linksdf[linksdf$from %in% list_user,]
df <- filter(df, from != user) # mails from all users except the selected user
temp <- linksdf[linksdf$from == user,] # mails from selected user
temp <- temp[temp$to %in% list_user,]

df <- rbind(temp, df)

nodesdf$group <- ifelse(nodesdf$id == user, 1, 2)
nodesdf <- subset(nodesdf, ((id %in% df$from) | (id %in% df$to)))

visNetwork(nodes = nodesdf, edges = df) %>% visEdges(arrows = 'from')


###############################################Degree Centrality####################################

moded = 'total'

df <- linksdf # load data
df <- graph_from_data_frame(df)
dat <- data.frame(igraph::degree(df, mode = moded)) # find out degre centrality
cendf <- data.frame(id = row.names(dat), value = dat$igraph..degree.df..mode...moded.)

user <- cendf[order(-cendf$value),] # get top 10
rownames(user) <- NULL
user <- as.numeric(as.character(user$id))

df <- hopdf(linksdf = linksdf, user = user[1])
rownames(df) <- NULL


#Degree centrality
nodesdf <- merge(x = nodesdf, y = cendf, by.x = "id", by.y = "id", all.x = TRUE)
nodesdf <- subset(nodesdf, ((id %in% df$from) | (id %in% df$to)))
nodesdf['value'] <- nodesdf['value']*nodesdf['value']*nodesdf['value']
nodesdf$shape <- ifelse(nodesdf$id == user[1], 'star', 'circle')

# 2 hop network
visNetwork(nodes = nodesdf, edges = df) %>% 
  visNodes(shape = nodesdf$shape) %>% 
  visEdges(arrows = 'to')

###############################################Betweeness Centrality####################################

df <- linksdf # load data
df <- graph_from_data_frame(df)
dat <- data.frame(igraph::betweenness(df)) # find out betweeness centrality
cendf <- data.frame(id = row.names(dat), value = dat$igraph..betweenness.df.)

user <- cendf[order(-cendf$value),] # get top 10
rownames(user) <- NULL
user <- as.numeric(as.character(user$id))

df <- hopdf(linksdf = linksdf, user[1])
rownames(df) <- NULL

#Betweenes centrality
nodesdf <- merge(x = nodesdf, y = cendf, by.x = "id", by.y = "id", all.x = TRUE)
nodesdf <- subset(nodesdf, ((id %in% df$from) | (id %in% df$to)))
nodesdf['value'] <- nodesdf['value']

# 2 hop network
visNetwork(nodes = nodesdf, edges = df) %>% visEdges(arrows = 'to')

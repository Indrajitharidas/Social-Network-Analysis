library(network)
library(networkD3)
library(dplyr)

linksdf <- read.table('email-Eu-core.txt', col.names = c('Source', 'Target'), colClasses=c("character", "character"))
nodesdf <- read.table('email-Eu-core-department-labels.txt', col.names = c('NodeID', 'DepartmentID'), colClasses=c("character", "character"))

# Most emails sent
esent <- data.frame(table(linksdf['Source']))
colnames(esent) <-  c('Source', 'Mails')
head(esent)

# user with maximum mails sent
user <- as.numeric(esent[esent['Mails'] == max(esent$Mails)][1])

# Get the lest of 1-hope targets and make a dataframe with all the 1-hop users and the original user
list_user <- filter(linksdf, Source == user)
list_user <- data.frame(rbind(user, list_user['Target']))
colnames(list_user) <-  'list_user'
head(list_user)
simpleNetwork(subset(linksdf, Source%in% list_user$list_user), zoom = TRUE)

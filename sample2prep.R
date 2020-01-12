library(shiny)
library(visNetwork)
library(ndtv)
library(network)
library(igraph)
library(dplyr)
library(intergraph)

people<- read.csv("people.csv")
jobs<- read.csv("jobs.csv")
orgs<- read.csv("organizations.csv")
degrees<- read.csv("degrees.csv")
sub<- read.csv("sub_organizations.csv")

#Data Preparation
names(degrees)[names(degrees) == "person_uuid"] <- "from"
names(degrees)[names(degrees) == "org_school_uuid"] <- "to"

names(jobs)[names(jobs) == "person_uuid"] <- "from"
names(jobs)[names(jobs) == "org_uuid"] <- "to"

names(sub)[names(sub) == "sub_org_uuid"] <- "from"
names(sub)[names(sub) == "org_uuid"] <- "to"

degrees$started_on<- as.Date(degrees$started_on,format = "%Y-%m-%d")
#degrees$started_on<- as.numeric(format(joblinks$started_on, "%Y"))
degrees$completed_on<- as.Date(degrees$completed_on,format = "%Y-%m-%d")
#degrees$ended_on<- as.numeric(format(joblinks$ended_on, "%Y"))

jobs$started_on<- as.Date(jobs$started_on,format = "%Y-%m-%d")
jobs$ended_on<- as.Date(jobs$ended_on,format = "%Y-%m-%d")

orgs$founded_on<- as.Date(orgs$founded_on,format = "%Y-%m-%d")
orgs$closed_on<- as.Date(orgs$closed_on,format = "%Y-%m-%d")

deg <- graph.data.frame(degrees[,2:6], directed = F)

j <- graph.data.frame(jobs[,2:6], directed = F) 

sub_org <- graph.data.frame(sub, directed = F) 

full<- deg %u% j %u% sub_org

t<- V(full)$name %in% people[,1] 
vertex_attr(full)$type[t]<- 1 #Type 1 for people
t<- V(full)$name %in% orgs[,1]
vertex_attr(full)$type[t]<- 2 #Type 2 for organizations
t<- V(full)$name %in% degrees[,3]
vertex_attr(full)$type[t]<- 3 #Type 3 for universities
t<- V(full)$name %in% sub[,1]
vertex_attr(full)$type[t]<- 4 #Type 3 for sub-organizations

#vertex_attr(full)$shape<- c("circle","square","vrectangle")[vertex_attr(full)$type]
vertex_attr(full)$degree<- degree(full, mode="all") #Count the degree for each node
vertex_attr(full)$degree<- vertex_attr(full)$degree+30

titles<-vector(length=length(V(full)$name))
titles[vertex_attr(full)$type==1]<- "Person"
titles[vertex_attr(full)$type==3]<- "University"
titles[vertex_attr(full)$type==2]<- "Organization"
titles[vertex_attr(full)$type==4]<- "Sub-organization"

country<-vector(length=length(V(full)$name)) #Country information for organizations
country<- as.character(with(orgs, country_code[match(V(full)$name, org_uuid)]))

categ<-"" #Sector information for organizations
categ<- as.character(with(orgs, categories_groups[match(V(full)$name, org_uuid)]))
categ[vertex_attr(full)$type==3]<- "education"

for (i in seq(1,length(categ))) {
  if (!is.na(categ[i])) {
    if (nchar(categ[i])!=0){
      categ[i]<- tail(strsplit(categ[i],';')[[1]])
    }
  }
}  

str<- ""
i<- length(sub[,1])
str[1:i]<- "sub-organization"

start<- edge_attr(full)$started_on_1
start[is.na(start)]<-  edge_attr(full)$started_on_2[is.na(start)]

text<-vector(length=length(V(full)$name)) 
for (i in seq(1,length(text),1)){
  if (V(full)$name[i] %in% people$person_uuid){
    text[i]<- paste(colnames(people)[2],":",as.character(people[people$person_uuid==V(full)$name[i],2]))
    for (j in seq(3,ncol(people))){
      text[i]<- paste0(text[i],"</p>",colnames(people)[j], ":", as.character(people[people$person_uuid==V(full)$name[i],j]))
    }
  }
  if (V(full)$name[i] %in% orgs$org_uuid){
    text[i]<- paste(colnames(orgs)[2],":",as.character(orgs[orgs$org_uuid==V(full)$name[i],2]))
    for (j in seq(3,ncol(orgs))){
      text[i]<- paste0(text[i],"</p>",colnames(orgs)[j], ":", as.character(orgs[orgs$org_uuid==V(full)$name[i],j]))
    }
  }
  if (V(full)$name[i] %in% degrees$to){
    text[i]<- "University"
  }
}

nodes<- data.frame(id=vertex_attr(full)$name,label=titles, value=vertex_attr(full)$degree,
                   group=titles, country=country, sector=categ, title=text)
edges<- data.frame(from=c(as.character(degrees$from),as.character(jobs$from), as.character(sub$from))
                   , to=c(as.character(degrees$to),as.character(jobs$to), as.character(sub$to)), label=
                     c(as.character(degrees$degree_level),as.character(jobs$title),str), arrows="")

#Degree
edges_deg<- degrees[,2:6]
deg <- graph.data.frame(edges_deg, directed = F)
names(edges_deg)[names(edges_deg) == "degree_level"] <- "label"
edges_deg$label<- as.character(ifelse(!is.na(edges_deg$started_on), 
                                      paste0(edges_deg$label,", Started on: ", 
                                             edges_deg$started_on,",Completed on: ", edges_deg$completed_on, sep=""),
                                      as.character(edges_deg$label)))

nodes_deg<- data.frame(id=V(deg)$name)
t<- nodes_deg$id %in% people[,1] 
nodes_deg$group[t]<- "Person" #Type 1 for people
t<- nodes_deg$id %in% degrees[,3]
nodes_deg$group[t]<- "University"

nodes_deg$label<-""
nodes_deg$size<- degree(deg, mode="all")*10

text<-vector(length=length(V(deg)$name)) 
for (i in seq(1,length(text),1)){
  if (V(deg)$name[i] %in% people$person_uuid){
    text[i]<- paste(colnames(people)[2],":",as.character(people[people$person_uuid==V(deg)$name[i],2]))
    for (j in seq(3,ncol(people))){
      text[i]<- paste0(text[i],"</p>",colnames(people)[j], ":", as.character(people[people$person_uuid==V(full)$name[i],j]))
    }
  }
  if (V(deg)$name[i] %in% orgs$org_uuid){
    text[i]<- paste(colnames(orgs)[2],":",as.character(orgs[orgs$org_uuid==V(deg)$name[i],2]))
    for (j in seq(3,ncol(orgs))){
      text[i]<- paste0(text[i],"</p>",colnames(orgs)[j], ":", as.character(orgs[orgs$org_uuid==V(deg)$name[i],j]))
    }
  }
  if (V(deg)$name[i] %in% degrees$to){
    text[i]<- "University"
  }
}

nodes_deg$title<- text

#Jobs
j <- graph.data.frame(jobs[,2:6], directed = F) 
nodes_j<- data.frame(id=V(j)$name)
t<- nodes_j$id %in% people[,1] 
nodes_j$group[t]<- "Person" #Type 1 for people
t<- nodes_j$id %in% orgs[,1]
nodes_j$group[t]<- "Organization"

nodes_j$label<-""

edges_j<- jobs[,2:6]
names(edges_j)[names(edges_j) == "title"] <- "label"
edges_j$label<- as.character(ifelse(!is.na(edges_j$started_on), 
                                    paste0(edges_j$label,", Started on: ", 
                                           edges_j$started_on,",Ended on: ", edges_j$ended_on, sep=""),
                                    as.character(edges_j$label)))

j<-graph.data.frame(edges_j, directed = F) 
nodes_j$size<- degree(j, mode="all")+5

text<-vector(length=length(V(j)$name)) 
for (i in seq(1,length(text),1)){
  if (V(j)$name[i] %in% people$person_uuid){
    text[i]<- paste(colnames(people)[2],":",as.character(people[people$person_uuid==V(j)$name[i],2]))
    for (k in seq(3,ncol(people))){
      text[i]<- paste0(text[i],"</p>",colnames(people)[k], ":", as.character(people[people$person_uuid==V(j)$name[i],k]))
    }
  }
  if (V(j)$name[i] %in% orgs$org_uuid){
    text[i]<- paste(colnames(orgs)[2],":",as.character(orgs[orgs$org_uuid==V(j)$name[i],2]))
    for (k in seq(3,ncol(orgs))){
      text[i]<- paste0(text[i],"</p>",colnames(orgs)[k], ":", as.character(orgs[orgs$org_uuid==V(j)$name[i],k]))
    }
  }
  if (V(j)$name[i] %in% degrees$to){
    text[i]<- "University"
  }
}

nodes_j$title<- text

#Sub network
nodes_sub<- orgs
names(nodes_sub)[names(nodes_sub) == "org_uuid"] <- "id"

t<- nodes_sub$id %in% sub[,1] 
nodes_sub$group[t]<- "Sub-organization" #Type 1 for sub
t<- nodes_sub$id %in% sub[,2]
nodes_sub$group[t]<- "Organization"

edges_sub<- sub

text<-vector(length=length(nodes_sub$id)) 
for (i in seq(1,length(text),1)){
  if (nodes_sub$id[i] %in% orgs$org_uuid){
    text[i]<- paste(colnames(nodes_sub)[2],":",as.character(nodes_sub[nodes_sub$id==nodes_sub$id[i],2]))
    for (k in seq(3,9,1)){
      text[i]<- paste0(text[i],"</p>",colnames(nodes_sub)[k], ":", as.character(nodes_sub[nodes_sub$id==nodes_sub$id[i],k]))
    }
  }
}
nodes_sub$title<- text

vis<- function(nodes,links){
  visNetwork(nodes, links,height = "600px", width = "100%") %>%
  visNodes(id=NULL, title=NULL, label=NULL) %>%
  visNodes(id=NULL, title=NULL, label=NULL, color = list(highlight = "orange", hover = "orange")) %>%
  visEdges(color = list(highlight = "darkblue", hover = "darkblue")) %>%
  visGroups(groupname = "Person", color = list(background = "lightblue", 
                                               border = "darkblue"),
            shadow = list(enabled = TRUE)) %>% 
  visGroups(groupname = "University", color = list(background = "yellow", 
                                                   border = "orange"), 
            shadow = list(enabled = TRUE)) %>% 
  visGroups(groupname = "Organization", color = list(background = "red", 
                                                     border = "black"),
            shadow = list(enabled = TRUE)) %>% 
  visGroups(groupname = "Sub-Organization", color = list(background = "brown", 
                                                         border = "black"), 
            shadow = list(enabled = TRUE)) %>% 
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
             nodesIdSelection = list(enabled = TRUE)) %>%
  visInteraction(hover = T) %>%
  visEvents(hoverNode = "function(n){
            this.body.data.nodes.update({id: n.node, font: {size : 14}});
            }") %>%
  visEvents(blurNode = "function(n){
            this.body.data.nodes.update({id: n.node, font: {size : 0}});
  }") %>%
  visEvents(hoverEdge = "function(e){
            this.body.data.edges.update({id: e.edge, font: {size : 14}});
  }") %>%
  visEvents(blurEdge = "function(e){
            this.body.data.edges.update({id: e.edge, font: {size : 0}});
  }") %>%
  visLegend()
  }


library(shiny)
library(visNetwork)
library(ndtv)
library(network)
library(igraph)
library(dplyr)
library(intergraph)

vis<- function(nodes,links){ #Network visualization function. Just use vis(nodes,edges).
  visNetwork(nodes, links,height = "600px", width = "100%") %>%
    visNodes(id=NULL, title=NULL, label=NULL) %>%
    visNodes(id=NULL, title=NULL, label=NULL, color = list(highlight = list(background = "green", 
                                                                            border = "black"), hover = list(background = "gold", 
                                                                                                            border = "black"))) %>%
    visEdges(color = list(highlight = "darkblue", hover = "darkblue")) %>%
    visGroups(groupname = "Person", color = list(background = "lightblue", 
                                                 border = "darkblue"),
              shadow = list(enabled = TRUE)) %>% 
    visGroups(groupname = "University", color = list(background = "yellow", 
                                                     border = "black"), 
              shadow = list(enabled = TRUE)) %>% 
    visGroups(groupname = "Organization", color = list(background = "red", 
                                                       border = "black"),
              shadow = list(enabled = TRUE)) %>% 
    visGroups(groupname = "Sub-Organization", color = list(background = "lightgreen", 
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
  }")
}


people<- read.csv("people.csv")
jobs<- read.csv("jobs.csv")
orgs<- read.csv("organizations.csv")
degrees<- read.csv("degrees.csv")
sub<- read.csv("sub_organizations.csv")
acquisitions<- read.csv("acquisitions.csv")
frounds<- read.csv("funding_rounds.csv")
investors<- read.csv("investors_per_funding_round.csv")
ipos<- read.csv("ipos_per_company.csv")
add<- read.csv("additional.csv")
orgs<- rbind(orgs,add)

{ #Add a column with full details of a person
text<-vector(length=length(people$person_uuid)) 
for (i in seq(1,length(text),1)){
    text[i]<- paste(colnames(people)[2],":",as.character(people[i,2]))
    for (j in seq(3,ncol(people))){
      text[i]<- paste0(text[i],"</p>",colnames(people)[j], ":", as.character(people[i,j]))
    }
  }
people$details<- text
}

{ #Add a column with full details of a job
  jobs$details<- jobs$title
  jobs$details<- as.character(ifelse(jobs$started_on!="", 
                                     paste0(jobs$details,", Started on: ", 
                                            jobs$started_on,",Ended on: ", jobs$ended_on, sep=""),
                                     as.character(jobs$details)))
}

{ #Add a column with full details of an organization
  text<-vector(length=length(orgs$org_uuid)) 
  for (i in seq(1,length(text),1)){
    text[i]<- paste(colnames(orgs)[2],":",as.character(orgs[i,2]))
    for (j in seq(3,ncol(orgs))){
      text[i]<- paste0(text[i],"</p>",colnames(orgs)[j], ":", as.character(orgs[i,j]))
    }
  }
  orgs$ipo<- orgs$org_uuid %in% ipos$org_uuid
  orgs$details<- text
  
  text<-vector(length=length(ipos$ipo_uuid)) 
  for (i in seq(1,length(text),1)){
    text[i]<- paste(colnames(ipos)[3],":",as.character(ipos[i,3]))
    for (j in seq(4,ncol(ipos))){
      text[i]<- paste0(text[i],"</p>",colnames(ipos)[j], ":", as.character(ipos[i,j]))
    }
  }
  ipos$details<- text
  
  for (i in seq(1,length(ipos$ipo_uuid),1)){
    orgs$details[as.character(ipos$org_uuid[i])==as.character(orgs$org_uuid)]<- paste0(as.character(orgs$details[as.character(ipos$org_uuid[i])==as.character(orgs$org_uuid)]),"</p>", 
                                                                                    as.character(ipos$details[i]))
  }
  
  #Get all the unique categories
  s<-""
  for (i in seq(1, length(orgs$org_uuid))){
    s<- paste(s, orgs$categories_groups[i], sep = ";")
  }
  categories<- strsplit(s,";")
  categories<- unique(categories[[1]])
  categories<- categories[-1]
}

{ #Add a column with full details of a degree
  degrees$details<- degrees$degree_level
  degrees$details<- as.character(ifelse(degrees$started_on!="", 
                                     paste0(degrees$details,", Started on: ", 
                                            degrees$started_on,",Ended on: ", degrees$completed_on, sep=""),
                                     as.character(degrees$details)))
}

{ #Initialize nodes/edges for sub-organizations graph
edges_sub<- data.frame(from=sub$sub_org_uuid, to=sub$org_uuid, label="Sub-Organization", arrows="",
                         onset=as.Date("",format="%Y-%m-%d") 
                         ,terminus=as.Date("",format="%Y-%m-%d"), type=5)
nodes_sub<- data.frame(id=unique(c(as.character(edges_sub$from),as.character(edges_sub$to))))
t<- nodes_sub$id %in% edges_sub$from 
nodes_sub$group[t]<- "Sub-Organization"
nodes_sub$group[!t]<- "Organization"
nodes_sub$label<- ""
for (i in seq(1:length(nodes_sub$id))){
  nodes_sub$title[i]<- orgs$details[as.character(orgs$org_uuid)==as.character(nodes_sub$id[i])]
}

for (i in seq(1,length(edges_sub$from),1)){
  edges_sub$onset[i]<- as.Date(orgs$founded_on[as.character(orgs$org_uuid)==edges_sub$from[i]],format="%Y-%m-%d")
}

vis(nodes_sub, edges_sub)
}

{ #Initialize nodes/edges for jobs graph
  edges_j<- data.frame(from=jobs$person_uuid, to=jobs$org_uuid, label= jobs$details, arrows="",
                       onset=as.Date(jobs$started_on,format="%Y-%m-%d") 
                       ,terminus=as.Date(jobs$ended_on,format="%Y-%m-%d"), type=2)
  nodes_j<- data.frame(id=unique(c(as.character(edges_j$from),as.character(edges_j$to))))
  nodes_j$id<- as.character(nodes_j$id)
  t<- nodes_j$id %in% edges_j$from 
  nodes_j$group[t]<- "Person"
  nodes_j$group[!t]<- "Organization"
  nodes_j$label<- ""
  nodes_j$title<- ""
  for (i in seq(1:length(nodes_j$id))){
    if (nodes_j$group[i]=="Organization") {
      nodes_j$title[i]<- orgs$details[orgs$org_uuid==nodes_j$id[i]]
    }
    else {
      nodes_j$title[i]<- people$details[people$person_uuid==nodes_j$id[i]]
    }
  }
  
  vis(nodes_j, edges_j)
}

{ #Initialize nodes/edges for degrees graph
  edges_deg<- data.frame(from=degrees$person_uuid, to=degrees$org_school_uuid, label=degrees$details, arrows="",
                         onset=as.Date(degrees$started_on,format="%Y-%m-%d") 
                         ,terminus=as.Date(degrees$completed_on,format="%Y-%m-%d"), type=3)
  nodes_deg<- data.frame(id=unique(c(as.character(edges_deg$from),as.character(edges_deg$to))))
  nodes_deg$id<- as.character(nodes_deg$id)
  t<- nodes_deg$id %in% edges_deg$from 
  nodes_deg$group[t]<- "Person"
  nodes_deg$group[!t]<- "University"
  nodes_deg$label<- ""
  nodes_deg$title<- ""
  
  for (i in seq(1:length(nodes_deg$id))){
    if (nodes_deg$group[i]=="University") {
      if (nodes_deg$id[i] %in% orgs$org_uuid){  
        nodes_deg$title[i]<- orgs$details[orgs$org_uuid==nodes_deg$id[i]]
      }
    }
    else {
      nodes_deg$title[i]<- people$details[people$person_uuid==nodes_deg$id[i]]
    }
  }
  
  vis(nodes_deg, edges_deg)
}

{ #funding rounds preparation
edges_fr<- frounds
names(edges_fr)[names(edges_fr) == "funding_round_uuid"] <- "from" #round
names(edges_fr)[names(edges_fr) == "org_uuid_received_funding"] <- "to" #organization
text<-vector(length=length(edges_fr$from)) 
for (i in seq(1,length(text),1)){
  text[i]<- paste0(colnames(edges_fr)[3],":",edges_fr[i,3])
    for (k in seq(4,8,1)){
      text[i]<- paste0(text[i],", ",colnames(edges_fr)[k], ": ", edges_fr[i,k])
    }
}

edges_fr$label<- text
edges_fr$arrows=""

nodes_fr<- data.frame(id=c(as.character(unique(edges_fr$from)), as.character(unique(edges_fr$to))))
t<- nodes_fr$id %in% frounds[,1] 
nodes_fr$group[t]<- "Funding Round" 
t<- nodes_fr$id %in% frounds[,2] 
nodes_fr$group[t]<- "Organization" 

nodes_fr$title<-""
nodes_fr$label<-""

vis(nodes_fr, edges_fr) #Visualisation
}

{ #investors preparation
invest<- data.frame(matrix(ncol=3),stringsAsFactors = FALSE)
colnames(invest)<- c("id","investor","group")
for (i in investors$funding_round_uuid){
  if(!investors[investors$funding_round_uuid==i,2]==""){
    for (k in strsplit(as.character(investors[investors$funding_round_uuid==i,2]),split=";")){
      invest<- rbind(invest,c(round=i, investor=k, group="Person"))
    }
  }
  if(!investors[investors$funding_round_uuid==i,3]==""){
    for (k in unlist(strsplit(as.character(investors[investors$funding_round_uuid==i,3]),split=";"))){
      t<-c(as.character(i), as.character(k),"Organization")
      invest<- rbind(invest,t)
    }
  }
}
invest<-invest[-1,]
edges_inv<- invest[,1:2]
edges_inv$label<-""
edges_inv$arrows<-""

colnames(edges_inv)<- c("from","to","label",'arrows')
nodes_inv<- data.frame(id=c(as.character(unique(edges_inv$from)), as.character(unique(edges_inv$to))))
t<- nodes_inv$id %in% invest[,1] 
nodes_inv$group[t]<- "Funding Round" 
t<- nodes_inv$id %in% invest[invest$group=="Organization",2]  
nodes_inv$group[t]<- "Organization"
t<- nodes_inv$id %in% invest[invest$group=="Person",2]  
nodes_inv$group[t]<- "Person"
nodes_inv$label<- ""
nodes_inv$title<- ""

vis(nodes_inv, edges_inv) #Visualisation
}

{ #acquisitions preparation
edges_acq<- acquisitions
names(edges_acq)[names(edges_acq) == "acquirer_uuid"] <- "from" #acquirer
names(edges_acq)[names(edges_acq) == "acquiree_uuid"] <- "to" #acquiree
edges_acq<-edges_acq[,-1]
edges_acq$label<- paste0(edges_acq$acquisition_type)
edges_acq$label<- as.character(ifelse(!is.na(edges_acq$price_usd), 
                                    paste0(edges_acq$label,", Price($): ", 
                                           as.numeric(edges_acq$price_usd)),
                                    as.character(edges_acq$label)))
edges_acq$label<- as.character(ifelse(!is.na(edges_acq$announced_on), 
                                      paste0(edges_acq$label,", Announced on: ", 
                                             edges_acq$announced_on),
                                      as.character(edges_acq$label)))

edges_acq<-edges_acq[,4:6]
edges_acq$arrows<- "to"
edges_acq$onset<- as.Date(acquisitions$announced_on, format="%Y-%m-%d")
edges_acq$terminus<- as.Date("",format="%Y-%m-%d")
edges_acq$type<- 1

nodes_acq<- data.frame(id=c(as.character(unique(edges_acq$from)), as.character(unique(edges_acq$to))))
nodes_acq$group<- "Organization" 
nodes_acq$label<- ""
nodes_acq$title<- "" #Fix with additional data
for (i in seq(1, length(nodes_acq$id),1)) {
  if (nodes_acq$id[i] %in% orgs$org_uuid){
    nodes_acq$title[i]<- orgs$details[nodes_acq$id[i]==orgs$org_uuid]
  }
}

vis(nodes_acq, edges_acq) #Visualisation
}

{ ##Organisation-Funding Round-Investor network
edges_fund<- rbind(edges_fr[,c(1,2,9,10)], edges_inv)
nodes_inv$title<- "Investor"
nodes_inv$label=""
nodes_fund<-   rbind(nodes_fr,nodes_inv[!nodes_inv$id %in% nodes_fr$id,])

vis(nodes_fund,edges_fund)

##Eliminate Funding rounds
edges_ <- data.frame(from=character(), to=character(), series=character(),
                     funding_type=character(), money_raised_usd=numeric(),
                     pre_money_valuation_usd=numeric(),target_money_raised_usd=numeric(),announced_on=character()
                     , stringsAsFactors = F)

for (fround in edges_fr[,1]){
  if (fround %in% edges_inv$from) {
    for (i in seq(1,nrow(edges_inv),1)){
      if (edges_inv$from[i]==fround){
        edges_<- rbind(edges_,c(from=as.character(edges_inv$to[i]),edges_fr[edges_fr$from==fround,2:8]))
      }
    }
    edges_$from<- as.character(edges_$from)
    edges_$to<- as.character(edges_$to)
    edges_$series<- as.character(edges_$series)
    edges_$funding_type<- as.character(edges_$funding_type)
    edges_$announced_on<- as.character(edges_$announced_on)
  }
}

text<-vector(length=length(edges_$from)) 
for (i in seq(1,length(text),1)){
  text[i]<- paste0(colnames(edges_)[3],":",edges_[i,3])
  for (k in c(4,5,8)){
    text[i]<- paste0(text[i],", ",colnames(edges_)[k], ": ", edges_[i,k])
  }
}

edges_$label<- text
#edges_fr<- edges_fr[,c(1,2,9,10)]
edges_$arrows<-"to"
edges_$onset<- as.Date(edges_$announced_on, format="%Y-%m-%d")
edges_$terminus<- as.Date("", format="%Y-%m-%d")
edges_$type<- 4

nodes_<- nodes_fund[nodes_fund$group!="Funding Round",]

for (i in seq(1, length(nodes_$id),1)) {
  if (nodes_$id[i] %in% orgs$org_uuid){
    nodes_$title[i]<- orgs$details[nodes_$id[i]==orgs$org_uuid]
  }
}

edges_<- edges_[,c(1,2,9,10,11,12,13)]


vis(nodes_,edges_)
}

{ ##Funding - Acquisition network
edges_<- rbind(edges_,edges_acq)

nodes_<- rbind(nodes_, nodes_acq[!nodes_acq$id %in% nodes_$id,])

vis(nodes_, edges_)
}

{ #Combine graphs. Nodes are added one at a time to check for duplicates
edges_full<- rbind(edges_, edges_deg, edges_j, edges_sub)
nodes_full<- nodes_j
nodes_full<- rbind(nodes_full, nodes_sub[!nodes_sub$id %in% nodes_full$id,])
nodes_full<- rbind(nodes_full, nodes_deg[!nodes_deg$id %in% nodes_full$id,])
nodes_full<- rbind(nodes_full, nodes_[!nodes_$id %in% nodes_full$id,])

#Calculate the degree and assign it as node size attribute
graph <- graph.data.frame(edges_full)
degree <- degree(graph, mode = "all")
nodes_full$value <- degree[match(nodes_full$id, names(degree))]

t<- vector(length=length(nodes_full$id))
for (i in seq(1, length(nodes_full$id),1)){
  if (nodes_full$id[i] %in% edges_full$to) {
    t[i]<- edges_full$type[edges_full$to== nodes_full$id[i]] == 1
  }
}
nodes_full$title[t]<- "Acquiree"

vis(nodes_full, edges_full)
}





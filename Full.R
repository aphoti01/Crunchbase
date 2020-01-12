nodes_full<- nodes[,c(1,4,2,7)]
nodes_full$id<- as.character(nodes_full$id)
rownames(nodes_full)<- 1:length(nodes_full$id)

#Add acquisition entities
nodes_full<- rbind(nodes_full, data.frame(id=nodes_acq$id[!nodes_acq$id %in% nodes_full$id], 
                                          group="Organization",label="",title="Acquiree"))

#Add investors
t<- nodes_inv$id %in% invest$investor[invest$group=="Organization"] 
nodes_inv$label[t]<-"Organization"
t<- nodes_inv$id %in% invest$investor[invest$group=="Person"] 
nodes_inv$label[t]<-"Person"
nodes_full<- rbind(nodes_full, data.frame(id=nodes_inv$id[nodes_inv$group!="Funding Round"], 
                                          group=nodes_inv$label[nodes_inv$group!="Funding Round"],
                                          label="",title="Investor"))

#nodes_full$shape<-"circle"
#nodes_full$shape[nodes_full$label=="Investor"]<- "square"
#nodes_full$shape<- NULL

#Edges
edges_full<- rbind(edges,edges_)
arrows<- vector(length=length(edges_full$from))
arrows[1:length(arrows)]<- ""
arrows[176:227]<- "to"

edges_full$arrows= arrows

nodes_full$value<-NULL
nodes_full$label<- ""

vis(nodes_full, edges_full)



shinyServer(function(input, output) {
  
    nodes <- nodes_full
    names(nodes)[names(nodes) == "title"] <- "title1"
    
    edges <- edges_full
    #nodes$jobs<- ifelse(nodes$id %in% edges$from[edges$type==2] | nodes$id %in% edges$to[edges$type==2],2,0)
    #nodes$deg<- ifelse(nodes$id %in% edges$from[edges$type==3] | nodes$id %in% edges$to[edges$type==3],3,0)
    #nodes$sub<- ifelse(nodes$id %in% edges$from[edges$type==5] | nodes$id %in% edges$to[edges$type==5],5,0)
    #nodes$inv<- ifelse(nodes$id %in% edges$from[edges$type==4] | nodes$id %in% edges$to[edges$type==4],4,0)
    #nodes$acq<- ifelse(nodes$id %in% edges$from[edges$type==1] | nodes$id %in% edges$to[edges$type==1],1,0)
    
    data<- reactive({
        nodes_fil<- nodes
        edges_fil<- edges
        
        if (!is.null(input$selectgraph)){
          t<- vector(length=length(edges_fil$from))
          t<- edges_fil$type %in% input$selectgraph
          edges_fil<- edges_fil[t,]
          t<- vector(length=length(nodes_fil$id))
          t<-  nodes_fil$id %in% edges_fil$to | nodes_fil$id %in% edges_fil$from 
          nodes_fil<- nodes_fil[t,]
        }
        if (!is.null(input$dateRange) & input$enDate){
          from <- as.Date(input$dateRange[1])
          to <- as.Date(input$dateRange[2])
          t<- vector(length=length(edges_fil$from))
          for (i in seq(1, length(t),1)) {
            if (edges_fil$type[i]==1){
              if (edges_fil$onset[i]<to) {
                t[i]<- TRUE
              } else {
                t[i]<- FALSE
              }
            } else if(edges_fil$type[i]==2){
              if (!is.null(edges_fil$onset[i])){
                if (!is.null(edges_fil$terminus[i])){
                  t[i]<- between(edges_fil$terminus[i],from,to) | between(edges_fil$onset[i],from,to) | (edges_fil$onset[i]<from & edges_fil$terminus[i]>to)
                } else {
                  t[i]<- edges_fil$onset[i]< to
                }
              } else {
                t[i]<- FALSE
              }
            } else if(edges_fil$type[i]==3){
              if (!is.null(edges_fil$terminus[i])){
                if (!is.null(edges_fil$onset[i])){
                  t[i]<- between(edges_fil$terminus[i],from,to) | between(edges_fil$onset[i],from,to) | (edges_fil$onset[i]<from & edges_fil$terminus[i]>to)
                } else {
                  t[i]<- between(edges_fil$terminus[i],from,to)
                }
              } else {
                t[i]<- FALSE
              }
            } else if(edges_fil$type[i]==4){
              if (edges_fil$onset[i]<to) {
                t[i]<- TRUE
              } else {
                t[i]<- FALSE
              }
            } else {
              if (!is.na(edges_fil$onset[i])){
                if (edges_fil$onset[i]<to) {
                  t[i]<- TRUE
                } else {
                  t[i]<- FALSE
                }} else {
                t[i]<- FALSE
              }}
          }
          edges_fil<- edges_fil[t,]
          edges_fil<- edges_fil[!is.na(edges_fil$from),]
          
          t<- vector(length=length(nodes_fil$id))
          t[1:length(t)]<- T
          for (i in seq(1,length(nodes_fil$id),1)){
            if (nodes_fil$id[i] %in% orgs$org_uuid){
              if (orgs$founded_on[orgs$org_uuid==nodes_fil$id[i]]!=""){
                t[i]<- as.Date(orgs$founded_on[orgs$org_uuid==nodes_fil$id[i]]) < to
              }
            }
          }
          
          
          #t<- nodes_fil$id %in% edges_fil$from | nodes_fil$id %in% edges_fil$to
          nodes_fil<- nodes_fil[t,]
        }
        
        if (!is.null(input$country)){
          f<- orgs$org_uuid[orgs$country_code %in% input$country]
          t<- nodes_fil$id %in% f
          t[!nodes_fil$group %in% c("Organization","Sub-Organization")]<- T
          t[nodes_fil$title1 %in% c("Investor", "Acquiree")] <-T
          nodes_fil<-nodes_fil[t,]
        } 
        
        if (!is.null(input$category)){
          t<- vector(length=length(nodes_fil$id))
          t[1:length(t)]<- T
          for (i in seq(1, length(nodes_fil$id),1)){
            if ((nodes_fil$group[i] %in% c("Organization", "Sub-Organization")) & !(nodes_fil$title1[i] %in% c("Investor", "Acquiree"))){
              t[i]<- F
              for (category in input$category){
                t[i]<- t[i] | grepl(category, orgs$categories_groups[orgs$org_uuid== nodes_fil$id[i]])
              }
            }
          }
          nodes_fil<- nodes_fil[t,]
        }
        
        if (input$twitter){
          t<- vector(length = length(nodes_fil$id))
          for (i in seq(1,length(nodes_fil$id))){
            if (nodes_fil$id[i] %in% people$person_uuid) {
              t[i]<- as.logical(people$has_Twitter[people$person_uuid== nodes_fil$id[i]])
            } else {
              t[i]<- TRUE
            }
          }
          nodes_fil<- nodes_fil[t,]
        }
        
        if (input$ipo){
          t<- vector(length = length(nodes_fil$id))
          t<- as.logical(t)
          for (i in seq(1,length(nodes_fil$id))){
            if (nodes_fil$id[i] %in% orgs$org_uuid) {
              t[i]<- as.logical(orgs$ipo[orgs$org_uuid==nodes_fil$id[i]])
            } else {
              t[i]<- TRUE
            }
          }
          nodes_fil<- nodes_fil[t,]
        }
        
        tt<- vector(length = length(edges_fil$from))
        tt[1:length(tt)]<- TRUE
        
        if (!is.null(input$degree)){
          for (i in seq(1,length(edges_fil$from))) {
            tt[i]<- (edges_fil$type[i] == 3 & strsplit(edges_fil$label[i], ",")[[1]][1] %in% input$degree) | edges_fil$type[i] != 3
            }
        }
        
        if (!is.null(input$job)){
          for (i in seq(1,length(edges_fil$from))) {
            tt[i]<- (edges_fil$type[i] == 2 & strsplit(edges_fil$label[i], ",")[[1]][1] %in% input$job) | (edges_fil$type[i] != 2 & tt[i])
          }
        }
        
        if (!is.null(input$fr)){
          for (i in seq(1,length(edges_fil$from))) {
            if (edges_fil$type[i]==4 ){
              tt[i]<- length(strsplit(strsplit(edges_fil$label[i],",")[[1]],":")[[1]])==2
              if (tt[i] == T & edges_fil$type[i]==4 ){
                tt[i]<- tt[i] & strsplit(strsplit(edges_fil$label[i],",")[[1]],":")[[1]][2] %in% input$fr
              }
            }
          }
        }
        
        if (input$moneyfr!=""){
          amount= as.numeric(input$moneyfr) * 1000000
          for (i in seq(1,length(edges_fil$from))) {
            if (edges_fil$type[i]==4 ){
              tt[i]<- length(strsplit(strsplit(edges_fil$label[i],",")[[1]][3],":")[[1]])==2
              if (tt[i] == T){
                tt[i]<- as.numeric(strsplit(strsplit(edges_fil$label[i],",")[[1]][3],":")[[1]][2]) >= amount
              }
            }
          }
        }
        
        if (input$moneyipo!=""){
          amount= as.numeric(input$moneyipo) * 1000000
          t<- vector(length= length(nodes_fil$id))
          t[1:length(t)]<- T
          for (i in seq(1,length(nodes_fil$id))) {
            if (nodes_fil$id[i] %in% ipos$org_uuid ){
              if (!is.na(ipos$money_raised_usd[ipos$org_uuid==nodes_fil$id[i]])){
                t[i]<- as.numeric(ipos$money_raised_usd[ipos$org_uuid==nodes_fil$id[i]]) >= as.numeric(amount)
              }
            }
          }
          nodes_fil<- nodes_fil[t,]
        }
        
        edges_fil<- edges_fil[tt,]
          
        t<- vector(length=length(edges_fil$from))
        t<- edges_fil$from %in% nodes_fil$id & edges_fil$to %in% nodes_fil$id
        edges_fil<- edges_fil[t,]
        
        #t<- vector(length= length(nodes_fil$id))
        #t<- nodes_fil$id %in% edges_fil$from | nodes_fil$id %in% edges_fil$to
        #nodes_fil<- nodes_fil[t,]
        
        list(nodes= nodes_fil, edges=edges_fil)
    })
    
    output$network_full <- renderVisNetwork({
        
        vis(nodes, edges)
        
    })
    
    output$view_id_full <- renderText({
        paste("Current node selection : ", input$network_full_selected)
    })
    
    output$attributes_full <- renderPrint({
        cat(gsub("</p>","\n",data()$nodes$title1[input$network_full_selected==data()$nodes$id]))
    })
    
    observe({
        if (input$deg>1){    
            col <- paste0("rgba(200,200,200,", 1, ")")
            visNetworkProxy("network_full") %>%
                visOptions(highlightNearest = list(enabled = T, degree = input$deg, hideColor = col, hover =T))
        }
    })
    
    observeEvent(input$filter ,{
        output$network_full <- renderVisNetwork({
            
            vis(data()$nodes, data()$edges)
            
        })
    })
    

})



shinyServer(function(input, output) {
  
  nodes <- nodes_full
  names(nodes)[names(nodes) == "title"] <- "title1"
  
  edges <- edges_full
  
  nodes$jobs<- ifelse(nodes$id %in% edges$from[edges$type==2] | nodes$id %in% edges$to[edges$type==2],2,0)
  nodes$deg<- ifelse(nodes$id %in% edges$from[edges$type==3] | nodes$id %in% edges$to[edges$type==3],3,0)
  nodes$sub<- ifelse(nodes$id %in% edges$from[edges$type==5] | nodes$id %in% edges$to[edges$type==5],5,0)
  nodes$inv<- ifelse(nodes$id %in% edges$from[edges$type==4] | nodes$id %in% edges$to[edges$type==4],4,0)
  nodes$acq<- ifelse(nodes$id %in% edges$from[edges$type==1] | nodes$id %in% edges$to[edges$type==1],1,0)
  
  data<- reactive({
    nodes_fil<- nodes
    edges_fil<- edges
    "if (!is.null(input$dateRange)){
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
              t[i]<- TRUE
            }
          }
          edges_fil<- edges_fil[t,]
          t<- nodes_fil$id %in% edges_fil$from | nodes_fil$id %in% edges_fil$to
          nodes_fil<- nodes_fil[t,]
        }"
    
    if (!is.null(input$country)){
      f<- orgs$org_uuid[orgs$country_code %in% input$country]
      t<- nodes_fil$id %in% f
      t[!nodes_fil$group %in% c("Organization","Sub-Organization")]<- T
      nodes_fil<-nodes_fil[t,]
      
      t<- edges_fil$from %in% nodes_fil$id & edges_fil$to %in% nodes_fil$id
      edges_fil<- edges_fil[t,]
    } 
    
    if (!is.null(input$category)){
      f<- orgs$org_uuid[orgs$categories_groups %in% input$category]
      t<- nodes_fil$id %in% f
      t[!nodes_fil$group %in% c("Organization","Sub-Organization")]<- T
      nodes_fil<-nodes_fil[t,]
      
      t<- edges_fil$from %in% nodes_fil$id & edges_fil$to %in% nodes_fil$id
      edges_fil<- edges_fil[t,]
    }
    
    if (!is.null(input$degree)){
      t<- vector(length = length(edges_fil$from))
      for (i in seq(1,length(edges_fil$from))) {
        t[i]<- (edges_fil$type[i] == 3 & strsplit(edges_fil$label[i], ",")[[1]][1] %in% input$degree)
      }
      edges_deg<- edges_fil[t,]
    }
    
    if (!is.null(input$job)){
      t<- vector(length = length(edges_fil$from))
      for (i in seq(1,length(edges_fil$from))) {
        t[i]<- (edges_fil$type[i] == 2 & strsplit(edges_fil$label[i], ",")[[1]][1] %in% input$job)
      }
      edges_job<- edges_fil[t,]
    }
    if (!is.null(input$job) | !is.null(input$degree)){
      if (!is.null(input$job) & !is.null(input$degree)){
        edges_fil<- rbind(edges_deg,edges_job)
      } else if (!is.null(input$job)){
        edges_fil<- edges_job
      } else {
        edges_fil<- edges_deg
      }
      t<- nodes_fil$id %in% edges_fil$from | nodes_fil$id %in% edges_fil$to
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
      t<- vector(length=length(edges_fil$from))
      t<- edges_fil$from %in% nodes_fil$id | edges_fil$to %in% nodes_fil$id
      edges_fil<- edges_fil[t,]
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
      t<- vector(length=length(edges_fil$from))
      t<- edges_fil$from %in% nodes_fil$id | edges_fil$to %in% nodes_fil$id
      edges_fil<- edges_fil[t,]
    }
    if (!is.null(input$fr)){
      t<- vector(length = length(edges_fil$from))
      for (i in seq(1,length(edges_fil$from))) {
        if (edges_fil$type[i]==4 ){
          t[i]<- length(strsplit(strsplit(edges_fil$label[i],",")[[1]],":")[[1]])==2
        }
        if (t[i] == T){
          t[i]<- t[i] & strsplit(strsplit(edges_fil$label[i],",")[[1]],":")[[1]][2] %in% input$fr
        }
      }
      edges_fil<- edges_fil[t,]
      t<- nodes_fil$id %in% edges_fil$from | nodes_fil$id %in% edges_fil$to
      nodes_fil<- nodes_fil[t,]
    }
    
    
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
  observeEvent(input$selectgraph ,{
    selections<- input$selectgraph
    if (is.null(selections)){
      output$network_full <- renderVisNetwork({
        
        vis(nodes, edges)
        
      })
    } else {
      t<- nodes$jobs %in% selections | nodes$deg %in% selections | nodes$sub %in% selections | nodes$acq %in% selections | nodes$inv %in% selections
      nodes_s<- nodes[t,]
      t<- edges$from %in% nodes_s$id | edges$to %in% nodes_s$id
      edges_s<- edges[t,]
      output$network_full <- renderVisNetwork({
        
        vis(nodes_s,edges_s)
        
      }) 
    }
  })
  
})

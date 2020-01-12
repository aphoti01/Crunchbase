shinyUI(fluidPage(

    title = "Crunchbase Data",
    fluidRow(
        column(9,
            visNetworkOutput("network_full",height = "400px")
        ),
        column(3,
               verbatimTextOutput('view_id_full'), 
               verbatimTextOutput('attributes_full')
               ) 
        ),
    hr(),
    
    fluidRow(
        column(3,
               h3("Graph Options"),
               
               sliderInput("deg", "Degrees of Separation:", min = 1, max = 10, value = 1),
               selectInput("selectgraph","Select Sub-Graph", c("Acquisitions"=1,"Jobs"=2,"Academic Degree"=3
                                                               ,"Investor"=4,"Sub-Organizations"=5), multiple = T,selected=NULL),
               dateRangeInput('dateRange',
                              label = "Date Range",
                              start = Sys.Date() - 3, end = Sys.Date() + 3,
                              min = Sys.Date() - 30000, max = Sys.Date() + 10, format = "yyyy-mm-dd",
                              startview = 'year',  weekstart = 1
               ),
               checkboxInput("enDate", "Enable Date Filter", FALSE)
               
        ),
        column(3, 
               h3("Organizations Filters"),
               
               selectInput("country", label = "Select Country",
                           choices = as.list(unique(orgs$country_code)), multiple=T, selected = NULL),
               selectInput("category", label = "Select Organization Category",
                           choices = as.list(categories), multiple=T, selected = NULL)
        ),
        column(3,
               h3("People Filters"),
               
               selectInput("degree", label = "Select Academic Degree",
                           choices = as.list(unique(degrees$degree_level)), multiple=T, selected = NULL),
               selectInput("job", label = "Select Job Position",
                           choices = as.list(unique(jobs$title)), multiple=T, selected = NULL),
               checkboxInput("twitter", "Have Twitter", FALSE),
               actionButton("filter", "Filter Graph")
        ),
        column(3,
               h3("Financing Filters"),
               selectInput("fr", label = "Select Funding Round",
                           choices = as.list(unique(frounds$series)), multiple=T, selected = NULL),
               textInput("moneyfr", "Money Raised in Funding Rounds in millions of $ (at least)", value = "", placeholder = "e.g. 4000000"),
               textInput("moneyipo", "Money Raised in IPO in millions of $ (at least)", value = "", placeholder = "e.g. 4000000"),
               checkboxInput("ipo", "IPO", FALSE)
    )
)))

library(shiny)
library(htmlwidgets)
library(visNetwork)
library(arulesViz)
library(arules)
library(RColorBrewer)
#source("MBA.R")

itemList <- NULL
# Define UI ----
ui <- fluidPage(
  titlePanel("Association rules"),
  
  fluidRow(
    column(3,
           sliderInput("supp", h3("Support"),
                       min = 0, max = 1, value = 0.5),
           sliderInput("conf", h3("Confidence"),
                       min = 0, max = 1, value = 0.5),
           br(),
           
           fileInput("fileName", "Choose an Excel file", multiple = FALSE, 
                     accept = c(".xlsx"),
                     width = NULL, buttonLabel = "Browse...",
                     placeholder = "No file selected")
    ),
    column(5,
           selectInput("choice",
                       label = "Select an item to display",
                       choices = c("", itemList),
                       selected = ""
           ),
           
           actionButton("do", "Print"),
           
           textOutput("Transactions"),
           #br(),
           textOutput("AR"),
           #br(),
           verbatimTextOutput("rules"),
           
           dataTableOutput("contents")
           #verbatimTextOutput("contents")
    ),
    column(4,
           selectInput("graph",
                       label = "Plotting",
                       choices = c("")
           ),
           # htmlOutput("inc")
           plotOutput(outputId = "Plot")
           
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
    x = 0
    retail = NULL
    retial.out = NULL
    transactionData = NULL
    tr = NULL
    rules = NULL
    rules.sub = NULL
    
    observeEvent(input$do, {
      
      if (x==1) {
        
        supp <- as.numeric(input$supp)
        conf <- as.numeric(input$conf)
        
        # get association rules
        rules.sub <- switch (input$choice,
                             "All" = {subset(rules, support>=supp & confidence>=conf)},
                             {subset(rules, rhs %in% input$choice & support>=supp & confidence>=conf)}
        )
        
        # get total rules from rules.sub
        len <- length(rules.sub)
        
        # print total transactions
        output$Transactions <- renderText({
          paste("Number of transactions: ", length(tr))
        })
        
        # print number of rules of specified items
        output$AR <- renderText({
          paste("Number of association rules: ", len)
        })
        
        # print the association rules
        output$rules <- renderPrint({
          inspect(rules.sub[0:len])
        })
      }
    })
  
    output$Plot <- renderPlot({
      switch(input$graph,
              "Purchase hour" = {retail %>% ggplot2::ggplot(ggplot2::aes(x=Time)) + ggplot2::geom_histogram(stat="count",fill="indianred")},
              "Absolute" = {arules::itemFrequencyPlot(tr,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")},
              "Relative" = {arules::itemFrequencyPlot(tr,type="relative",col=brewer.pal(8,'Pastel2'), main="Relative Item Frequency Plot")},
              "Network graph" = {plot(rules, method="graph", engine = "htmlwidget")},
              "Parallel coordinates" = {plot(rules, method="paracoord")}
              )
      
    })
    
    
    
    output$contents <- renderDataTable({
      inFile <- input$fileName
      
      if (is.null(inFile)) {
        x <<- 0
        return(NULL)
      } else if (tolower(tools::file_ext(inFile$datapath)) == "xlsx"){
        x <<- 1
        
        retail <<- readxl::read_excel(inFile$datapath)
        retail.out <<- retail
        retail <<- retail[complete.cases(retail),]
        
        
        retail$Date <<- as.Date(retail$InvoiceDate)
        retail$Time <<- format(retail$InvoiceDate,"%H:%M:%S")
        retail$Time <<- as.factor(retail$Time)
        library(plyr)
        transactionData <<- ddply(retail,c("InvoiceNo"), function(df1)paste(df1$Description, collapse = ","))
        transactionData$InvoiceNo <<- NULL
        colnames(transactionData) <<- c("Items")
        write.table(transactionData,paste(inFile$datapath, "sample.csv"), quote = FALSE, row.names = FALSE, col.names = FALSE, sep=',')
        tr <<- read.transactions(paste(inFile$datapath, "sample.csv"), format = 'basket', quote = "", sep = ',')
        
        rules <<- apriori(tr, parameter = list(minlen=2, maxlen=10))
        rules <<- sort(rules, by='confidence', decreasing = TRUE)
        rules.sub <<- subset(rules, support>=0 & confidence>=0)
        itemList <<- rules@rhs@itemInfo[["labels"]]
        
        updateSelectInput(session,
                          "choice",
                          choices = c("All", itemList)
        )
        
        updateSelectInput(session,
                          "graph",
                          choices = c("","Purchase hour", "Absolute", "Relative", "Parallel coordinates")
        )
        retail.out
      }
      else {
        x <<- 0
        
        updateSelectInput(session,
                          "choice",
                          choices = c("")
        )
        
        updateSelectInput(session,
                          "graph",
                          choices = c("")
        )
      }
      
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)
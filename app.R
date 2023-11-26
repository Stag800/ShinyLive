library(shiny)
library(shinydashboard)
library(readxl)
library(networkD3)
library(PerformanceAnalytics)
library(igraph)
library(DT)
library(corrplot)


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Correlation Matrix and Force Network Diagram Generator",titleWidth = "600px"),
  
  dashboardSidebar(
    disable = TRUE
  ),
  
  dashboardBody(
    fluidRow(
      box(
        width = 3,
          title = "Select File",
        status = "primary",
        solidHeader = TRUE,
        fileInput("file", "Choose a file", accept = c(".xlsx", ".csv")),
        sliderInput("correlation_threshold", "Minimum Correlation", value = 0, min = 0, max = 1, step = 0.01)
      ),
      box(
        width = 9,
        title = "Force-Network",
        status = "primary",
        solidHeader = TRUE,
        forceNetworkOutput("force_network_plot")
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Correlation Matrix",
        status = "info",
        solidHeader = TRUE,
        DTOutput("correlation_matrix")
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Correlation Plot",
        status = "success",
        solidHeader = TRUE,
        plotOutput("diag_corr_graph")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    if (!(ext %in% c("xlsx", "csv"))) {
      return(NULL)  
    }
    
    if (ext == "xlsx") {
      data <- readxl::read_excel(input$file$datapath)
    } else {
      data <- read.csv(input$file$datapath)
    }
    
    if (!all(sapply(data, is.numeric))) {
      return(NULL)  
    }
    
    data <- na.omit(data, cols = rownames(data)[apply(data, 1, function(row) sum(is.na(row))/length(row) > 0.1)])
    data <- na.omit(data, rows = colnames(data)[apply(data, 2, function(col) sum(is.na(col))/length(col) > 0.1)])
    
    return(data)
  })
  
  output$correlation_matrix <- renderDT({
    req(data())
    
    correlation_matrix <- cor(data(), use = "complete.obs")
    formatted_matrix <- formatC(correlation_matrix, format = "f", digits = 2)
    
    datatable(formatted_matrix)
  })
  
  filtered_edges <- reactive({
    req(data())
    correlation_matrix <- cor(data(), use = "complete.obs")
    correlation_edges <- as.data.frame(as.table(correlation_matrix))
    colnames(correlation_edges) <- c("source", "target", "correlation")
    filtered_edges <- correlation_edges[abs(correlation_edges$correlation) > input$correlation_threshold, ]
    
    return(filtered_edges)
  })
  
  graph_data <- reactive({
    req(filtered_edges())
    graph <- graph_from_data_frame(filtered_edges(), directed = FALSE)
    return(graph)
  })
  
  output$force_network_plot <- renderForceNetwork({
    req(graph_data())
    
    networkD3_data <- igraph_to_networkD3(graph_data())
    networkD3_data$nodes$Group <- 1
    
    if (nrow(networkD3_data$nodes) == 0 || nrow(networkD3_data$links) == 0) {
      return(NULL)
    }
    
    forceNetwork(
      Links = networkD3_data$links,
      Nodes = networkD3_data$nodes,
      Source = "source",
      Target = "target",
      NodeID = "name",
      Group = "Group",
      fontSize = 12,
      opacity = 0.8,
      zoom = TRUE,
      opacityNoHover = 0.5,
      linkWidth = JS("function(d) { return d.weight * 1.5; }")
    )
  })
  
  output$diag_corr_graph <- renderPlot({
    req(data())
    
    cor_matrix <- cor(data())
    my_colors <- colorRampPalette(c("red","yellow" ,"black", "yellow","#013220"))(100)
    
    corrplot(cor_matrix, method = "number", type = "upper", diag = FALSE, col = my_colors, addCoef.col = "black", order = "original")
  })
}

shinyApp(ui, server)

library(shiny)
library(ggplot2)
library(FactoMineR)
library(factoextra)

data(iris)

ui <- fluidPage(
  headerPanel("110971018"), 
  sidebarLayout(
    sidebarPanel(
      sliderInput("num", "樣本數:", min = 6, max = nrow(iris), value = 120),
      selectInput("xaxis", "x軸: ", choices = c("PC1", "PC2", "PC3", "PC4"), selected = "PC1"), 
      selectInput("yaxis", "y軸: ", choices = c("PC1", "PC2", "PC3", "PC4"), selected = "PC2")
    ), 
    mainPanel(
      tabsetPanel(
        tabPanel("PCA", plotOutput("pcaPlot")), 
        tabPanel("PCA Summary", verbatimTextOutput("pcaSummary")),
        tabPanel("CA", plotOutput("caPlot")),
        tabPanel("CA Summary", verbatimTextOutput("caSummary"))
      )
    )
  )
)

server <- function(input, output){
  
  sampled_data <- reactive({
    row_index <- sample(nrow(iris), input$num)
    sampled_data <- iris[row_index, ]
    return(sampled_data)
  })
  
  pca <- reactive({
    sampled_data <- sampled_data()
    pca <- PCA(sampled_data[, 1:4], scale.unit = TRUE, graph = FALSE)
    return(pca)
  })
  
  ca <- reactive({
    sampled_data <- sampled_data()
    ca <- CA(sampled_data[, 1:4], graph = FALSE)
    return(ca)
  })
  
  selected_xaxis <- reactive({
    selected_xaxis <- switch(input$xaxis, "PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4)
    return(selected_xaxis)
  })

  selected_yaxis <- reactive({
    selected_yaxis <- switch(input$yaxis, "PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4)
    return(selected_yaxis)
  })
  
  
  output$pcaPlot <- renderPlot(
    {
      sampled_data <- sampled_data()
      biplot <- fviz_pca_biplot(pca(), axes = c(selected_xaxis(), selected_yaxis()), lable = "var", habillage = sampled_data[, 5], addEllipses = TRUE, ellipse.level = 0.95)
      print(biplot)
    }
  )
  
  output$caPlot <- renderPlot(
    {
      sampled_data <- sampled_data()
      caPlot <- fviz_ca_biplot(ca(), axes = c(selected_xaxis(), selected_yaxis()), col.row = sampled_data[, 5])
      print(caPlot)
    }
  )
  
  output$pcaSummary <- renderPrint({
    summary(pca())
  })
  
  output$caSummary <- renderPrint({
    summary(ca())
  })
  
}

shinyApp(ui = ui, server = server)



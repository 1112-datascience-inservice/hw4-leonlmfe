library(shiny)
library(ggplot2)
library(FactoMineR)
library(factoextra)

data(iris)

ui <- fluidPage(
  headerPanel("NCCU_DS2023_hw4_110971018"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num", "樣本數:", min = 6, max = nrow(iris), value = 50),
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
  
  output$pcaPlot <- renderPlot(
    {
      row_index <- sample(nrow(iris), input$num)
      sampled_data <- iris[row_index, ]
      pca <- PCA(sampled_data[, 1:4], scale.unit = TRUE, graph = FALSE)
      selected_xaxis <- switch(input$xaxis, "PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4)
      selected_yaxis <- switch(input$yaxis, "PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4)
      biplot <- fviz_pca_biplot(pca, axes = c(selected_xaxis, selected_yaxis), lable = "var", habillage = sampled_data[, 5], addEllipses = TRUE, ellipse.level = 0.95)
      print(biplot)
    }
  )
  
  output$caPlot <- renderPlot(
    {
      row_index <- sample(nrow(iris), input$num)
      sampled_data <- iris[row_index, ]
      ca <- CA(sampled_data[, 1:4], graph = FALSE)
      selected_xaxis <- switch(input$xaxis, "PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4)
      selected_yaxis <- switch(input$yaxis, "PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4)
      caPlot <- fviz_ca_biplot(ca, axes = c(selected_xaxis, selected_yaxis), col.row = sampled_data[, 5])
      print(caPlot)
    }
  )
  
  output$pcaSummary <- renderPrint({
    row_index <- sample(nrow(iris), input$num)
    sampled_data <- iris[row_index, ]
    pca <- PCA(sampled_data[, 1:4], scale.unit = TRUE, graph = FALSE)
    summary(pca)
  })
  
  output$caSummary <- renderPrint({
    row_index <- sample(nrow(iris), input$num)
    sampled_data <- iris[row_index, ]
    ca <- CA(sampled_data[, 1:4], graph = FALSE)
    summary(ca)
  })
  
}

shinyApp(ui = ui, server = server)



library(shiny)
library(ggplot2)
load("../data/analysis-script.RData")
aux_data = read.csv("../data/aux-data.csv")
subset_dat = read.csv("../data/subset-data.csv")
ridge_black = read.csv("../data/ranked-ridge-black.csv")
ridge_hisp = read.csv("../data/ranked-ridge-hispanic.csv")
load("../data/states-abbreb.RData")

# Test

ui <- fluidPage(
  titlePanel('Finding Target Schools'),
  
  fluidRow(
    column(4, 
           selectInput("demo",
                       "At Risk Demographic",
                       choices = c("African American", "Hispanic"))
    ),
    column(4, 
           selectInput("state",
                       "State",
                       state.abb, 
                       selected = "CA")
    ),
    column(4,
           selectInput("status",
                       "School Status",
                       c("Highly Selective", "Inclusive", "Selective"))
    )
  ),
  
  fluidRow(
    DT::dataTableOutput("table")
  ), 
  plotOutput('plot')
  
  
)


server <- function(input, output) {
  
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    # Minority Status
    if (input$status == "Highly Selective") stat = highly_selective
    if (input$status == "Inclusive") stat = inclusive
    if (input$status == "Selective") stat = selective
    
    # State Selection
    st = eval(as.name(input$state))
    
    if (input$demo == "African American") {
      gr = ridge_black
      data <- by_state(gr, st, stat)[,-3]
    }
    if (input$demo == "Hispanic") {
      gr = ridge_hisp
      data <- by_state(gr, st, stat)[,-2]
    } 
    
    data
  }))
  
  output$plot <- renderPlot({
    # Minority Status
    if (input$status == "Highly Selective") stat = highly_selective
    if (input$status == "Inclusive") stat = inclusive
    if (input$status == "Selective") stat = selective
    
    # State Selection
    st = eval(as.name(input$state))
    
    if (input$demo == "Hispanic") {
      gr = ridge_hisp
      data <- by_state(gr, st, stat)[,-2]
      print(data)
      
      dat1 = data[,c(1,2)]
      dat1$factor = rep(as.factor("Hispanic"), length(dat1$Names))
      dat1$Names = factor(dat1$Names, levels = dat1$Names)
      colnames(dat1) = c("Names", "Graduation Rate", "factor")
      
      dat2 = data[,c(1,3)]
      dat2$factor = rep(as.factor("Total"), length(dat2$Names))
      dat2$Names = factor(dat2$Names, levels = dat2$Names)
      colnames(dat2) = c("Names", "Graduation Rate", "factor")
      
      ggdat = data.frame(rbind(dat1, dat2))
      ggdat$Graduation.Rate = as.numeric(ggdat$Graduation.Rate)
    } 
    
    
    if (input$demo == "African American") {
      gr = ridge_black
      data <- by_state(gr, st, stat)[,-3]
      
      dat1 = data[,c(1,2)]
      dat1$factor = rep(as.factor("African American"), length(dat1$Names))
      dat1$Names = factor(dat1$Names, levels = dat1$Names)
      colnames(dat1) = c("Names", "Graduation Rate", "factor")
    
      dat2 = data[,c(1,3)]
      dat2$factor = rep(as.factor("Total"), length(dat2$Names))
      dat2$Names = factor(dat2$Names, levels = dat2$Names)
      colnames(dat2) = c("Names", "Graduation Rate", "factor")
    
      ggdat = data.frame(rbind(dat1, dat2))
      ggdat$Graduation.Rate = as.numeric(ggdat$Graduation.Rate)
    }
    
    ggplot(data = ggdat, aes(x = Names)) + 
      geom_bar(aes(y = Graduation.Rate, fill = factor), stat = "identity", position = "dodge", 
               ) + 
      theme(axis.text.x = element_text(angle = 25, hjust = 1)) + 
      ggtitle("Differnce in Graduation Rates")
    
  })
  
}


shinyApp(ui = ui, server = server)


library(shiny)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if (!require(htmltools)) install.packages("htmltools")
library(htmltools)
if (!require(ellipse)) install.packages("ellipse")
library(ellipse)
if (!require(shinyWidgets)) install.packages("shinyWidgets")
library(shinyWidgets)

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

ui <- fluidPage(
  # App title ----
  titlePanel("Comparing Clustering Techniques: Iris Data"),
 
   # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      prettyRadioButtons("disp", "Classification:",
                   choices = c("Original",
                               "Gaussian Mixture Model",
                               "K-means clustering"),
                   animation = "pulse"),
      br(),
      prettyRadioButtons("x", "X-axis",
                   choices = c("Sepal Length" = "1",
                               "Sepal Width" = "2",
                               "Petal Length" = "3",
                               "Petal Width" = "4"), 
                   animation = "pulse", 
                   selected = "2"),
      br(),
      prettyRadioButtons("y", "Y-axis",
                   choices = c("Sepal Length" = "1",
                               "Sepal Width" = "2",
                               "Petal Length" = "3",
                               "Petal Width" = "4"), 
                   animation = "pulse", 
                   selected = "4"),
      br(),
      #fluidRow(
      #  column(3, 
      #         br(),
          # radioButtons('disp', "Classification", choices=c("original","Gaussian mixture","K-means clustering"), selected="original"),
          # fluidRow(
          #   column(6, 
          #          radioButtons('x', "x-axis", 
          #                       choices=c("Sepal length"="1","Sepal width"="2","Petal length"="3","Petal width"="4"), selected="2")),
          #   column(6, 
          #          radioButtons('y', "y-axis", 
          #                       choices=c("Sepal length"="1","Sepal width"="2","Petal length"="3","Petal width"="4"), selected="4"))
          # ),
      #prettyRadioButtons(inputId = "ellipse",
      #                   label = "Ellipses", icon = icon("check"),
      #                   animation = "pulse"),
      checkboxInput('ellipse', label = "Ellipses", value=TRUE),
      sliderInput("alpha", "Level (Ellipses)", value = 0.8, min = 0.25, max = 0.95, step=0.025)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Visual Output", plotOutput("plot1", height="500px")),
                  tabPanel("Numeric Output", tableOutput("table"))
      )
      
    )
    #column(9, 
    #       plotOutput("plot1",  height="500px"),
    #       tableOutput("result")
    #)
  )
)

server <- shinyServer(function(input, output) {
  iris <- datasets::iris
  load("mmiris.RData")
  class.em <- as.factor(r.em$class)
  class.km <- as.factor(r.km$class)
  iris[,6] <- class.em
  iris[,7] <- class.km
  
  r <- reactive({
    alpha <- input$alpha
    j1 <- as.numeric(input$x)
    j2 <- as.numeric(input$y)
    disp <- input$disp
    
    df_ell <- data.frame()
    if (disp=="original") {
      d <- iris[,c(j1,j2,5)]
    } else if (disp=="Gaussian Mixture Model") {
      class.em <- as.factor(r.em$class)
      d <- cbind(iris[,c(j1,j2)],class.em)
      Species <- levels(class.em)
      for(g in (1:3)){
        #        M=r.em$sigma[[g]][c(j1,j2),c(j1,j2)]
        M=r.em$sigma[c(j1,j2),c(j1,j2)]
        c=r.em$mu[[g]][c(j1,j2)]
        df_ell <- rbind(df_ell, cbind(as.data.frame(ellipse(M,centre=c, level=alpha)), 
                                      group=Species[g]))
      }
    } else {
      class.km <- as.factor(r.km$cluster)
      d <- cbind(iris[,c(j1,j2)],class.km)
      Species <- levels(class.km)
      r.km$sigma <- sqrt(r.km$withinss/r.km$size/3)
      for(g in (1:3)){
        M=rep(r.km$sigma[g],2)
        c=r.km$center[g,c(j1,j2)]
        df_ell <- rbind(df_ell, cbind(as.data.frame(ellipse(0,scale=M,centre=c, level=alpha)), group=Species[g]))
      }
    }
    r <- list(df_ell=df_ell, d=d)
    return(r)
  })
  
  output$plot1 <- renderPlot({
    df_ell <- r()$df_ell
    d <- r()$d
    # col <- c("#FF1BB3","#A7FF5B","#99554D")
    col <- c("#7DB0DD","#86B875","#E495A5")
    
    pl <- ggplot(data=d, aes_string(x=d[,1],y=d[,2], colour=d[,3])) + geom_point(size=2) +
      xlab(names(d)[1]) + ylab(names(d)[2]) + theme_bw() #+ scale_colour_manual(values=col)
    
    if (input$disp=="Original") {
      pl <- pl  + scale_colour_manual(values=col, name="Species") + theme(legend.position="bottom")
    } else {
      pl <- pl + theme(legend.position="bottom") + scale_colour_manual(values=col, name="Group")
    }
    if (input$ellipse==TRUE) {
      if (input$disp=="Original") {
        pl <- pl + stat_ellipse(level=input$alpha, size=1) 
      } else {
        pl <- pl + geom_path(data=df_ell, aes(x=x, y=y,color=group), size=1)
      }
    }
    return(pl)
  })

  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    r()
  })
  
})

shinyApp(ui, server)

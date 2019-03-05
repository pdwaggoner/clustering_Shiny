alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}
shinyUI(fluidPage(
  fluidRow(
    column(3, 
           br(),
           radioButtons('disp', "Classification", choices=c("original","Gaussian mixture","K-means clustering"), selected="original"),
           fluidRow(
             column(6, 
                    radioButtons('x', "x-axis", 
                                 choices=c("Sepal length"="1","Sepal width"="2","Petal length"="3","Petal width"="4"), selected="2")),
             column(6, 
                    radioButtons('y', "y-axis", 
                                 choices=c("Sepal length"="1","Sepal width"="2","Petal length"="3","Petal width"="4"), selected="4"))
           ),
           checkboxInput('ellipse', label = "ellipses", value=TRUE),
           sliderInput("alpha", "level (ellipses)", value = 0.8, min = 0.25, max = 0.95, step=0.025),
           br()
    ),
    column(9, 
           plotOutput("plot1",  height="500px"),
           tableOutput("result")
    )
  )
))


library(shiny)
library(ggplot2)
library(htmltools)
library(ellipse)
library(shinyWidgets)

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

ui <- shinyUI(fluidPage(
  titlePanel("Comparing Clustering Techniques: Simulated Data"),
  fluidRow(
    column(2, 
           br(),
           sliderInput("mu2", "mu2", value = 2, min = 0, max = 5, step=0.2),
           sliderInput("sigma2", "sigma2", value = 0.6, min = 0.2, max = 5, step=0.2),
           sliderInput("p2", "p2", value = 0.4, min = 0.0, max = 1, step=0.05),
           br(),
           selectInput("n", label = "Sample Size", 
                       choices = c(50,100,250,500,1000,2000), 
                       selected = "250"),
           br(),
           actionButton("new", "New")
    ),
    column(7, 
           plotOutput("Visual",  height="400px"),
           tableOutput("Numeric")
    ),
    column(3, 
           br(),
           br(),
           prettyCheckbox('histo', label = "Histogram", outline = TRUE,
                          animation = "pulse",
                          shape = "round",
                          status = "primary"),
           prettyRadioButtons('select', "Distribution", 
                        choices = c("Original", "Gaussian Mixture Model", "K-means Clustering"),
                        selected="Original",
                        animation = "pulse"),
           checkboxGroupButtons(inputId = 'display', label = "Display",
                                direction = "vertical", 
                                # individual = TRUE,
                                 justified = FALSE,
                                 status = "primary",
                                 size = "sm",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
             choices = c("Components (pdf)", "Mixture (pdf)", "Data (rug)"),
             selected = c("Components (pdf)", "Mixture (pdf)", "Data (rug)"))
    )
  )
))

mixt2.em <- function(x, lambda, mu, sigma, K=200)
{
  p  <- lambda 
  for (k in 1:K) {
    # E step
    d1<-p[1]*dnorm(x,mu[1],sigma[1])
    d2<-p[2]*dnorm(x,mu[2],sigma[2])
    tau1 <-d1/(d1+d2)
    tau2 <- 1-tau1
    # M step
    p[1] <- mean(tau1)
    mu[1] <- sum(tau1*x)/sum(tau1)
    sigma[1] <-sqrt(sum(tau1*(x^2))/sum(tau1)-(mu[1])^2)
    p[2] <- 1-p[1]
    mu[2] <- sum((tau2)*x)/sum((tau2))
    sigma[2] <-sqrt(sum(tau2*(x^2))/sum(tau2)-(mu[2])^2)
  }
  d1<-p[1]*dnorm(x,mu[1],sigma[1])
  d2<-p[2]*dnorm(x,mu[2],sigma[2])
  tau1 <-d1/(d1+d2)
  tau2 <- 1-tau1
  
  return(list(lambda=p, mu=mu, sigma=sigma, posterior=cbind(tau1,tau2)))
}

server <- shinyServer(function(input, output) {
  set.seed(123456)
  z1 <- -0.015
  z2 <- -0.01
  mu1 <- 0
  sigma1 <- 1
  x1 <- -3
  nx <- 200
  r <- reactive({
    new <- input$new
    mu2 <- input$mu2
    sigma2 <- input$sigma2
    p2 <- input$p2
    n <- as.numeric(input$n)
    p1 <- 1-p2
    x2 <- max(3, qnorm(pnorm(3),mu2,sigma2))
    x <- seq(x1,x2,length.out = nx)
    f1 <- p1*dnorm(x,mu1,sigma1)
    f2 <- p2*dnorm(x,mu2,sigma2)
    f <- f1 + f2
    pdf1 <- data.frame(x,f)
    pdf2 <- data.frame(x=c(x,x),ori=c(f1, f2),group=as.factor(c(rep(1,nx),rep(2,nx))))
    n1 <- rbinom(1,n,p1)
    n2 <- n-n1
    y <- c(rnorm(n1,mu1,sigma1),rnorm(n2,mu2,sigma2))
    d <- data.frame(group=as.factor(c(rep(1,n1),rep(2,n2))),y=y)
    d$z <- c(rep(z1,n1),rep(z2,n2))
    
    # r.em <- normalmixEM(y, lambda=c(p1,p2), mu=c(mu1,mu2), sigma=c(sigma1,sigma2), fast=T)
    r.em <- mixt2.em(y, lambda=c(p1,p2), mu=c(mu1,mu2), sigma=c(sigma1,sigma2))
    p.em <- r.em$lambda
    mu.em <- r.em$mu
    sigma.em <- r.em$sigma
    f1.em <- p.em[1]*dnorm(x,mu.em[1],sigma.em[1])
    f2.em <- p.em[2]*dnorm(x,mu.em[2],sigma.em[2])
    df.em <- f1.em - f2.em
    i.em <- which(df.em[1:(nx-1)]*df.em[2:(nx)] <0 )
    if (length(i.em)>0) {
      f12.em <- (f1.em[i.em] + f1.em[i.em+1])/2
      x12.em <- (x[i.em] + x[i.em+1])/2
      em12 <- data.frame(x=x12.em, f=f12.em, f0=z2)
    } else {
      em12 <- NULL
    }
    pdf1$f.em <- f1.em + f2.em
    pdf2$em <-c(f1.em,f2.em)
    d$g.em <- as.factor((r.em$posterior[,1]<0.5)+1)
    d$z.em <- rep(z2,n)
    
    r.km <- kmeans(y, centers=c(0,1))
    p.km <- r.km$size/sum(r.km$size)
    mu.km <- as.vector(r.km$centers)
    sigma.km <- sqrt(r.km$withinss/r.km$size)
    f1.km <- p.km[1]*dnorm(x,mu.km[1],sigma.km[1])
    f2.km <- p.km[2]*dnorm(x,mu.km[2],sigma.km[2])
    df.km <- f1.km - f2.km
    i.km <- which(df.km[1:(nx-1)]*df.km[2:(nx)] <0 )
    if (length(i.km)>0) {
      f12.km <- (f1.km[i.km] + f1.km[i.km+1])/2
      x12.km <- (x[i.km] + x[i.km+1])/2
      km12 <- data.frame(x=x12.km, f=f12.km, f0=z2)
    } else {
      km12 <- NULL
    }
    pdf1$f.km <- f1.km + f2.km
    pdf2$km <- c(f1.km,f2.km)
    d$g.km <- as.factor(r.km$cluster)
    d$z.km <- rep(z2,n)
    
    res <- data.frame(p1=c(p1,p.em[1],p.km[1]),
                      p2=c(p2,p.em[2],p.km[2]),
                      mu1=c(mu1,mu.em[1],mu.km[1]),
                      mu2=c(mu2,mu.em[2],mu.km[2]),
                      sigma1=c(sigma1,sigma.em[1],sigma.km[1]),
                      sigma2=c(sigma2,sigma.em[2],sigma.km[2]))
    res$tvd <- c(0, sum(abs(pdf1$f - pdf1$f.em)),sum(abs(pdf1$f - pdf1$f.km)))*(x[2]-x[1])
    res <- round(res,3)
    res$missclass = c(0, paste0(mean(d$g.em!=d$group)*100,"%"), paste0(mean(d$g.km!=d$group)*100,"%")) 
    row.names(res) <- c("Original", "Gaussian Mixture Model", "K-means Clustering")
    
    r <- list(d=d, pdf1=pdf1, pdf2=pdf2, res=res, em12=em12, km12=km12)
    return(r)
  })
  
  output$Visual <- renderPlot({
    d <- r()$d
    pdf1 <- r()$pdf1
    pdf2 <- r()$pdf2
    em12 <- r()$em12
    km12 <- r()$km12
    
    sl <- 1
    
    pl <- ggplot() 
    
    if (input$histo == TRUE)
      pl <- pl + stat_bin(data=d,aes(x=y, y=..density..), 
                          bins=round(sqrt(length(d$y))), fill="white", color="black")
    if (input$select=="Original") {
      if ("Mixture (pdf)" %in% input$display)
        pl <- pl + geom_line(data=pdf1, aes(x, f), color="black", size=sl) 
      if ("Components (pdf)" %in% input$display)
        pl <- pl + geom_line(data=pdf2, aes(x, ori, color=group), size=sl)  
      if ("Data (rug)" %in% input$display)
        pl <- pl + geom_point(data=d, aes(x=y,y=z, colour=group))
    } else if (input$select=="Gaussian Mixture Model") {
      if ("Mixture (pdf)" %in% input$display)
        pl <- pl + geom_line(data=pdf1, aes(x, f.em), color="black",  size=sl) 
      if ("Components (pdf)" %in% input$display)
        pl <- pl + geom_line(data=pdf2, aes(x, em, color=group),  size=sl)  
      if ("Data (rug)" %in% input$display) {
        pl <- pl + geom_point(data=d, aes(x=y,y=z.em, colour=g.em)) 
        if (!is.null(em12)  && "Components (pdf)" %in% input$display) 
          pl <- pl + geom_segment(data=em12, aes(x=x, xend=x, y=f0, yend=f))
      }
    } else {
      if ("Mixture (pdf)" %in% input$display)
        pl <- pl + geom_line(data=pdf1, aes(x, f.km), color="black",  size=sl) 
      if ("Components (pdf)" %in% input$display)
        pl <- pl + geom_line(data=pdf2, aes(x, km, color=group),  size=sl)  
      if ("Data (rug)" %in% input$display) 
        pl <- pl + geom_point(data=d, aes(x=y,y=z.km, colour=g.km)) 
    }
    pl <- pl + theme_bw()
    pl <- pl + theme(legend.position=c(0.9, 0.9)) 
    print(pl)
    return(pl)
  })
  
  output$Numeric <- renderTable({
    r()$res
  }, rownames=TRUE)
  
})

shinyApp(ui, server)

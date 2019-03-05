library(ggplot2)
library(ellipse)
theme_set(theme_bw())


shinyServer(function(input, output) {
  iris <- datasets::iris
  load("mmiris.RData")
  print(str(iris)) # trash if necessary
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
    } else if (disp=="Gaussian mixture") {
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
      xlab(names(d)[1]) + ylab(names(d)[2]) #+ scale_colour_manual(values=col)
    
    if (input$disp=="original") {
      pl <- pl  + scale_colour_manual(values=col, name="Species") + theme(legend.position="bottom")
    } else {
            pl <- pl + theme(legend.position="bottom") + scale_colour_manual(values=col, name="Group")
    }
    if (input$ellipse==TRUE) {
      if (input$disp=="original") {
        pl <- pl + stat_ellipse(level=input$alpha, size=1) 
      } else {
        pl <- pl + geom_path(data=df_ell, aes(x=x, y=y,color=group), size=1)
      }
    }
    return(pl)
  })
  
  
})

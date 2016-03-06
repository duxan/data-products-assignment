shinyServer(function(input, output, session) {

  #load library for multivariate normal
  library(mvtnorm)

  #load Old Faithful data frame
  data(faithful)

  #setup grid for plotting
  xpts <- seq(from=1,to=6,length.out=100)
  ypts <- seq(from=40,to=100,length.out=100)

  iter <- reactive({
    input$iter
  })

  #initial parameter estimates (chosen to be deliberately bad)
  theta <- reactive({
    list(
      tau=c(0.5,0.5),
      mu1 = c(input$mu1x, input$mu1y), 
      mu2 = c(input$mu2x, input$mu2y), 
      sigma1=matrix(c(    #0.8,7,7,70),ncol=2),
                input$mu1x - input$std1x/2, 
                input$mu1x + input$std1x/2,
                input$mu1x + input$std1x/2, 
                input$mu1y - input$std1y/2
              ), ncol=2),
      sigma2=matrix(c(    #0.8,7,7,70),ncol=2),
                input$mu2x - input$std2x/2, 
                input$mu2x + input$std2x/2,
                input$mu2x + input$std2x/2, 
                input$mu2y - input$std2y/2
              ), ncol=2)
    )
  })

  output$plotEMinitial <- renderPlot({
    T <- E.step(theta())
    theta <- M.step(T)
    plot.em.initial(theta)
  })

  output$plotEMiter <- renderPlot({
    fix_theta <- theta()
    for (i in 1:iter()){
      T <- E.step(fix_theta)
      fix_theta <- M.step(T)
      plot.em.iter(fix_theta)
    }
  })

  #E step: calculates conditional probabilities for latent variables
  E.step <- function(theta){
    t(apply(cbind(
      theta$tau[1] * dmvnorm(faithful,mean=theta$mu1,sigma=theta$sigma1),
      theta$tau[2] * dmvnorm(faithful,mean=theta$mu2,sigma=theta$sigma2)
    ),1,function(x) x/sum(x)))
  }

  #M step: calculates the parameter estimates which maximise Q
  M.step <- function(T){
    list(
      tau= apply(T,2,mean),
      mu1= apply(faithful,2,weighted.mean,T[,1]),
      mu2= apply(faithful,2,weighted.mean,T[,2]),
      sigma1= cov.wt(faithful,T[,1])$cov,
      sigma2= cov.wt(faithful,T[,2])$cov
    )
  }

  #function to plot current data
  plot.em.initial <- function(theta){
    mixture.contour <- outer(xpts,ypts,function(x,y) {
      theta$tau[1]*dmvnorm(cbind(x,y),mean=theta$mu1,sigma=theta$sigma1) + theta$tau[2]*dmvnorm(cbind(x,y),mean=theta$mu2,sigma=theta$sigma2)
    })
    contour(xpts,ypts,mixture.contour,nlevels=5,drawlabel=FALSE,col="red",xlab="Eruption time (mins)",ylab="Waiting time (mins)",main=expression("Old Faithful data w/ initial params"))
    points(faithful)
  }
  
  #function to plot current data after iter() # of iterations
  plot.em.iter <- function(theta){
    mixture.contour <- outer(xpts,ypts,function(x,y) {
      theta$tau[1]*dmvnorm(cbind(x,y),mean=theta$mu1,sigma=theta$sigma1) + theta$tau[2]*dmvnorm(cbind(x,y),mean=theta$mu2,sigma=theta$sigma2)
    })
    contour(xpts,ypts,mixture.contour,nlevels=5,drawlabel=FALSE,col="red",xlab="Eruption time (mins)",ylab="Waiting time (mins)",main=bquote("Old Faithful data after" ~ .(iter()) ~ "iteration(s)"))
    points(faithful)
  }

})

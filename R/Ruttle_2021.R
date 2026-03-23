
downloadRuttle2021Files <- function() {
  
  if (!dir.exists('data/Ruttle_2021')) {
    dir.create('data/Ruttle_2021')
  }
  
  # Download the data files
  Reach::downloadOSFdata(
    repository='5sqk3',
    filelist=list('Ruttle_2021'=c('nocursor_nocursors.csv', 'nocursor_reaches.csv')),
    folder='data/Ruttle_2021',
    overwrite=FALSE
    
  )
  
}

processRuttle2021_NC <- function() {
  
  # Read in the data files
  nocursors <- read.csv('data/Ruttle_2021/nocursor_nocursors.csv')
  reaches <- read.csv('data/Ruttle_2021/nocursor_reaches.csv')
  
  # Get the confidence intervals and averages for the nocursor data
  nc_CI <- apply(nocursors[2:ncol(nocursors)], 1, function(i) {
    Reach::getConfidenceInterval(data=i, conf.level=0.95, method='b')
  })
  nc_avg <- rowMeans(nocursors[2:ncol(nocursors)])
  
  # Get the confidence intervals and averages for the reaches data
  r_CI <- apply(reaches[2:ncol(reaches)], 1, function(i) {
    Reach::getConfidenceInterval(data=i, conf.level=0.95, method='b')
  })
  r_avg <- rowMeans(reaches[2:ncol(reaches)])
  
  # get perturbation schedule
  distortion <- nocursors[,'distortion']
  changes <- which(diff(distortion) != 0)
  x <- c(1)
  y <- distortion[1]
  for (i in changes) {
    x <- c(x, i+0.5, i+0.5)
    y <- c(y, distortion[i], distortion[i+1])
  }
  clamped <- which(is.na(distortion))
  x <- c(x, rep(min(clamped-0.5),2), max(clamped))
  y <- c(y, y[length(y)], 0, 0)
  
  return(list(
    nc_CI=nc_CI,
    nc_avg=nc_avg,
    r_CI=r_CI,
    r_avg=r_avg,
    x=x,
    y=y
  ))
}

plotRuttle2021_NC <- function() {
  
  data <- processRuttle2021_NC()
  clrs <- getColors()
  
  for (target in c('pdf','svg')) {
    Reach::setupFigureFile(target=target, width=8, height=6, dpi=300, filename='figures/Ruttle_2021_NC')
    
    plot(NULL,NULL,
         main='',xlab='trial',ylab='reach angle [°]',
         xlim=c(0, max(data$x)+1), ylim=c(-30, 30),
         bty='n', axis=F,
         cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
    
    lines(data$x, data$y, col='grey', lwd=2)
    # polygon(x=c())
    # 
    # for (i in 1:length(data$r_avg)) {
    #   lines(c(i,i), data$r_CI[,i], col=clrs$op[], lwd=2)
    # }
    
    axis(side = 1, at=unique(data$x))
    axis(side = 2, at=seq(-30,30,15))
    
    dev.off()
  }
}
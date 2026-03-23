
# graphic elements -----

getColors <- function() {
  
  mycolors <- list()
  mycolors[['op']] <- c('orange', '#e51636', 'purple3', 'blue3', 'darkblue')
  mycolors[['tr']] <- unlist(lapply(mycolors[['op']], Reach::colorAlpha, alpha=22))
    
  return(mycolors)
  
}
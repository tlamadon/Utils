
means <- function(x) {
  sfit = summary(lm(x~1))
  return(list(mean=sfit$coef[1],sem=sfit$coef[2],sd=sd(x)))
}

list2df <- function(ll) {
 return(ldply(ll,function(l){ return(data.frame(rbind(unlist(l))))}))
}

fitMirror <- function (x,LB=NA,UB=NA) {
  test1 = TRUE
  test2 = TRUE
  while( test1 | test2) {
    if ( !is.na(UB) & (x>UB)) {
      x = 2*UB - x 
    } else {
      test1 = FALSE
      } 
    if ( !is.na(LB) & (x<LB)) {
      x = 2*LB - x
    } else {
      test2 = FALSE
    } 
   }
 return(x)
}

multiplot <- function(..., plotlist=NULL, cols) {
    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # Make the panel
    plotCols = cols                       # Number of columns of plots
    plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols

    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
    vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
        curRow = ceiling(i/plotCols)
        curCol = (i-1) %% plotCols + 1
        print(plots[[i]], vp = vplayout(curRow, curCol ))
    }

}


# function that compiles and links
# a Rcpp module
ModuleFromFile <- function(name) {
  text <- readLines(paste(name,'.cpp',sep=''),encoding="UTF-8")
  fx   <- cxxfunction( , "", includes =text , plugin = "RcppGSL" )
  foo <- Module(name, getDynLib(fx) )
  return(foo)
}


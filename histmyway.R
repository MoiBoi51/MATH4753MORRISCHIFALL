# learn R my friends!!
myhist <- function(x){
  graphics.off() # kill graphics
  n <- length(x) # sample size
  ifelse(n < 25, bins <- 5:6, 
         ifelse(n >=25 & n <= 50, bins <- 7:10, 
                bins <- 11:15))
  
  nplots <- length(bins) # a separate plot for each bin number
  mach <- Sys.info()
  if(mach[1] == "Windows"){windows()}else(quartz())
  layout(matrix(1:nplots)) # cut the surface according to number of plots
  
  mybarplot <- function(i){# function to use in the for loop
    xc <- cut(x,bins[i])
    xct <- table(xc) # make a table
    r <- xct/max(xct) # 0<= r <= 1 -- used for rgb
    barplot(xct,
            col = rgb(r,r^2,0.5), 
            space = 0,# continuous
            xlab = "x",
            ylab = "frequency",
            main = paste0("Histogram of sample, ","number of bins = ", bins[i]),
            las = 2)}
  
  for(i in 1:nplots) mybarplot( i = i)
    
  invisible(list(bins = bins, x = x, nplots = nplots, n  =n)) # returns an invisible list
}

out <- myhist(x = rnorm(20,100,10))
out$x

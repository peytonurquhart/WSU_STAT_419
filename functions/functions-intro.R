
# isWholeNumber ... camelCase
# WhiteSmith indentor ..
# isClose ...
# zeroIsh ... 

require("ggplot2")

# returns True for close enough to integer, false for non-integer
isWholeNumber = function(x, tol = .Machine$double.eps^0.5)  
{
    return(abs(x - round(x)) < tol);
}

numHandshake = function(n)
{
  return(n*(n-1)/2);
}

handShake = function(n=1, plotMe=FALSE)
{
  if(n < 1) 
  { 
    stop("n must be greater than 0"); 
  }
  
  if(!isWholeNumber(n)) 
  { 
    stop("n must be an integer"); 
  }
  
  h = numHandshake(n);

  if(plotMe)
  {

    p = getGgUnitCirclePlot(100)
    
    # Add 'n' points evenly distributed on the unit circle
    hs_dat = circleData(center=c(1,1), r=1, npoints = n)
    p = p + geom_point(data=hs_dat)
    
    # can you draw a circle
    # can you loop through "n" points 
    # ... and connect "n-1" elements
    # for(i in 1:n) ... for(j in 1:(n-1))
    
    
    
    
    print(p)
  }
  
  return(h);
}

# Get a plot of a unit cirle totally in quadrant 1
getGgUnitCirclePlot = function(npoints=100)
{
  c_dat = circleData(center=c(1,1), r=1, npoints)
  p = ggplot(c_dat,aes(x,y)) + geom_path() 
  return(p)
}

# Get n points representing a circle
# reference: https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
circleData = function(center = c(0,0), r = 1, npoints = 100)
{
  tt = seq(0,2*pi,length.out = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# readBin
# readChar  ... one long string
# readLines ... vector of lines of strings 


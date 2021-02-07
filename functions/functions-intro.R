
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

# Get number of handshakes between n people
numHandshake = function(n)
{
  return(n*(n-1)/2);
}

# Plot handshakes between n people on a circle
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
  
  # Get number of handshakes for n people
  h = numHandshake(n);
  
  print(h);

  if(plotMe)
  {

    # Get ggplot for a unit circle. (100 points to draw)
    p = getGgUnitCirclePlot(100)
    
    # Add 'n' points evenly distributed on the unit circle
    hs_dat = circleData_NonEnclosed(center=c(1,1), r=1, npoints = n)
    
    # Add handshake data points to the circle
    p = p + geom_point(data=hs_dat)
    
    # Draw lines for each handshake (not optimized but looks simpler)
    for(i in 1:n)
    {
      for(j in 1:(n-1))
      {
          # create line data frame for two entries in the data (i,j)
          l = createLineForEntries(df=hs_dat, i, j);
          # update the plot
          p = p + geom_line(data=l)       
      }
    }
    
    # draw the plot
    print(p)
  }
}

# Given a data frame of x y pairs (col x) (col y) create a 
# Data frame for a line between two rows
createLineForEntries = function(df, e1 = 1, e2 = 2)
{
  l = createLineDataFrame(p1=c(df$x[e1], df$y[e1]), p2 = c(df$x[e2], df$y[e2])); 
  return(l)
}

# Given two points, create a data frame for a line between them
createLineDataFrame = function(p1 = c(0,0), p2 = c(0,0))
{
  x = c(p1[1], p2[1]);
  y = c(p1[2], p2[2]);
  return(data.frame(x, y))
}

# Get a ggplot of a unit circle in quadrant 1
getGgUnitCirclePlot = function(npoints=100)
{
  c_dat = circleData(center=c(1,1), r=1, npoints)
  p = ggplot(c_dat,aes(x,y)) + geom_path() 
  return(p)
}

# Get n points representing a closed circle, (2 of the points are the same for drawing easier)
# reference: https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
circleData = function(center = c(0,0), r = 1, npoints = 100)
{
  tt = seq(0,2*pi,length.out = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# Get n unique points representing a circle, not closed. (n unique points)
circleData_NonEnclosed = function(center = c(0,0), r = 1, npoints = 100)
{
  # add point, must remove duplicate later..
  npoints = npoints + 1;
  
  tt = seq(0,2*pi,length.out = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[2] + r * sin(tt)
  dat = data.frame(x = xx, y = yy)
  
  # remove duplicate from data set
  dat <- dat[-c(npoints), ]
}




# readBin
# readChar  ... one long string
# readLines ... vector of lines of strings 


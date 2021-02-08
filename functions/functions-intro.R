require("ggplot2")

# HANDSHAKE --------------------------------------------------------------------

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


# DECLARATION ------------------------------------------------------------------


# Plot histogram for a data frame with the columns being the x value.
plotHistogramForDeclrData = function(df, name)
{
  alpha <- c(letters[1:26]);
  
  # Restructure data for histogram
  dfd = xyDataframeFromColYDataframe(df);
  
  p <- ggplot(dfd, aes(x = idx, y = count)) + geom_bar(stat="identity", position = "dodge");
  p <- p + ylim(0, 1200);
  p <- p + ggtitle(name);
  p <- p + geom_text(aes(label=alpha), position=position_dodge(width=0.9), vjust=-2.0);
  p <- p + geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25);
  
  print(p);
}

# Convert letter-count table to histogram-able data set for declaration problem
xyDataframeFromColYDataframe = function(df1)
{
  alpha = "abcdefghijklmnopqrstuvwxyz";
  
  df <- as.data.frame(matrix(0, ncol = 2, nrow = 26));
  names(df)[1] <- "idx";
  names(df)[2] <- "count";
  
  for(i in 1:26)
  {
    ci = substr(alpha,i,i);
    df$idx[i] <- df$idx[i] + i;
    df$count[i] <- df$count[i] + df1[[ci]];
  }
  return(df);
}

# Count lower case alpha chars in a text file, keep track of $OTHER chars
# Return a dataframe
countLowerCaseChars_f = function(filepath)
{
  s = readChar(filepath, file.info(filepath)$size);
  df = countLowerCaseChars_s(tolower(s));
  
  return(df);
}

# Count lower case alpha chars in a string, keep track of $OTHER chars
# Return a dataframe
countLowerCaseChars_s = function(s)
{
  # Create a-z data frame, add column OTHER
  df <- createAZDataframe();
  df$OTHER <- 0;
  
  # Count each lower case alpha char, update data frame
  for(i in 1:nchar(s))
  {
    ci = substr(s,i,i);
    if(ci %in% colnames(df))
    {
      df[[ci]] <- df[[ci]] + 1;
      
    } else if (ci != ' ') {
      df$OTHER <- df$OTHER + 1;
    }
  }
  return(df);
}

# Creates data frame with columns a-z, and a single row.
createAZDataframe = function()
{
  alpha <- c(letters[1:26]);
  df <- as.data.frame(matrix(0, ncol = 26, nrow = 1));
  
  for(i in 1:26)
  {
    names(df)[i] <- alpha[i];
  }
  return(df);
}

# DET MATRIX -------------------------------------------------------------------

det_2x2 = function(mat)
{
  return((mat[1,1] * mat[2,2]) - (mat[1,2] * mat[2,1]));
}

det_3x3 = function(mat, print=FALSE)
{
  efhi = matrix(0, ncol = 2, nrow = 2);
  dfgi = matrix(0, ncol = 2, nrow = 2);
  degh = matrix(0, ncol = 2, nrow = 2);
  
  efhi[1,1] = mat[2,2];
  efhi[1,2] = mat[2,3];
  efhi[2,1] = mat[3,2];
  efhi[2,2] = mat[3,3];
  
  dfgi[1,1] = mat[2,1];
  dfgi[1,2] = mat[2,3];
  dfgi[2,1] = mat[3,1];
  dfgi[2,2] = mat[3,3];
  
  degh[1,1] = mat[2,1];
  degh[1,2] = mat[2,2];
  degh[2,1] = mat[3,1];
  degh[2,2] = mat[3,2];
  
  if(print==TRUE)
  {
    print(mat);
    print(efhi);
    print(dfgi);
    print(degh);
  }
  
  return((mat[1,1]*(det_2x2(efhi))) - (mat[1,2]*(det_2x2(dfgi))) + (mat[1,3]*(det_2x2(degh))));
}


















































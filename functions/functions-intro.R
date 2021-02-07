
# isWholeNumber ... camelCase
# WhiteSmith indentor ..
# isClose ...
# zeroIsh ... 

# returns True for close enough to integer, false for non-integer
isWholeNumber = function(x, tol = .Machine$double.eps^0.5)  
{
    return(abs(x - round(x)) < tol);
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
  
  h = n*(n-1)/2;

  if(plotMe)
    {
    # can you draw a circle
    # can you loop through "n" points 
    # ... and connect "n-1" elements
    # for(i in 1:n) ... for(j in 1:(n-1))
    plot(n,h);
  }
  
  return(h);
}


# readBin
# readChar  ... one long string
# readLines ... vector of lines of strings 


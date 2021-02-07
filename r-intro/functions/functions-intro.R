
# isWholeNumber ... camelCase
# WhiteSmith indentor ...
# isClose ...
# zeroIsh ... 

is.wholenumber = function(x, tol = .Machine$double.eps^0.5)  
    {
    # x = 1.3
    abs(x - round(x)) < tol;
    }

# Alt-F-S
handShake = function(n=1, plotMe=FALSE)
  {
  if(n < 1) { stop("n must be greater than 0"); }  # warning
  if(!is.wholenumber(n)) { stop("n must be an integer"); }
  h = n*(n-1)/2;
  # return( n*(n-1)/12 );
  if(plotMe)
    {
    # can you draw a circle
    # can you loop through "n" points 
    # ... and connect "n-1" elements
    # for(i in 1:n) ... for(j in 1:(n-1))
    plot(n,h);
    }
  h;
}


# readBin
# readChar  ... one long string
# readLines ... vector of lines of strings 


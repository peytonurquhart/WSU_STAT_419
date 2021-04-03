
# Return a df of movies which exist in both dataframes
findCommonMovies = function(mdf1, mdf2)
{
  n = 0;
  id1 = 0;
  for(i in 1:nrow(mdf1)) 
  {
    ttid1 <- mdf1[i,1];
    for(k in 1:nrow(mdf2)) 
    {
      ttid2 <- mdf2[k,1];
      
      if(ttid2 == ttid1)
      {
        if(n == 0)
        {
          id1 = k;
        }
        
        n = n + 1;
      }
    }
    
  }
  if (n == 0)
  {
    print("Zero common movies found");
    return(NULL);
  }
  ndf <- mdf2[c(id1),]
  if (n == 1)
  {
    print("1 common movie found");
    return(ndf);
  }
  for(i in 1:nrow(mdf1)) 
  {
    ttid1 <- mdf1[i,1];
    for(k in 1:nrow(mdf2)) 
    {
      ttid2 <- mdf2[k,1];
      
      if(ttid2 == ttid1 && k != id1)
      {
        ndf <- rbind(ndf, mdf2[c(k), ])
      }
    }
  }
  
  print("Multiple common movies found");
  return(ndf)
}
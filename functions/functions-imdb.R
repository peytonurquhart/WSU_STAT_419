
# HELPER -----------------------------------------------------------------------

# Filtering for dataframe
filterBy = function(df, col, lambda)
{
  first = TRUE;
  ndf = NULL;
  
  for(i in 1:nrow(df))
  {
    if(lambda(df[i,col]) == TRUE)
    {
      if (first)
      {
        ndf <- df[c(i),];
        first = FALSE;
      } else {
        ndf <- rbind(ndf, df[c(i), ]);
      }
    }
  }
  return(ndf);
}

# MOVIES -----------------------------------------------------------------------

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


buildGenresDf = function(df)
{
  gdf <- as.data.frame(matrix(0, ncol = 2, nrow = 14));
  names(gdf)[1] <- "genre";
  names(gdf)[2] <- "count";

  gdf[1,1] <- "Action"
  gdf[2,1] <- "Adventure"
  gdf[3,1] <- "Drama"
  gdf[4,1] <- "Fantasy"
  gdf[5,1] <- "Sci-Fi"
  gdf[6,1] <- "Biography"
  gdf[7,1] <- "Comedy"
  gdf[8,1] <- "Romance"
  gdf[9,1] <- "Crime"
  gdf[10,1] <- "Thriller"
  gdf[11,1] <- "Animation"
  gdf[12,1] <- "Sport"
  gdf[13,1] <- "Mystery"
  gdf[14,1] <- "Documentary"
   
  for(i in 1:nrow(df))
  {
    for(j in 1:14)
    {
      if (grepl(gdf[j,1], df[i,6], fixed = TRUE))
      {
        gdf[j,2] <- gdf[j,2] + 1;
      }
    }
  }
  
  return(gdf);
}

plotGenresDf = function(gdf, s_title)
{
  p <- ggplot(gdf, aes(x="", y=count, fill=genre));
  p <- p + geom_bar(stat="identity", width=1, color="Black");
  p <- p + coord_polar("y", start=0);
  p <- p + theme_void();
  p <- p + ggtitle(s_title);
  return(p)
}

buildMPAADf = function(df)
{
  gdf <- as.data.frame(matrix(0, ncol = 2, nrow = 7));
  names(gdf)[1] <- "mpaa";
  names(gdf)[2] <- "count";
  
  gdf[1,1] <- "G"
  gdf[2,1] <- "PG"
  gdf[3,1] <- "PG-13"
  gdf[4,1] <- "NC-17"
  gdf[5,1] <- "R"
  gdf[6,1] <- "TV-MA"
  gdf[7,1] <- "Not Rated"

  for(i in 1:nrow(df))
  {
    for(j in 1:7)
    {
      if (gdf[j,1] == df[i,7])
      {
        gdf[j,2] <- gdf[j,2] + 1;
      }
    }
  }
  
  return(gdf);
}

plotMPAADf = function(gdf, s_title)
{
  p <- ggplot(gdf, aes(x="", y=count, fill=mpaa));
  p <- p + geom_bar(stat="identity", width=1, color="Black");
  p <- p + coord_polar("y", start=0);
  p <- p + theme_void();
  p <- p + ggtitle(s_title);
  return(p)
}












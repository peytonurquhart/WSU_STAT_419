library("quantmod");
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

# Filter out all NA values for a column
filterOutMissingValues = function(df, col)
{
  return(filterBy(df, col, (function(x) {!(is.na(x))})))
}

# Add an empty column to a dataframe called s.newname filled with default.value
addEmptyCol = function(df, s.newname, default.value = NA)
{
  df <- cbind(df, new___col___ = default.value);
  names(df)[ncol(df)] <- s.newname;
  return(df);
}

# Gets CPI data from 1947 - 2021
getCPIData = function(s.baseyear="2021")
{
  getSymbols("CPIAUCSL", src='FRED'); #Consumer Price Index
  tail(CPIAUCSL);
  set.seed(1);
  p <- xts(rnorm(63, mean=10, sd=3), seq(from=as.Date('1950-12-01'), by='years', length.out=63));
  colnames(p) <- "price";
  avg.cpi <- apply.yearly(CPIAUCSL, mean);
  names(avg.cpi)[1] <- "CPI";
  cf <- avg.cpi/as.numeric(avg.cpi[s.baseyear]);
  cf <- addEmptyCol(cf, "year", 0);
  for(x in 1:nrow(cf))
  {
    cf[x,2] <- (x+1946);
  }
  return(cf);
}

# Apply inflation to a dollar amount from.year. Only accurate 1947-1921
applyInflationToUSD = function(amount, from.year, s.baseyear="2021", cpi_data=NULL)
{
  if(from.year < 1947)
  {
    from.year = 1947;
  }
  if(from.year > 2021)
  {
    from.year = 2021;
  }
  
  if(is.null(cpi_data))
  {
    cf = getCPIData(s.baseyear);
  } else {
    cf <- cpi_data;
  }
  
  for(x in 1:nrow(cf))
  {
    if(cf[x,2] == from.year)
    {
      i_a = (amount/(cf[x,1]));
      return(as.numeric(i_a));
    }
  }
  stop("Error in applyInflationToUSD");
}


# MOVIES -----------------------------------------------------------------------


adjustForInflation = function(mdf, s.baseyear="2021")
{
  cf = getCPIData(s.baseyear);
  for(x in 1:nrow(mdf))
  {
    if(!is.na(mdf[x,4]))
    {
      if(!is.na(mdf[x,12]))
      {
        mdf[x,12] <- applyInflationToUSD(as.numeric(mdf[x,12]), as.numeric(mdf[x,4]), s.baseyear, cf);
      }
    }
  }
  
  return(mdf);
}

# Takes the average for <col> by any given year in the range <year.min>:<year.max>
# EX:
# 2020: 3
# 2020: 5
# 2020: 1
# 2021: 7
# SUM? --->
# 2020: (3+5+1)/3 = 3
# 2021: 7/1 = 7
# : 9, 7

smoothDataOverTime = function(mdf, col, year.min = 1980, year.max = 2022, sum_=FALSE)
{ 
  hdf <- as.data.frame(matrix(0, ncol = 3, nrow = (year.max - year.min)));
  names(hdf)[1] <- "year";
  names(hdf)[2] <- "count.orig";
  names(hdf)[3] <- "data";
  
  for(x in year.min:year.max)
  {
    hdf[x - year.min + 1,1] <- x;
  }
  
  for(i in year.min:year.max)
  {
    idx = i - year.min + 1
    tdf = filterBy(filterOutMissingValues(mdf, 4), 4, (function(x){(x == i)}));
    if(!is.null(tdf))
    {
      tdf = filterOutMissingValues(tdf, col);
      if(!is.null(tdf))
      {
        sum = 0;
        for(x in 1:nrow(tdf))
        {
          sum <- sum + tdf[x,col];
        }
        
        hdf[idx,3] <- sum;
        hdf[idx,2] <- nrow(tdf);
      }
    }
  }
  
  if(sum_==FALSE)
  {
    for(x in 1:nrow(hdf))
    {
      hdf[x,3] <- hdf[x,3] / hdf[x,2];
    }
  }
  
  xdf = filterOutMissingValues(hdf, 2)
  xdf = filterOutMissingValues(xdf, 3)
  xdf = filterBy(xdf, 2, (function(x) {x > 0}))
  
  return(xdf)
}

plotTimeData = function(w_dft, d_dft, s_ylab, s_title)
{
  options(scipen=999)
  p = ggplot() + 
    geom_line(data = w_dft, aes(x = year, y = data, colour = "Will")) +
    geom_line(data = d_dft, aes(x = year, y = data, colour = "Denzel")) +
    xlab('Year') +
    ylab(s_ylab) +
    theme(legend.title = element_blank()) +
    ggtitle(s_title)
  return(p);
}


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


plotBarChartAvg = function(mdf_will, mdf_den, col, s_ylab, s_title, sum = FALSE)
{
  options(scipen=999)
  will.ratings = filterOutMissingValues(mdf_will, col);
  denzel.ratings = filterOutMissingValues(mdf_den, col);
  
  rdf <- as.data.frame(matrix(0, ncol = 2, nrow = 2));
  names(rdf)[1] <- "actor";
  names(rdf)[2] <- "avg";
  rdf[1,1] <- "Will";
  rdf[2,1] <- "Denzel";
  
  if(sum == TRUE)
  {
    rdf[1,2] <- (sum(will.ratings[,col]));
    rdf[2,2] <- (sum(denzel.ratings[,col]));
  }else
  {
    rdf[1,2] <- (sum(will.ratings[,col])) / nrow(will.ratings);
    rdf[2,2] <- (sum(denzel.ratings[,col])) / nrow(denzel.ratings);
  }
  p <- ggplot(data=rdf, aes(x=actor, y=avg, fill=avg)) 
  p <- p +geom_bar(stat="identity", position=position_dodge())
  p <- p + xlab("Actor");
  p <- p + ylab(s_ylab);
  p <- p + theme(legend.position="none")
  p <- p + ggtitle(s_title);
  p
}

buildGenresAvgDf = function(df, col, sum=FALSE)
{
  options(scipen=999)
  gdf <- as.data.frame(matrix(0, ncol = 3, nrow = 14));
  names(gdf)[1] <- "genre";
  names(gdf)[2] <- "count";
  names(gdf)[3] <- "data";
  
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
        gdf[j,3] <- gdf[j,3] + df[i,col];
      }
    }
  }
  
  if(sum == FALSE)
  {
    for(j in 1:14)
    {
      if(gdf[j,3] > 0)
      {
        gdf[j,3] <- gdf[j,3] / gdf[j,2];
      }
    }
  }
  
  return(gdf);
}

plotGenresAvgDf = function(gdf, s_title, s_xlab, s_ylab, ymax = NULL)
{
  p <- ggplot(gdf, aes(x=genre, y=data, fill=genre));
  p <- p +geom_bar(stat="identity", position=position_dodge())
  p <- p + geom_bar(stat="identity", width=1, color="Black");
  p <- p + ggtitle(s_title);
  p <- p + xlab(s_xlab);
  p <- p + ylab(s_ylab);
  p <- p + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  
  if(!is.null(ymax))
  {
    p <- p + coord_cartesian(ylim = c(0, ymax))
  }
  return(p)
}











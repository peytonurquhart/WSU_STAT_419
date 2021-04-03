github.monte.raw = "https://raw.githubusercontent.com/MonteShaffer/";
include.setup = paste0(github.monte.raw, "humanVerse/main/include.setup.R");
source(include.setup);  # Maybe comment this out, if error 
 
github.monte.http = "https://github.com/MonteShaffer/";
######## we will parse this page to get a list of the .R functions to include ########
github.monte.humanVerse = paste0(github.monte.http, "humanVerse/tree/main/humanVerse/R/"); 

######## you can pass flag `force.download = TRUE` if you want to make certain it is not coming from cache ########

###### R::humanVerse #####
includeGithubFolder(github.monte.humanVerse, force.download = TRUE); 
 
  


folder.owell = paste0(github.monte.raw, "humanVerse/main/data/o-well/");

file.location = paste0(folder.owell, "wells-location-plus.txt");

wells.pipe = readFromPipe(file.location);  

wells.pipe = replaceFactorColumnWithIndicatorVariables(wells.pipe, "geology", use.boolean=FALSE);

wells.pipe;








file.location = paste0(folder.owell, "well-23.R");
source(file.location);

names(wells);

# "sodium" is Table 6

Wilcox = wells$sodium$Wilcox;
Wilcox = str_replace("C","", Wilcox);
Wilcox = str_replace("S","", Wilcox);
tmp = explodeMe("-",Wilcox,NULL);
Wilcox.C = as.numeric( getElementsInList(tmp, 1) ); 
Wilcox.S = as.numeric( getElementsInList(tmp, 2) ); 

wells$sodium$Wilcox.C = Wilcox.C;
wells$sodium$Wilcox.S = Wilcox.S;

wells;









wells.df = wells.pipe;
  wells.df = merge(wells.df, wells$metals, by="well");
  wells.df = merge(wells.df, wells$chem, by="well");
  wells.df = merge(wells.df, wells$sodium, by="well");

dim(wells.df); 
nrow(wells.df);
ncol(wells.df);

wells.df;  # do we have any variable collision?
  
library(geosphere);  
library(measurements); 
library(pracma);    
# http://www.csgnetwork.com/degreelenllavcalc.html 
# default "mi" is miles as in statute miles (5280 feet)
# GREAT CIRCLE ... http://en.wikipedia.org/wiki/Great_circle_distance
# ??distHaversine 
convertLatitudeLongitudeToDistance = function(lat, format="mi")
  {
	m1 = 111132.92; m2 = -559.82; m3 = 1.175; m4 = -0.0023;
	p1 = 111412.84; p2 = -93.5;   p3 = 0.118;
		
	lat = pracma::deg2rad(lat);
	# lon = pracma::deg2rad(lon);
	# note:  longitude is not required
	# below computes in meters
	latlen  = m1 + (m2 * cos(2 * lat)) + (m3 * cos(4 * lat)) +	(m4 * cos(6 * lat));
	lonlen = (p1 * cos(lat)) + (p2 * cos(3 * lat)) + (p3 * cos(5 * lat));
	
	res = list();
	  res$format     = format;
	  res$lat.factor = measurements::conv_unit( latlen,  "m", format);
	  res$lon.factor = measurements::conv_unit( lonlen,  "m", format);
	res;
  }

latlon = convertLatitudeLongitudeToDistance( mean(wells.df$latitude) );
wells.df$lat.mi = latlon$lat.factor * wells.df$latitude;
wells.df$lon.mi = latlon$lon.factor * wells.df$longitude;


# names(wells.df);
# d1 = conv_unit(  distm( wells.df[,3:2], fun=distHaversine) ,  "m", "mi");
# d2 = as.matrix ( dist( wells.df[,55:56], 
#               method="euclidean", diag=TRUE, upper=TRUE) );
# sum( d2 - d1 ) / (23 * 23);



library(measurements); # install.packages("measurements");
conv_unit(2.54, "cm", "inch");

wells.df$altitude.mi = conv_unit(wells.df$altitude.ft, "ft", "mi");
wells.df$sea.mi = conv_unit(wells.df$sea.m, "m", "mi");
wells.df$fault.mi = conv_unit(wells.df$fault.m, "m", "mi");

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

countAllChars_f = function(filepath)
{
  n = file.info(filepath)$size;
  return(n);
}

# n = max entries in sample
scaleAsPercent = function(df, n)
{
  for(i in 1:nrow(df)) 
  {
    for (j in 1:ncol(df))
    {
      df[i,j] <- ((df[i,j]) / n) * 100;
    }
  }
  
  return(df);
}

# Convert letter-count table to histogram-able data set for declaration problem
xyDataframeFromColYDataframe = function(df1)
{
  alpha = "abcdefghijklmnopqrstuvwxyz";
  
  df <- as.data.frame(matrix(0, ncol = 3, nrow = 26));
  names(df)[1] <- "idx";
  names(df)[2] <- "count";
  names(df)[3] <- "alpha";
  
  for(i in 1:26)
  {
    ci = substr(alpha,i,i);
    df$idx[i] <- df$idx[i] + i;
    df$alpha[i] <- ci
    df$count[i] <- df$count[i] + df1[[ci]];
  }
  return(df);
}

plotDeclData = function(dfl, dff)
{
  alpha <- letters[1:26]
  n <- 1:26
  
  p = ggplot() + 
    geom_line(data = dfl, aes(x = idx, y = count), color = "blue") +
    geom_line(data = dff, aes(x = idx, y = count), color = "red") +
    xlab('Letters') +
    ylab('Frequency (%)') +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
    scale_x_continuous("Letters", labels = alpha, breaks = n) +
    ggtitle("Letter frequency in Declaration Drafts. Blue:Rough Red:Final")
  
  print(p)
}

awardsSimulation = function(n, pz, p1, p2, p3)
{
  df <- as.data.frame(matrix(0, ncol = 2, nrow = n));
  names(df)[1] <- "person";
  names(df)[2] <- "num.awards";
  set.seed(NULL)
  for (i in 1:n)
  {
    df$person[i] <- i;
    
    r = runif(1,0,1)
    if (r < p3)
    {
      df$num.awards[i] = 3;
    } else if (r < p2)
    {
      df$num.awards[i] = 2;
    } else if (r < p1)
    {
      df$num.awards[i] = 1;
    } else
    {
      df$num.awards[i] = 0;
    }
  }
  return(df)
}

awardsCount = function(n, adf)
{
  df <- as.data.frame(matrix(0, ncol = 2, nrow = 4));
  names(df)[1] <- "awards";
  names(df)[2] <- "count";
  
  df$awards[1] = 0
  df$count[1] = 0
  
  df$awards[2] = 1
  df$count[2] = 0
  
  df$awards[3] = 2
  df$count[3] = 0
  
  df$awards[4] = 3
  df$count[4] = 0
  
  for(i in 1:n)
  {
    if(adf$num.awards[i] == 0)
    {
      df$count[1] <- df$count[1] + 1;
    }
    if(adf$num.awards[i] == 1)
    {
      df$count[2] <- df$count[2] + 1;
    }
    if(adf$num.awards[i] == 2)
    {
      df$count[3] <- df$count[3] + 1;
    }
    if(adf$num.awards[i] == 3)
    {
      df$count[4] <- df$count[4] + 1;
    }
  }
  
  return(df)
}

nameWellDescEntry = function(wdf, cname, property, units, description)
{
  for(i in 1:41)
  {
    if(wdf$variable[i] == cname)
    {
      wdf$property[i] <- property;
      wdf$units[i] <- units;
      wdf$desc[i] <- description;
    }
  }
  
  return(wdf);
}


















  
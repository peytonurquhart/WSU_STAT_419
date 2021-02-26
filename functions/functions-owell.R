require("ggplot2")
library(reshape2)

plotStDevMetalsForWellData = function(metalsDf)
{
  # Remove the unneeded columns from the dataframe
  for(i in 16:21)
  {
    metalsDf <- metalsDf[-c(16)];
  }
  dfx = createStDevDataFrame(metalsDf);
  plotSingleRowBarChart(dfx, "Metal", "Standard Deviation", "Standard Deviation for Metal Water Content: n=23");
}

plotMeanMetalsForWellData = function(metalsDf)
{
  # Remove the unneeded columns from the dataframe
  for(i in 16:21)
  {
    metalsDf <- metalsDf[-c(16)];
  }
  dfx = createMeanDataFrame(metalsDf);
  plotSingleRowBarChart(dfx, "Metal", "Mean", "Mean Metal Water Content in ug/L");
}

plotCVMetalsForWellData = function(metalsDf)
{
  # Remove the unneeded columns from the dataframe
  for(i in 16:21)
  {
    metalsDf <- metalsDf[-c(16)];
  }
  dfx = createCVDataFrame(metalsDf);
  plotSingleRowBarChart(dfx, "Metal", "CV", "Coefficient of Variance for Metal Water Content");
}

plotMeanChemForWellData = function(chemDf)
{
  # Remove the unneeded columns from the dataframe
  for(i in 21:26)
  {
    chemDf <- chemDf[-c(21)];
  }
  dfx = createMeanDataFrame(chemDf);
  plotSingleRowBarChart(dfx, "Chemistry", "Mean", "Mean Chemical Water Content in mg/L");
}

plotStDevChemForWellData = function(chemDf)
{
  # Remove the unneeded columns from the dataframe
  for(i in 21:26)
  {
    chemDf <- chemDf[-c(21)];
  }
  dfx = createStDevDataFrame(chemDf);
  plotSingleRowBarChart(dfx, "Chemistry", "Standard Deviation", "Standard Deviation for Chemical Water Content: n=23");
}

plotCVChemForWellData = function(chemDf)
{
  # Remove the unneeded columns from the dataframe
  for(i in 21:26)
  {
    chemDf <- chemDf[-c(21)];
  }
  dfx = createCVDataFrame(chemDf);
  plotSingleRowBarChart(dfx, "Chemistry", "CV", "Coefficient of Variance for Chemical Water Content");
}

plotHgContentVsSand = function(metalsDf)
{
  df <- as.data.frame(matrix(0, ncol = 2, nrow = 3));
  
  names(df)[1] = "type"
  names(df)[2] = "mean.Hg"
  df[1,1] = "Sandy"
  df[2,1] = "Partly Sandy"
  df[3,1] = "Not Sandy"
  
  numSandy = 0;
  numPartSandy = 0;
  numNotSandy = 0;
  
  for(i in 1:23)
  {
    st = metalsDf[i,17]
    if(st == "Sand")
    {
      df[1,2] <- df[1,2] + metalsDf[i,10]
      numSandy = numSandy + 1;
    } 
    else if (grepl("Sand",st, fixed = TRUE) && !grepl("Sandstone",st, fixed = TRUE))
    {
      df[2,2] <- df[2,2] + metalsDf[i,10]
      numPartSandy = numPartSandy + 1;
    }
    else
    {
      df[3,2] <- df[3,2] + metalsDf[i,10]
      numNotSandy = numNotSandy + 1;
    }
  }
  
  df[1,2] <- df[1,2] / numSandy;
  df[2,2] <- df[2,2] / numPartSandy;
  df[3,2] <- df[3,2] / numNotSandy;
  
  p <- ggplot(df, aes(x = type, y = mean.Hg)) + geom_bar(stat="identity", position = "dodge");
  p <- p + ggtitle("Hg Content vs Sandy Geology") + ylab("Mean Hg (ug/L)") + xlab("Geology");
  
  suppressWarnings(suppressMessages(print(p)));
}


# Helper Functions -------------------------------------------------------------


# Plot the pearson correlation for cols 'xc' vs 'yc' in dataframe df
plotPearsonCorrelation = function(df, xc, yc, s_title, x_lab, y_lab)
{
  s_r = sprintf("%0.3f",(cor(xc, yc ,method = "pearson")));
  s_r = paste0("r = ", s_r);
  
  p <- ggplot(df, aes(x=xc, y=yc)) 
  p <- p + geom_point() 
  p <- p + ggtitle(s_title) 
  p <- p + geom_smooth(method=lm, se=FALSE) 
  p <- p + scale_x_continuous(name = x_lab, limits = c(min(xc), max(xc))) 
  p <- p + scale_y_continuous(name = y_lab, limits = c(min(yc), max(yc))) 
  p <- p + theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black")) 
  p <- p + theme_bw()
  p <- p + geom_text(x=(max(xc) / 1.2), y=(max(yc) / 1.2), label=s_r)
  
  suppressWarnings(suppressMessages(print(p)));
}

# Given a data frame with single row, create a bar chart
plotSingleRowBarChart = function(dfMeans, s_xname, s_yname, title, print=TRUE)
{
  # Restructure Data to be columns instead of rows
  dfMeans <- melt(data=dfMeans);
  names(dfMeans)[1] = s_xname;
  names(dfMeans)[2] = s_yname;
  
  p <- ggplot(dfMeans, aes(x = dfMeans[,1], y = dfMeans[,2])) + geom_bar(stat="identity", position = "dodge");
  p <- p + ggtitle(title) + xlab(s_xname) + ylab(s_yname);
  
  if(print)
  {
    suppressWarnings(suppressMessages(print(p)));
  }
}

# Create a dataframe for mean of columns in df, using same column names
createMeanDataFrame = function(df)
{
  n = 0;
  for(i in names(df))
  {
    n = n + 1;
  }
  dfm <- as.data.frame(matrix(0, ncol = n, nrow = 1));
  for(i in 1:n)
  {
    names(dfm)[i] = names(df)[i];
    dfm[1,i] = mean(df[,i]);
  }
  return(dfm);
}

# Create a dataframe for stdev of columns in df, using same column names
createStDevDataFrame = function(df)
{
  n = 0;
  for(i in names(df))
  {
    n = n + 1;
  }
  dfm <- as.data.frame(matrix(0, ncol = n, nrow = 1));
  for(i in 1:n)
  {
    names(dfm)[i] = names(df)[i];
    dfm[1,i] = sd(df[,i]);
  }
  return(dfm);
}

# Create a dataframe for coefficient of variance of columns in df, using same column names
createCVDataFrame = function(df)
{
  n = 0;
  for(i in names(df))
  {
    n = n + 1;
  }
  dfm <- as.data.frame(matrix(0, ncol = n, nrow = 1));
  for(i in 1:n)
  {
    names(dfm)[i] = names(df)[i];
    dfm[1,i] = (sd(df[,i]) / mean(df[,i]));
  }
  return(dfm);
}

# CSV filepath as input, returns a dataframe
csvToDataframe = function(filepath)
{
  return(read.csv(filepath));
}

readWellCsvData = function(filepath)
{
  df = csvToDataframe(filepath);
  df <- df[-c(1)];
  return(df);
}



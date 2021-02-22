require("ggplot2")
library(reshape2)

plotStDevMetalsForWellData = function(metalsDf)
{
  # Remove the unneeded columns from the dataframe
  for(i in 16:20)
  {
    metalsDf <- metalsDf[-c(16)];
  }
  dfx = createStDevDataFrame(metalsDf);
  plotSingleRowBarChart(dfx, "Metal", "Standard Deviation", "Standard Deviation for Metal Water Content: n=23");
}

plotMeanMetalsForWellData = function(metalsDf)
{
  # Remove the unneeded columns from the dataframe
  for(i in 16:20)
  {
    metalsDf <- metalsDf[-c(16)];
  }
  dfx = createMeanDataFrame(metalsDf);
  plotSingleRowBarChart(dfx, "Metal", "Mean", "Mean Metal Water Content in ug/L");
}


plotMeanChemForWellData = function(chemDf)
{
  # Remove the unneeded columns from the dataframe
  for(i in 21:25)
  {
    chemDf <- chemDf[-c(21)];
  }
  dfx = createMeanDataFrame(chemDf);
  plotSingleRowBarChart(dfx, "Chemistry", "Mean", "Mean Chemical Water Content in mg/L");
}


plotStDevChemForWellData = function(chemDf)
{
  # Remove the unneeded columns from the dataframe
  for(i in 21:25)
  {
    chemDf <- chemDf[-c(21)];
  }
  dfx = createStDevDataFrame(chemDf);
  plotSingleRowBarChart(dfx, "Chemistry", "Standard Deviation", "Standard Deviation for Chemical Water Content: n=23");
}


# Helper Functions -------------------------------------------------------------


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
    print(p);
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



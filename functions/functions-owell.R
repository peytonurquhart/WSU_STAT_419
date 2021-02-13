
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
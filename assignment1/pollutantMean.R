pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  files_list <- list.files(directory, full.names=TRUE)   
  
  # join the necessary files into a working dataFrame
  dataFrame <- data.frame()                             
  for (i in id) {                                
    dataFrame <- rbind(dataFrame, read.csv(files_list[i]))
  }
  
  # calculate and return mean
  obtainedMean <- mean(dataFrame[,pollutant], na.rm=TRUE)
  round(obtainedMean, 3)                    # rounds to 3 digits after decimal
}

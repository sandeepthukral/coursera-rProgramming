corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # get list of all files in the folder
  files_list <- list.files(directory, full.names=TRUE) 
  
  # empty vector to store the correlation
  vectorCorrelations <- vector()
  
  # for each file
  for (i in seq_along(files_list)){
  
    ## load the file into a temp data table
    tempDataTable <- data.frame()
    tempDataTable = read.csv(files_list[i])
    dim(tempDataTable)
    
    ## find how many full readings we have
    tempDataTable <- tempDataTable[complete.cases(tempDataTable),]
    
    ## if this number is greater than threshold, then
    if (nrow(tempDataTable) > threshold){
      ### then find correlation, add to vector
      correlation <- cor(tempDataTable$sulfate, tempDataTable$nitrate)
      vectorCorrelations <- c ( vectorCorrelations, correlation)
    }
  }
  # return vector
  vectorCorrelations
}

cr <- corr("specdata", 150)
head(cr)
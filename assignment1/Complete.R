complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  files_list <- list.files(directory, full.names=TRUE) 
  
  # join the necessary files into a working dataFrame
  dataFrame <- data.frame()                             
  for (i in id) {                                
    dataFrame <- rbind(dataFrame, read.csv(files_list[i]))
  }
  
  completes <- complete.cases(dataFrame)
  
  dataFrame <- dataFrame[completes, ]
  
  ID <- numeric(length(id))
  nobs <- numeric(length(id))
  
  for (i in seq_along(id)) {
    subSet <- subset(dataFrame,ID == id[i])
    ID[i] = id[i]
    nobs[i] <- nrow(subSet)
  }
  
  data.frame(ID, nobs)
  
}

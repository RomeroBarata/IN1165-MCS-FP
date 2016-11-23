readDataSets <- function(path, files_names = NULL){
  if (is.null(files_names)) 
    files_names <- list.files(file.path(path, ""), pattern = ".csv$")
  
  readData <- function(file_name){
    read.csv(file.path(path, file_name), header = TRUE, strip.white = TRUE)
  }
  
  data_list <- lapply(files_names, readData)
  names(data_list) <- as.character(strsplit(files_names, split = ".csv"))
  
  organizeClasses <- function(data){
    data$Class <- factor(data$Class, levels = c(1, -1))
    data
  }
  
  data_list <- lapply(data_list, organizeClasses)
  
  data_list
}
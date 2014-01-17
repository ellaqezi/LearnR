corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  com <- complete(directory)
  dir <- com[com$nobs > threshold,]
  cr <- numeric()
  if (length(dir[,1]) > 0) {
    for (i in 1:length(dir[,1])) {
      data <- getmonitor(dir[i,1], directory)
      data <- data[complete.cases(data),]
      cr[i] = cor(data$nitrate, data$sulfate)
    }
  }
  y <- cr
}



timeseries2Subseqs <- function(timeseries, windowSize, overlapping = FALSE) {
  
  # This function chunks a raw time series into subsequences based on the specified 
  # sliding window size and stype (i.e. overlapping or non-overlapping of the sliding window)
  # 
  #   Input:
  #       timeseries: a vector representing the raw time series
  #       
  #       windowSize: size of the sliding window
  #
  #       overlapping: a boolean value indicating whether a overlapping window is used,
  #                    default to FALSE
  #   Output:
  #       subsequences: a list, each entry is a subsequence 
  #
  
  # sanity check
  if (windowSize > length(timeseries)) {
    stop('window size cannot be larger than the length of the time series')
  }
  
  subsequences = list()
  
  idx = 1
  
  if (overlapping) {
    
    for (i in 1 : (length(timeseries) - windowSize + 1)) {
      subsequences[[idx]] = timeseries[i : (i + windowSize - 1)]
      
      idx = idx + 1
    }
    
  } else {
    
    remainder = length(timeseries) %% windowSize
    
    numWindows = (length(timeseries) - remainder) / windowSize
    
    for (i in 1 : numWindows) {
      subsequences[[idx]] = timeseries[((i - 1) * windowSize + 1) : (i * windowSize)]
      
      idx = idx + 1
    }
    
    # add the last subsequence, whose size is smaller than the specified window size
    
    if (remainder > 0) {
      subsequences[[idx]] = timeseries[(length(timeseries) - remainder + 1) : length(timeseries)]
    }
    
  }
  
  return(subsequences)
  
}
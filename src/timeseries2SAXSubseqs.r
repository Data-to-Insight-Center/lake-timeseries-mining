timeseries2SAXSubseqs <- function(timeseries, windowSize, overlapping = FALSE,
                                  wordSize, alphabetSize) {
  
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
  #
  #       wordSize: the number of symbols/characters a sliding window will be mapped to
  #
  #       alphabetSize: alphabet size used to construct the strings
  
  #   Output:
  #       subsequences: a list, each entry is a list x, x$index is (startIdx, endIdx),
  #                     x$sebseq is extracted subsequence, x$saxNumRep is the number
  #                     representation, $saxInteger is sax word's integer representation
  #
  
  # sanity check
  if (windowSize > length(timeseries)) {
    stop('window size cannot be larger than the length of the time series')
  }
  
  source("./timeseries2Subseqs.r")
  
  subsequences = list()
  
  idx = 1
  
  if (overlapping) {
    
    for (i in 1 : (length(timeseries) - windowSize + 1)) {
      
      index = c(i, i + windowSize - 1)

      subseq = timeseries[i : (i + windowSize - 1)]

      saxNumRep = (timeseries2Symbol(subseq, wordSize, alphabetSize))$num_rep

      saxInteger = saxNumRep2Integer(saxNumRep, alphabetSize)

      subseqData = list(index = index, subseq = subseq, saxNumRep = saxNumRep,
                  saxInteger = saxInteger)

      subsequences[[idx]] = subseqData
      
      idx = idx + 1
    }
    
  } else {
    
    remainder = length(timeseries) %% windowSize
    
    numWindows = (length(timeseries) - remainder) / windowSize
    
    for (i in 1 : numWindows) {
      
      index = c((i - 1) * windowSize + 1, i * windowSize)

      subseq = timeseries[((i - 1) * windowSize + 1) : (i * windowSize)]
      
      saxNumRep = (timeseries2Symbol(subseq, wordSize, alphabetSize))$num_rep

      saxInteger = saxNumRep2Integer(saxNumRep, alphabetSize)
      
      subseqData = list(index = index, subseq = subseq, saxNumRep = saxNumRep,
                        saxInteger = saxInteger)

      subsequences[[idx]] = subseqData
      
      idx = idx + 1
    }
    
    # add the last subsequence, whose size is smaller than the specified window size
    
    if (remainder > 0) {
      
      index = c(length(timeseries) - remainder + 1, length(timeseries))

      subseq = timeseries[(length(timeseries) - remainder + 1) : length(timeseries)]

      saxNumRep = (timeseries2Symbol(subseq, wordSize, alphabetSize))$num_rep

      saxInteger = saxNumRep2Integer(saxNumRep, alphabetSize)

      subseqData = list(index = index, subseq = subseq, saxNumRep = saxNumRep,
                  saxInteger = saxInteger)

      subsequences[[idx]] = subseqData
    }
    
  }
  
  return(subsequences)
  
}

saxNumRep2Integer <- function(saxNumRep, alphabetSize) {
  
  num = 0
  
  for (i in 1 : length(saxNumRep)) {
    
    num = num + alphabetSize ^ (i - 1) * (saxNumRep[i] - 1)
    
  }
  
  return(num)
}
kmeansTSB <- function(tsData, windowSize, overlapping = FALSE, wordSize, 
                      alphabetSize, level, norm, numCenters, maxIterNum, algo) {

  # This function conducts a kmeans clustering of a set of time series by transforming
  # the time series into Time Series Bitmap (TSB) representation. The resulting kmeans object 
  # is returned for further visualization.
  #
  #
  #   Input:
  #       tsData: a numerical matrix, each row is a time series
  #
  #       windowSize: size of the sliding window
  #
  #       overlapping: whether the sliding window should be overlapped or not, default
  #                    to FALSE
  #
  #       wordSize: the number of symbols/characters a sliding window will be mapped to
  #
  #       alphabetSize: alphabet size used to construct the strings
  #
  #       level: the length of SAX "subword", the resulting TSB matrix will have
  #             (alphabet_size) ^ level cells, each cell recording frequency for 
  #             one possible SAX "subword"
  #
  #       norm: a string specifying the distance norm when measuring the distance of two
  #             matrices, supported values are "L1", "L2", and "inf"
  #
  #       numCenters: the number of clusters, say k
  #
  #       maxIterNum: the maximum number of iterations allowed
  #
  #       algo: character: may be abbreviated, c("Hartigan-Wong", "Lloyd", "Forgy",
  #                        "MacQueen")
  #   Output:
  #       kclustObj: a kmenas object resulted from kmeans clustering
  #  
  
  numTimeseries = dim(tsData)[1]
  
  # sanity check
  if (numTimeseries == 1) {
    stop('Only one time series, no need for clustering')
  }
  
  source("./timeseries2TSB.r")
  
  for (i in 1 : numTimeseries) {
    ts = tsData[i, ]
    
    # transform time series to TSB
    tsb = timeseries2TSB(ts, windowSize, overlapping, wordSize, 
                          alphabetSize, level)
    
    # tsb as vector
    vector = as.vector(tsb)
    
    if (i == 1) {
      tsbAsVectorData = vector
    } else {
      tsbAsVectorData = rbind(tsbAsVectorData, vector)
    }
  }
  
  
  kclustObj = kmeans(tsbAsVectorData, centers = numCenters, iter.max = maxIterNum,
                     nstart = 1, algorithm = algo)
  
  return(kclustObj)
  
}
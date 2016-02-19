hclusteringTSB <- function(tsData, windowSize, overlapping = FALSE, wordSize, 
                           alphabetSize, level, norm = "L2", aggloMethod) {

  # This function conducts a hierarchical clustering of a set of time series by transforming
  # the time series into Time Series Bitmap (TSB) representation. The resulting 'hclust' object 
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
  #       aggloMethod: the agglomeration method to be used. This should be 
  #                    (an unambiguous abbreviation of) one of "ward.D", "ward.D2",
  #                    "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA),
  #                    "median" (= WPGMC) or "centroid" (= UPGMC)
  #
  #   Output:
  #       hclustObj: a 'hclust' object resulted from hierarchical clustering
  #  
  
  numTimeseries = dim(tsData)[1]
  
  # sanity check
  if (numTimeseries == 1) {
    stop('Only one time series, no need for clustering')
  }
  
  # initialize distance matrix, diag is set to 0s
  distMatrix = matrix(0, nrow = numTimeseries, ncol = numTimeseries)
  
  source("./timeseries2TSB.r")
  source("./matrixDist.r")
  
  # since the distance matrix is symmetric, only need to access the lower part
  for (i in 2 : numTimeseries) {
    
    timeseriesX = tsData[i, ]
    
    # transform time series to TSB
    tsbX = timeseries2TSB(timeseriesX, windowSize, overlapping, wordSize, 
                          alphabetSize, level)
    
    for (j in 1 : (i - 1)) {
      
      
      timeseriesY = tsData[j, ]
        
      tsbY = timeseries2TSB(timeseriesY, windowSize, overlapping, wordSize, 
                            alphabetSize, level)
      
      distMatrix[i, j] = matrixDist(tsbX, tsbY, norm)
      
      # distance is symmetric
      distMatrix[j, i] = distMatrix[i, j]
    }
  }
  
  # transform the matrix into a "dist" object
  distMatrix = as.dist(distMatrix)
  
  hclustObj = hclust(distMatrix, method = aggloMethod, members = NULL)
  
  return(hclustObj)
}
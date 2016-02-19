hclusteringSAX <- function(tsData, wordSize, alphabetSize, aggloMethod) {
  
  # This function conducts a hierarchical clustering of a set of time series by transforming
  # the time series into SAX representation. The resulting 'hclust' object is returned for
  # further visualization. Since in practice it is often meaningless to directly convert a
  # a long raw time series into a single SAX string, we may only use this function to cluster
  # subsequences (perhaps within the raw time series). For hierarchical clustering of raw
  # time series, we may want to use 'hclusteringTSB' (see 'hclusteringTSB.r') 
  #
  # Note that each time series is transformed into a single SAX word/string
  #
  #   Input:
  #       tsData: a numerical matrix, each row is a time series
  #
  #       wordSize: the number of symbols/characters a time seires will be mapped to
  #
  #       alphabetSize: alphabet size used to construct the strings
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
  
  timeseriesLen = dim(tsData)[2]
  
  compressionRatio = timeseriesLen / wordSize
  
  # initialize distance matrix, diag is set to 0s
  distMatrix = matrix(0, nrow = numTimeseries, ncol = numTimeseries)
  
  source("./timeseries2Symbol.r")
  source("./minDist.r")
  
  # since the distance matrix is symmetric, only need to access the lower part
  for (i in 2 : numTimeseries) {
    
    timeseriesX = tsData[i, ]
    # whole time series transformed to a SAX string of length 'wordSize'
    saxX = (timeseries2Symbol(timeseriesX, wordSize, alphabetSize))$num_rep
    
    for (j in 1 : (i - 1)) {
         
      timeseriesY = tsData[j, ]
      
      saxY = (timeseries2Symbol(timeseriesY, wordSize, alphabetSize))$num_rep
      
      distMatrix[i, j] = minDist(saxX, saxY, alphabetSize, compressionRatio)
      
      # distance is symmetric
      distMatrix[j, i] = distMatrix[i, j]
    }
  }
  
  # transform the matrix into a "dist" object
  distMatrix = as.dist(distMatrix)
  
  hclustObj = hclust(distMatrix, method = aggloMethod, members = NULL)
  
  return(hclustObj)
  
}
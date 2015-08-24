knnTSB <- function(trainTS, trainLabel, testTS, k, windowSize, overlapping = FALSE, 
                   wordSize, alphabetSize, level) {
  
  # This function conducts a knn classification by transforming the time series into 
  # Time Series Bitmap (TSB) representation. 
  #
  #
  #   Input:
  #       trainTS: training time series set, a numerical matrix, each row is a time series
  #
  #       trainLabel: a vector of integers, each one corresponds to the categorical value of 
  #                   a row in trainTS
  #
  #       testTS: testing time series set, a numerical matrix, each row is a time series to
  #               be classified
  #
  #       k: the parameter 'k' in knn algorithm
  #
  #       Below parameters are related to TSB transformation
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
  #   Output:
  #       knnResult: result of knn classification, which is an object of type 'factor',
  #                  each element is the classified label for the corresponding test time series,
  #                  the probability can be retrieved by calling 'attributes(knnResult)$prob'
  #  
  
  # sanity check
  if (dim(trainTS)[1] != length(trainLabel)) {
    stop("# of training time series does not match the # of labels")
  }
  
  source("./timeseries2TSB.r")
  
  ##############################
  # convert training set to TSB
  ##############################
  
  numTimeseries = dim(trainTS)[1]
  
  for (i in 1 : numTimeseries) {
    ts = trainTS[i, ]
    
    # transform time series to TSB
    tsb = timeseries2TSB(ts, windowSize, overlapping, wordSize, 
                         alphabetSize, level)
    
    # tsb as vector
    vector = as.vector(tsb)
    
    if (i == 1) {
      trainTSAsTSB = vector
    } else {
      trainTSAsTSB = rbind(trainTSAsTSB, vector)
    }
  }
  
  ##############################
  # convert testing set to TSB
  ##############################
  
  numTimeseries = dim(testTS)[1]
  
  for (i in 1 : numTimeseries) {
    ts = testTS[i, ]
    
    # transform time series to TSB
    tsb = timeseries2TSB(ts, windowSize, overlapping, wordSize, 
                         alphabetSize, level)
    
    # tsb as vector
    vector = as.vector(tsb)
    
    if (i == 1) {
      testTSAsTSB = vector
    } else {
      testTSAsTSB = rbind(testTSAsTSB, vector)
    }
  }
  
  # To perform the real payload
  require(class)
  
  knnResult = knn(trainTSAsTSB, testTSAsTSB, trainLabel, k, prob = TRUE)
  
  return(knnResult)
  
}
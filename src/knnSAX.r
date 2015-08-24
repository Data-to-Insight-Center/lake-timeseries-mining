knnSAX <- function(trainTS, trainLabel, testTS, k, wordSize, alphabetSize) {
  
  # This function conducts a knn classification by transforming the time series into 
  # SAX representation. Note that we assume time series provided to this function are 
  # chunked subsequences (e.g., daily subsequence of lake time series) of the original 
  # time series, since it might be meaningless to directly transform a long time series 
  # into a single SAX word for comparison. If you want to classify original time series, 
  # use 'knnTSB.r' which transforms time series into Time Series Bitmap (TSB) representation.
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
  #       Below parameters are related to SAX transformation
  #
  #       wordSize: the number of symbols/characters a time series will be mapped to
  #
  #       alphabetSize: alphabet size used to construct the strings
  #
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
  
  source("./timeseries2Symbol.r")
  
  ##############################
  # convert training set to SAX
  ##############################
  
  numTimeseries = dim(trainTS)[1]
  
  for (i in 1 : numTimeseries) {
    ts = trainTS[i, ]
    
    # transform time series to SAX
    sax = (timeseries2Symbol(ts, wordSize, alphabetSize))$num_rep
    
    if (i == 1) {
      trainTSAsSAX = sax
    } else {
      trainTSAsSAX = rbind(trainTSAsSAX, sax)
    }
  }
  
  ##############################
  # convert testing set to SAX
  ##############################
  
  numTimeseries = dim(testTS)[1]
  
  for (i in 1 : numTimeseries) {
    ts = testTS[i, ]
    
    # transform time series to SAX
    sax = (timeseries2Symbol(ts, wordSize, alphabetSize))$num_rep
    
    if (i == 1) {
      testTSAsSAX = sax
    } else {
      testTSAsSAX = rbind(testTSAsSAX, sax)
    }
  }
  
  # To perform the real payload
  require(class)
  
  knnResult = knn(trainTSAsSAX, testTSAsSAX, trainLabel, k, prob = TRUE)
  
  return(knnResult)
}
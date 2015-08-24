anomalyDetectionTSB <- function(timeseries, tsbWinSize, subseqWinSize, overlapping = FALSE,
                                wordSize, alphabetSize, level, norm, scoreThreshold) {
  
  # This function conducts anomaly detection based on Time Series Bitmap (TSB), the general
  # idea is as follows: 
  # 
  # We create two concatenated windows, and slide them together across the sequence. 
  # At each time instance we build a time series bitmap for the two windows, and measure 
  # the distance between them. This distance we report as an anomaly score. We consider
  # there is an anomaly when the score is larger than the specified threshold
  #
  #
  #   Input:
  #       timeseries: a vector representing the raw time series
  #
  #       tsbWinSize: the length of the subsequence covered by the TSB window
  #
  #       subseqWinSize: For the TSB subsequence, we can in turn chunk
  #                      it to a series of subsequences using an overlapping/nonoverlapping
  #                      sliding window whose size is specified by 'subseqWinSize'.
  #                      Each sliding window will be symbolized as a SAX string.
  #
  #                      If you want to treat the TSB window as a single SAX word, then
  #                      simply specify tsbWinSize == subseqWinSize
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
  #       scoreThreshold: distance score greater than this value will be regarded as an
  #                       anomaly
  #
  #   Output:
  #       anomalies: a list of anomalies, each anomaly is represented as a two element list
  #                  (start = startIndex, end = endIndex)
  #
  #   For details, see below paper
  #
  #   Eamonn, N. K. V. N. L., Ratanamahatana, K. S. L. C. A., & Wei, L. (2005). 
  #   Time-series bitmaps: a practical visualization tool for working with large time series 
  #   databases. 
  #
  
  anomalies = list()
  idx = 1
  
  source("./timeseries2Subseqs.r")
  source("./timeseries2Symbol.r")
  source("./matrixDist.r")
  
  for (i in 1 : (length(timeseries) - 2 * tsbWinSize + 1)) {
    
    firstTSBWindow = timeseries[i : (i + tsbWinSize - 1)]
    
    secondTSBWindow = timeseries[(i + tsbWinSize) : (i + 2 * tsbWinSize -1)]
    
    
    firstTSBMatrix = timeseries2TSB(firstTSBWindow, subseqWinSize, overlapping, 
                                    wordSize, alphabetSize, level)
    
    secondTSBMatrix = timeseries2TSB(secondTSBWindow, subseqWinSize, overlapping, 
                                    wordSize, alphabetSize, level)
    
    if (matrixDist(firstTSBMatrix, secondTSBMatrix, norm) > scoreThreshold) {
      anomalies[[idx]] = list(start = i, end = (i + 2 * tsbWinSize -1))
      
      idx = idx + 1
    }
    
  }
  
  return(anomalies)
  
}
timeseries2TSB <- function(timeseries, windowSize, overlapping = FALSE, wordSize, 
                           alphabetSize, level) {
  
  # This function transforms a time series into a Time Series Bitmap (TSB) representation
  #
  #   Input:
  #       timeseries: a numerical vector representing the raw time series
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
  #       tsbMatrix: the transformed time series in TSB matrix representation
  #
  #   For details, see below paper
  #
  #   Eamonn, N. K. V. N. L., Ratanamahatana, K. S. L. C. A., & Wei, L. (2005). 
  #   Time-series bitmaps: a practical visualization tool for working with large time series 
  #   databases. 
  #
  
  source("./timeseries2Subseqs.r")
  
  subsequences = timeseries2Subseqs(timeseries, windowSize, overlapping)
  
  saxWords = list()
  index = 1
  
  source("./timeseries2Symbol.r")
  
  for (subseq in subsequences) {
    saxWords[[index]] = (timeseries2Symbol(subseq, wordSize, alphabetSize))$num_rep
    index = index + 1
  }
  
  source("./buildTSB.r")
  
  tsbMatrix = buildTSB(saxWords, level, alphabetSize)
  
  return(tsbMatrix)
  
}
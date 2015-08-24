anmolyDetectionWCAD <- function(tsInSAX, numWindows, threshold) {
 
  # This function detects multiple anomalies based on CDM distance
  # The genernal idea is as follows:
  # 
  # We can divide the input sequence into W contiguous sections/windows, and
  # assign the anomaly value of the ith window as CDM(W_i, data ). In
  # other words, we simply measure how well a small local section can
  # match the global sequence. Setting this parameter is not too
  # burdensome for many problems. For clarity, we call this slight variation 
  # Window Comparison Anomaly Detection (WCAD).
  #
  #
  #   Input:
  #       tsInSAX: a time series in SAX representation, e.g., "abadcefg"
  #       
  #       numWindows: number of contiguous sections/windows
  #
  #       threshold: distance threshold
  #
  #   Output:
  #       anomalies: a list, each entry within the list is a list representing 
  #                  the window whose CDM distance score is greater than the threshold, 
  #                  in the form of (start = windowStartCharOffset, end = windowEndCharOffset,
  #                  dist = CDMDistanceScore)
  #
  # For details, see below paper
  #
  # Keogh, Eamonn, Stefano Lonardi, and Chotirat Ann Ratanamahatana. 
  # "Towards parameter-free data mining." In Proceedings of the tenth 
  # ACM SIGKDD international conference on Knowledge discovery and data mining, 
  # pp. 206-215. ACM, 2004.  
  
  
  
  # calculate window size in such a way that each window has roughly the same size
  winSize = floor(nchar(tsInSAX) / numWindows)
  
  numRemainChars = nchar(tsInSAX) - winSize * numWindows
  
  numExtraCharsPerWindow = floor(numRemainChars / numWindows)
  
  startIdx = 1
  
  anomalies = list()
  listIdx = 1
  
  source("./cdmDist.r")
  
  for (i in 1 : (numWindows -1)) {
    
    endIdx = startIdx + winSize + numExtraCharsPerWindow - 1
    
    strInWindow = substr(tsInSAX, startIdx, endIdx)
    
    dist = cdmDist(strInWindow, tsInSAX)
    
    if (dist > threshold) {
      anomalies[[listIdx]] = list(start = startIdx, end = endIdx, dist = dist)
      
      listIdx = listIdx + 1
    }
    
    startIdx = endIdx + 1
    
  }
  
  # deal with the last window
  strInWindow = substr(tsInSAX, startIdx, nchar(tsInSAX))
  dist = cdmDist(strInWindow, tsInSAX)
  
  if (dist > threshold) {
    anomalies[[listIdx]] = list(start = startIdx, end = nchar(tsInSAX), dist = dist)
  }
  
  return(anomalies)
}
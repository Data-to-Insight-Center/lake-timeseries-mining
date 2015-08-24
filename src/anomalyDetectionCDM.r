anomalyDetectionCDM <- function(tsInSAX) {
  
  # This function detects a single anomaly from a time series in SAX representation,
  # The genernal idea is as follows:
  # 
  # We use a divide- and-conquer strategy. The algorithm works as follows: Both the left and 
  # right halves of the entire sequence being examined are compared to the entire sequence using 
  # the CDM dissimilarity measure. The intuition is that the side containing the most unusual 
  # section will be less similar to the global sequence than the other half. Having identified 
  # the most interesting side, we can recursively repeat the above, repeatedly dividing the most 
  # interesting section until we can no longer divide the sequence.
  #
  # Limitation:
  # 
  # First, it assumes a single anomaly in the dataset. Second, in the first few iterations, 
  # the measure needs to note the difference a small anomaly makes, even when masked by a 
  # large amount of surrounding normal data.
  #
  # Check "anmolyDetectionWCAD.r" for an enhanced version also based on CDM measure
  #
  #   Input:
  #       tsInSAX: a time series in SAX string representation, e.g., "abadcefg"
  #       
  #   Output:
  #       locOfAnomaly: location of the anomaly, as the character offset
  #
  #
  # For details, see below paper
  #
  # Keogh, Eamonn, Stefano Lonardi, and Chotirat Ann Ratanamahatana. 
  # "Towards parameter-free data mining." In Proceedings of the tenth 
  # ACM SIGKDD international conference on Knowledge discovery and data mining, 
  # pp. 206-215. ACM, 2004.
  
  locOfAnomaly = 1
  
  source("./cdmDist.r")
  
  while (nchar(tsInSAX) > 2) {
    
    leftStr = substr(tsInSAX, 1, floor(nchar(tsInSAX) / 2))
    leftDist = cdmDist(leftStr, tsInSAX)
    
    rightStr = substr(tsInSAX, ceiling(nchar(tsInSAX) / 2), nchar(tsInSAX))
    rightDist = cdmDist(rightStr, tsInSAX)
    
    if (leftDist < rightDist) {
      
      locOfAnomaly = locOfAnomaly + floor(nchar(tsInSAX) / 2)
      
      tsInSAX = rightStr
      
    } else {
      
      tsInSAX = leftStr
    }
  }
  
  return(locOfAnomaly)
}
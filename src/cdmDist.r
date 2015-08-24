cdmDist <- function(stringX, stringY) {
  
  # This function computes the Compression-based Dissimilarity Measure (CDM) 
  #
  #   Input:
  #       stringX: first string (e.g., a SAX string), e.g., "abadcefg"
  #
  #       stringY: second string (e.g., a SAX string), e.g., "abaceefg"
  #       
  #   Output:
  #       cdmDistance: CDM distance. The CDM dissimilarity is close to 1 when
  #                    stringX and stringY are not related, and smaller than one 
  #                    if they are related. The smaller the CDM, the more closely 
  #                    related stringX and stringY are.
  #
  # For details, see below paper
  #
  # Keogh, Eamonn, Stefano Lonardi, and Chotirat Ann Ratanamahatana. 
  # "Towards parameter-free data mining." In Proceedings of the tenth 
  # ACM SIGKDD international conference on Knowledge discovery and data mining, 
  # pp. 206-215. ACM, 2004.
  
  dist = Inf
  
  # Note, we should choose the "best" combination of compression tools and 
  # compression parameters for the data which yield the best compression ratio
  
  for (algo in c("gzip", "bzip2", "xz")) {
    
    d = cdmDistInternal(stringX, stringY, algo)
    
    if (d < dist) {
      dist = d
    }
  }
  
  return(dist)
}

# internal function which computes CDM based on the specified compression algorithm

cdmDistInternal <- function(stringX, stringY, algo) {
  
  #   Input:
  #       algo: compression algo, supported values are "gzip", "bzip2" and "xz"
  #       
  
  strXLen = length(memCompress(stringX, algo))
  
  strYLen = length(memCompress(stringY, algo))
  
  # concatenated string by stringX and stringY
  conStr = c(stringX, stringY)
  
  strConLen = length(memCompress(conStr, algo))
  
  dist = strConLen / (strXLen + strYLen)
  
  return(dist)
}
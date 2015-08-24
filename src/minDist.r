minDist <- function(str1, str2, alphabet_size, compression_ratio) {
  
  # This function computes the minimum (lower-bounding) distance between two strings.  The strings
  # should have equal length.
  #   Input:
  #       str1: first SAX string, in numerical representation, i.e., an integer vector
  #       str2: second SAX string, in numerical representation, i.e., an integer vector
  #       alphabet_size: alphabet size used to construct the strings
  #       compression_ratio: original_data_len / symbolic_len
  #   Output:
  #       dist: lower-bounding distance
  #
  # For details, see below papers
  #
  #   Lin, J., Keogh, E., Lonardi, S. & Chiu, B. 
  #   "A Symbolic Representation of Time Series, with Implications for Streaming Algorithms." 
  #   In proceedings of the 8th ACM SIGMOD Workshop on Research Issues in Data Mining and 
  #   Knowledge Discovery. San Diego, CA. June 13, 2003. 
  #
  #
  #   Lin, J., Keogh, E., Patel, P. & Lonardi, S. 
  #   "Finding Motifs in Time Series". In proceedings of the 2nd Workshop on Temporal Data Mining, 
  #   at the 8th ACM SIGKDD International Conference on Knowledge Discovery and Data Mining. 
  #   Edmonton, Alberta, Canada. July 23-26, 2002    
  
  if (length(str1) != length(str2)) {
    stop('error: the two SAX strings must have equal length!')
  }

  if (any(str1 > alphabet_size) || any(str2 > alphabet_size)) {
    stop('error: some symbol(s) in the string(s) exceed(s) the alphabet size!')
  }
  
  source("./buildDistLookupTable.r")
  
  dist_matrix = buildDistLookupTable(alphabet_size)
  
  dist = sqrt(compression_ratio * sum((diag(dist_matrix[str1, str2])) ^ 2 ))
  
  return(dist)
}


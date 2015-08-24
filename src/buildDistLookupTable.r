buildDistLookupTable <- function(alphabet_size) {

  # Given the alphabet size, this function builds the distance table 
  # for the minimum distances between different symbols
  #
  #   Input:
  #       alphabet_size: alphabet size used to construct the SAX strings
  #       
  #   Output:
  #       dist_matrix: distance matrix
  #
  #   For details, see below paper:
  #   Lin, J., Keogh, E., Lonardi, S. & Chiu, B. 
  #   "A Symbolic Representation of Time Series, with Implications for Streaming Algorithms." 
  #   In proceedings of the 8th ACM SIGMOD Workshop on Research Issues in Data Mining and 
  #   Knowledge Discovery. San Diego, CA. June 13, 2003.
  
  
  # note, we need cast 'alphabet_size' to string for switch to function properly
  switch (toString(alphabet_size),
          "2" =  (cutlines = 0),
          "3" =  (cutlines = c(-0.43, 0.43)),
          "4" =  (cutlines = c(-0.67, 0, 0.67)),
          "5" =  (cutlines = c(-0.84, -0.25, 0.25, 0.84)),
          "6" =  (cutlines = c(-0.97, -0.43, 0, 0.43, 0.97)),
          "7" =  (cutlines = c(-1.07, -0.57, -0.18, 0.18, 0.57, 1.07)),
          "8" =  (cutlines = c(-1.15, -0.67, -0.32, 0, 0.32, 0.67, 1.15)),
          "9" =  (cutlines = c(-1.22, -0.76, -0.43, -0.14, 0.14, 0.43, 0.76, 1.22)),
          "10" =  (cutlines = c(-1.28, -0.84, -0.52, -0.25, 0, 0.25, 0.52, 0.84, 1.28))
  )
  
  dist_matrix = matrix(data = 0, nrow = alphabet_size, ncol = alphabet_size)
  
  for (i in 1 : alphabet_size) {
    # the min_dist for adjacent symbols are 0, so we start with i + 2
    
    if ((i + 2) <= alphabet_size) {
      for (j in (i + 2) : alphabet_size) {
      
        
        idx1 = max(c(i, j)) - 1
        idx2 = min(c(i, j))
        
        dist_matrix[i, j] = (cutlines[idx1] - cutlines[idx2])
      
        # the distance matrix is symmetric
        dist_matrix[j, i] = dist_matrix[i, j]
      }
    }
  }
  
  return(dist_matrix)
}
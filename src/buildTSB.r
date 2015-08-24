buildTSB <- function(sax_words, level, alphabet_size) {
  
  # This function computes a Time Series Bitmap (TSB) from a list of sax words generated from
  # slicing through a long raw time series via a shorter sliding window (either overlapping
  # or non-overlapping, for limnology, it is a non-overlapping window)
  #
  #
  # NOTE: also see the bottom deprecated implementation to see what improvements we made
  #       compared to the original one
  #
  #   Input:
  #       sax_words: a list of sax words, in numerical representation, i.e., an integer vector
  #
  #       level: the length of SAX "subword", the resulting TSB matrix will have
  #             (alphabet_size) ^ level cells, each cell recording frequency for 
  #             one possible SAX "subword"
  #
  #       alphabet_size: alphabet size used to construct the strings
  #       
  #       DEBUG: boolean indicating whether in debug mode (i.e., with more print of 
  #              intermediate results)
  #
  #   Output:
  #       tsb_matrix: TSB matrix
  #
  #
  #   For details, see below papers
  #
  #   Eamonn, N. K. V. N. L., Ratanamahatana, K. S. L. C. A., & Wei, L. (2005). 
  #   Time-series bitmaps: a practical visualization tool for working with large time series 
  #   databases. 
  #
  #   Kasetty, Shashwati, Candice Stafford, Gregory P. Walker, Xiaoyue Wang, and Eamonn Keogh. 
  #   "Real-time classification of streaming sensor data." In Tools with Artificial Intelligence, 
  #   2008. ICTAI'08. 20th IEEE International Conference on, vol. 1, pp. 149-156. IEEE, 2008.
  
  num_cells = alphabet_size ^ level
  
  tsb_vector = vector(mode="numeric", length = num_cells)
  
  # examine frequency for each possible subword by scanning SAX words
  
  # the mapping of letter to integer is: a = 1, b = 2, c = 3, ...,
  
  for (word in sax_words) {
    
    # if the word length is less than the specified subword length, just continue,
    # This may be the case for the last word/subsequence whose length is less than
    # the specified word size
    if (length(word) < level) {
      next
    }
      
    for (i in 1 : (length(word) - level + 1)) {
      
      subword = word[i : (i + level - 1)]
      
      # index in the tsb_vector
      idx = 0
      
      for (j in 1 : level) {
        idx = idx + (subword[j] - 1) * (alphabet_size ^ (j - 1)) 
      }
      
      # index in R starting from 1
      idx = idx + 1
      
      # increment the frequency of the subword by one    
      tsb_vector[idx] = tsb_vector[idx] + 1
    }  

  }

  # normalize the frequencies by dividing the largest value such that pixel values 
  # ranging from 0 to 1
  tsb_vector = tsb_vector / max(tsb_vector)
  
  # convert tsb_vector to a matrix of better shape
  bestShape = findDims(num_cells)
  
  tsb_matrix = matrix(data = tsb_vector, nrow = bestShape$nrow, ncol = bestShape$ncol)
   
  return(tsb_matrix)
}

findDims <- function(vectorLength) {
  
  # find the best shape to transform a vector into a matrix
  
  possibleDivisors = seq(vectorLength)
  
  divisors = possibleDivisors[vectorLength %% possibleDivisors == 0]
  
  smallestDiff = Inf
  
  # find a best matrix shape whose 'abs(nrows - ncols)' is smallest
  
  for (i in 1 : length(divisors)) {
    
    quotient = vectorLength / divisors[i]
    
    diff = abs(quotient - divisors[i])
    
    if (diff < smallestDiff) {
      smallestDiff = diff
      bestShape = list(nrow = quotient, ncol = divisors[i])
    }
    
  }
  
  return(bestShape)
}


########################################################################################

# The following implementation is based on the original paper, however, we found that
# this implementation has the limitation on the shape of the matrix, (i.e., square matrix
# and even # of rows/cols), which is totally unnecessary. So we deprecate this approach

########################################################################################



# buildTSB <- function(sax_words, level, alphabet_size, DEBUG = FALSE) {
#   
#   # This function computes a Time Series Bitmap (TSB) from a list of sax words generated from
#   # slicing through a long raw time series via a shorter sliding window (either overlapping
#   # or non-overlapping, for limnology, it is a non-overlapping window)
#   #
#   #   Input:
#   #       sax_words: a list of sax words, in numerical representation, i.e., an integer vector
#   #
#   #       level: the length of SAX "subword", the resulting TSB matrix will have
#   #             (alphabet_size) ^ level cells, each cell recording frequency for 
#   #             one possible SAX "subword"
#   #
#   #       alphabet_size: alphabet size used to construct the strings
#   #       
#   #       DEBUG: boolean indicating whether in debug mode (i.e., with more print of 
#   #              intermediate results)
#   #
#   #   Output:
#   #       tsb_matrix: TSB matrix
#   #
#   #   For details, see below papers
#   #
#   #   Eamonn, N. K. V. N. L., Ratanamahatana, K. S. L. C. A., & Wei, L. (2005). 
#   #   Time-series bitmaps: a practical visualization tool for working with large time series 
#   #   databases. 
#   #
#   #   Kasetty, Shashwati, Candice Stafford, Gregory P. Walker, Xiaoyue Wang, and Eamonn Keogh. 
#   #   "Real-time classification of streaming sensor data." In Tools with Artificial Intelligence, 
#   #   2008. ICTAI'08. 20th IEEE International Conference on, vol. 1, pp. 149-156. IEEE, 2008.
#   
#   num_cells = alphabet_size ^ level
#   dim = sqrt(num_cells)
#   tsb_matrix = matrix(data = 0, nrow = dim, ncol = dim)
#   
#   # examine frequency for each possible subword by scanning SAX words
#   
#   # the mapping of letter to integer is: a = 1, b = 2, c = 3, ...,
#   
#   for (word in sax_words) {
#     
#     # if the word length is less than the specified subword length, just continue,
#     # This may be the case for the last word/subsequence whose length is less than
#     # the specified word size
#     if (length(word) < level) {
#       next
#     }
#     
#     for (i in 1 : (length(word) - level + 1)) {
#       
#       subword = word[i : (i + level - 1)]
#       
#       # column index of the subword in the TSB matrix
#       col_index = 0
#       
#       for (j in 0 : (level - 1)) {
#         col_index = col_index + ((subword[j + 1] - 1) * (2 ^ (level - j - 1))) %% (2 ^ (level - j)) 
#       }
#       
#       # index in R starting from 1
#       col_index = col_index + 1
#       
#       # row index of the subword in the TSB matrix
#       row_index = 0
#       
#       for (j in 0 : (level - 1)) {
#         row_index = row_index + ((subword[j + 1] - 1) %/% 2) * (2 ^ (level - j - 1)) 
#       }      
#       
#       # index in R starting from 1
#       row_index = row_index + 1
#       
#       if (DEBUG) {
#         print(sprintf("rowidx = %d, colidx = %d", row_index, col_index))
#       }
#       
#       # increment the frequency of the subword by one    
#       tsb_matrix[row_index, col_index] = tsb_matrix[row_index, col_index] + 1
#     }  
#     
#   }
#   
#   if (DEBUG) {
#     message("tsb matrix before normalization")
#     print(tsb_matrix)
#   }
#   
#   # normalize the frequencies by dividing the largest value such that pixel values 
#   # ranging from 0 to 1
#   tsb_matrix = tsb_matrix / max(tsb_matrix)
#   
#   return(tsb_matrix)
# }
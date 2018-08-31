timeseries2Symbol <- function(data, n, alphabet_size) {
  
  # This function transforms a time series subsequence to a symbolic representation
  #
  # Input:
  #   data              is the raw time series as a numerical vector. Practically, it is
  #                     meaningless to convert the whole original time series into a SAX
  #                     string. Instead, we need to chunk it into subsequences first and
  #                     then apply function 'timeseries2Symbol' to each subsequence. So
  #                     here data is a subsequence to be symbolized. See 'timeseries2Subseqs.r'
  #                     upon how to transform original time series into subsequences.
  #
  #   n                 is the number of symbols in the low dimensional approximation of 
  #                     the sub sequence, i.e., the word size
  #
  #   alphabet_size     is the number of discrete symbols. 2 <= alphabet_size <= 20, although 
  #                     alphabet_size = 2 is a special "useless" case.
  #
  # Output:
  #   sax_rep:  a list of two elements, i.e. (list(num_rep = num_rep, str_rep = str_rep)). 
  #             sax_rep[[1]] or sax_rep$num_rep is the numerical representation of SAX
  #             string as an integer vector, sax_rep[[2]] or sax_rep$str_rep is the string 
  #             representation of SAX as a string
  #
  # The variable "win_size" is assigned to N/n, where N = lenght(data). This is the number 
  # of data points on the raw time series that will be mapped to a single symbol, and can 
  # be imagined as the "compression rate".
  #
  # For details, see below papers
  #
  #   Lin, J., Keogh, E., Lonardi, S. & Chiu, B. 
  #   "A Symbolic Representation of Time Series, with Implications for Streaming Algorithms." 
  #   In proceedings of the 8th ACM SIGMOD Workshop on Research Issues in Data Mining and 
  #   Knowledge Discovery. San Diego, CA. June 13, 2003. 
  #
  #   Lin, J., Keogh, E., Patel, P. & Lonardi, S. 
  #   "Finding Motifs in Time Series". In proceedings of the 2nd Workshop on Temporal Data Mining, 
  #   at the 8th ACM SIGKDD International Conference on Knowledge Discovery and Data Mining. 
  #   Edmonton, Alberta, Canada. July 23-26, 2002  
  
  if (alphabet_size > 20) {
    stop('Currently alphabet_size cannot be larger than 20. Please update the breakpoint table if you wish to do so')
  }
  
  if (sd(data, na.rm = TRUE) != 0 && !is.na(sd(data, na.rm = TRUE))){
    # if std != 0, and if entire timeseries != na (check at lease some data exist in this input)
    
    # length of the time series
    N = length(data)
    
    # win_size is the number of data points on the raw time series that will be
    # mapped to a single symbol
    win_size = floor(N/n)                         
    
    # Z normalize it, as it is meaningless to compare ts with different offsets and amplitudes
    data = (data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE) 
    
    if (N <= n){
      # take care of the special case where there is no dimensionality reduction needed
      PAA = data
    }
    else {
      # Convert to PAA.
      
      # N is not dividable by n
      if ((N %% n) != 0) {                               
        temp = matrix(0, nrow = n, ncol = N)
        
        for (i in 1 : n){
          temp[i, ] = data
        }
        
        expanded_data = matrix(temp, 1, N * n)
        
        PAA = colMeans(matrix(expanded_data, N, n), na.rm = TRUE)
        
      } else {
        # N is dividable by n
        PAA = colMeans(matrix(data, win_size, n), na.rm = TRUE)
      }
    }
    
    # Convert the PAA to SAX representation
    sax_rep = paa2SAX(PAA, alphabet_size)        
    
  } else {
    
    # assign all "a" word when entire data was faulty
    str_rep = "a"
    itr = n-1
    for (i in 1 : itr){
      str_rep = paste(str_rep,"a",sep='')
    }
    sax_rep = list(num_rep = array(0,n), str_rep = str_rep)
    
  }
  
  return(sax_rep)                                         
}       

# this internal function defines breakpoints lookup table and maps PAA to a SAX string

paa2SAX <- function(PAA, alphabet_size) {
  
  # numerical representation of the SAX string, as a vector of integers
  num_rep = rep(0, length(PAA))
  
  # note, we need cast 'alphabet_size' to string for switch to function properly
  switch (toString(alphabet_size),
          "1" =  (cut_points = -Inf),
          "2" =  (cut_points = c(-Inf, 0)),
          "3" =  (cut_points = c(-Inf, -0.43,  0.43)),
          "4" =  (cut_points = c(-Inf, -0.67,  0.00,  0.67)),
          "5" =  (cut_points = c(-Inf, -0.84, -0.25,  0.25,  0.84)),
          "6" =  (cut_points = c(-Inf, -0.97, -0.43,  0.00,  0.43,  0.97)),
          "7" =  (cut_points = c(-Inf, -1.07, -0.57, -0.18,  0.18,  0.57,  1.07)),
          "8" =  (cut_points = c(-Inf, -1.15, -0.67, -0.32,  0.00,  0.32,  0.67,  1.15)),
          "9" =  (cut_points = c(-Inf, -1.22, -0.76, -0.43, -0.14,  0.14,  0.43,  0.76,  1.22)),
          "10" = (cut_points = c(-Inf, -1.28, -0.84, -0.52, -0.25,  0.00,  0.25,  0.52,  0.84,  1.28)),
          "11" = (cut_points = c(-Inf, -1.34, -0.91, -0.60, -0.35, -0.11,  0.11,  0.35,  0.60,  0.91, 1.34)),
          "12" = (cut_points = c(-Inf, -1.38, -0.97, -0.67, -0.43, -0.21,  0.00,  0.21,  0.43,  0.67, 0.97, 1.38)),
          "13" = (cut_points = c(-Inf, -1.43, -1.02, -0.74, -0.50, -0.29, -0.10,  0.10,  0.29,  0.50, 0.74, 1.02, 1.43)),
          "14" = (cut_points = c(-Inf, -1.47, -1.07, -0.79, -0.57, -0.37, -0.18,  0.00,  0.18,  0.37, 0.57, 0.79, 1.07, 1.47)),
          "15" = (cut_points = c(-Inf, -1.50, -1.11, -0.84, -0.62, -0.43, -0.25, -0.08,  0.08,  0.25, 0.43, 0.62, 0.84, 1.11, 1.5)),
          "16" = (cut_points = c(-Inf, -1.53, -1.15, -0.89, -0.67, -0.49, -0.32, -0.16,  0.00,  0.16, 0.32, 0.49, 0.67, 0.89, 1.15, 1.53)),
          "17" = (cut_points = c(-Inf, -1.56, -1.19, -0.93, -0.72, -0.54, -0.38, -0.22, -0.07,  0.07, 0.22, 0.38, 0.54, 0.72, 0.93, 1.19, 1.56)),
          "18" = (cut_points = c(-Inf, -1.59, -1.22, -0.97, -0.76, -0.59, -0.43, -0.28, -0.14,  0.00, 0.14, 0.28, 0.43, 0.59, 0.76, 0.97, 1.22, 1.59)),
          "19" = (cut_points = c(-Inf, -1.62, -1.25, -1.00, -0.80, -0.63, -0.48, -0.34, -0.20, -0.07, 0.07, 0.20, 0.34, 0.48, 0.63, 0.80, 1.00, 1.25, 1.62)),
          "20" = (cut_points = c(-Inf, -1.64, -1.28, -1.04, -0.84, -0.67, -0.52, -0.39, -0.25, -0.13, 0.00, 0.13, 0.25, 0.39, 0.52, 0.67, 0.84, 1.04, 1.28, 1.64))
  )   
  
  for (i in 1 : length(PAA)) {
    # order is now: a = 1, b = 2, c = 3, etc
    num_rep[i] = sum(cut_points <= PAA[i], na.rm = TRUE)         
  }
  
  # map number to letter, 'str_rep' is a string representation for sax
  str_rep = lapply(num_rep, num2Letter)
  
  str_rep = paste(str_rep, sep = '', collapse = '')
  
  # return a list
  return(list(num_rep = num_rep, str_rep = str_rep))
}

# this internal function maps a number to a SAX character
num2Letter <- function(num) {
  
  # note, we need cast 'num' to string for switch to function properly
  switch (toString(num),
          "1" = "a",
          "2" = "b",
          "3" = "c",
          "4" = "d",
          "5" = "e",
          "6" = "f",
          "7" = "g",
          "8" = "h",
          "9" = "i",
          "10" = "j",
          "11" = "k",
          "12" = "l",
          "13" = "m",
          "14" = "n",
          "15" = "o",
          "16" = "p",
          "17" = "q",
          "18" = "r",
          "19" = "s",
          "20" = "t",
          "21" = "u",
          "22" = "v",
          "23" = "w",
          "24" = "x",
          "25" = "y",
          "26" = "z"
  )
  
}
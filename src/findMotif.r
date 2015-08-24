findMotif <- function(timeseries, windowSize, wordSize, alphabetSize) {
 
  # This function finds motifs within a time series
  #
  #   Input:
  #       timeseries: a vector representing the raw time series
  #
  #       windowSize: size of the sliding window, non-overlapping, since in limnology
  #                   we use non-overlapping window to chunk data
  #
  #       wordSize: the number of symbols/characters a sliding window will be mapped to
  #
  #       alphabetSize: alphabet size used to construct the SAX string
  #
  #
  #   Output:
  #       frequencyTable: a sorted list which maintains SAX words and associated
  #                       frequencies, in descending order. Each entry within is 'frequencyTable'
  #                       is in turn a list of form
  #                       (num_rep = numerical representation of the sax word, cnt =
  #                       sax word frequency), the name of the entry is str_rep, i.e.,
  #                       the string representation of the sax word, which can be obtained
  #                       by calling "names(frequencyTable)[Index]"
  #
  #   This algorithm is based on the idea in the below paper:
  #
  #   Lonardi, Jessica Lin Eamonn Keogh Stefano, and Pranav Patel. "Finding motifs 
  #   in time series." In Proc. of the 2nd Workshop on Temporal Data Mining, pp. 53-68. 2002. 
  #
  
  source("./timeseries2Subseqs.r")
  
  subsequences = timeseries2Subseqs(timeseries, windowSize, FALSE)

  # require hash package
  require(hash)
  
  frequencyTable = hash()
  
  source("./timeseries2Symbol.r")
  
  for (subseq in subsequences) {
    
    # transform each subsequence into a sax word
    saxword = timeseries2Symbol(subseq, wordSize, alphabetSize)
    
    # use the string representation as the key and update the hash table
    
    if (has.key(saxword$str_rep, frequencyTable)) {
      
      # increment the frequency by one
      new_cnt = (frequencyTable[[saxword$str_rep]])$cnt + 1
      
      frequencyTable[[saxword$str_rep]] = list(num_rep = saxword$num_rep, cnt = new_cnt)
      
    } else {
      
      # initial insertion
      frequencyTable[[saxword$str_rep]] = list(num_rep = saxword$num_rep, cnt = 1)
    }
    
  }
  
  # convert hash table to list
  frequencyTable = as.list(frequencyTable)
  
  # get frequency
  freqs = sapply(frequencyTable, "[[", "cnt")
  
  # get sorted list
  frequencyTable = frequencyTable[order(freqs, decreasing = TRUE)]
  
  return(frequencyTable)
  
}
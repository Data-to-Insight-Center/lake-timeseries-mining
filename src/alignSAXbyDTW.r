alignSAXbyDTW <- function(sax1, sax2, alphabetSize) {

  # This function aligns two time series in SAX representation using Dynamic Time 
  # Warping (DTW). Since it is meaningless to conduct interpolation again SAX strings, 
  # this function requires two sax strings are of the same length
  #
  #   Input:
  #       sax1: first SAX string in numerical representation, i.e., a numerical vector 
  #
  #       sax2: second SAX string in numerical representation, i.e., a numerical vector
  #
  #       alphabetSize: alphabet size used to construct SAX string
  #
  #   Output:
  #       dtwAlignment: a 'dtw' object, the aligned time series can be access in
  #                     the following means:
  #                     
  #                     sax1's aligned value:
  #
  #                         sax1[dtwAlignment$index1]
  #
  #                     similarly, for sax2:
  #
  #                         sax2[dtwAlignment$index2]
  #
  
  # sanity check, two SAX vectors should have the same length, as it is meaningless
  # to conduct interpolation again SAX strings
  
  if (length(sax1) != length(sax2)) {
    stop("Two sax words should have same length")
  }
  
  source("./buildDistLookupTable.r")
  
  # cache the distance matrix in global env
  alignSAXbyDTW.distMatrix <<- buildDistLookupTable(alphabetSize)
  
  dtwAlignment = dtw(sax1, sax2, dist.method = saxCharacterDist)
}



saxCharacterDist <- function(num1, num2) {
  
  # Internal function which calcuate the distance between two SAX characters
  #
  #   Input:
  #       num1: first SAX character/symbol as a number, also serve as the row index
  #             to distanceMatrix
  #
  #       num2: second SAX character/symbol as a number, also serve as the column index
  #             to distanceMatrix
  # 
  #   Output:
  #       dist: distance between the two characters
  
  if (exists("alignSAXbyDTW.distMatrix")) {
    # read distance matrix from cache    
    distanceMatrix = get("alignSAXbyDTW.distMatrix")
  } else {
    
    stop("Cannot find distance matrix var 'alignSAXbyDTW.distMatrix' in global env")
  }
  
  dist = distanceMatrix[num1, num2]
  
  return(dist)
}
alignTSbyDWT <- function(ts1, ts2, doInterpolation = TRUE, 
                         interpolationMethod = "spline") {

  # This function aligns two timeseries using Dynamic Time Warping (DTW). The two
  # time series may have different time resolution and we can do 'alignTSbyInterpolation'
  # as a first phase alignment before invoking DTW. In such means, we are actually
  # doing a two-phase alignment, i.e., interpolation followed by DTW.
  #
  # Note: This function assumes that the timeseries are represented as "xts" object.
  #
  #   Input:
  #       ts1: first timeseries as an "xts" object
  #
  #       ts2: second timeseries as an "xts" object
  #
  #       doInterpolation: whether should first perform interpolation, default
  #                        to TRUE
  #
  #       interpolationMethod: either "linear" or "spline" (default)
  #
  #   Output:
  #       dtwAlignment: a 'dtw' object, the aligned time series can be access in
  #                     the following means:
  #                     
  #                     ts1's aligned time stamp (due to the nature of dtw timestamps
  #                     can be repeated):
  #
  #                         index(ts1)[dtwAlignment$index1]
  #
  #                     ts1's aligned value (can be repeated)
  #                         coredata(ts1)[dtwAlignment$index1, ]
  #
  #                     similarly, for ts2:
  #                         index(ts2)[dtwAlignment$index2], and
  #                         coredata(ts2)[dtwAlignment$index2, ]
  #
  
  if (doInterpolation) {
    source("./alignTSbyInterpolation.r")
    
    # align time through interpolation, so ts1 and ts2 have exactly the same time
    # points, here we remove "NA" values from the result
    
    alingedTS = alignTSbyInterpolation(ts1, ts2, interpolationMethod, TRUE)
    
    # use the new aligned time series
    
    ts1 = alingedTS$ts1
    ts2 = alingedTS$ts2
  }
  
  # Since DTW does not care the actual time points but only the "index", we extract
  # the "value" from "xts" object

  require(xts)
  
  # coredata() returns a matrix, so for single variable timeseries we convert to a 
  # vector
  
  ts1 = as.vector(coredata(ts1))
  ts2 = as.vector(coredata(ts2))
  
  require(dtw)
  
  dtwAlignment = dtw(ts1, ts2)
  
  return(dtwAlignment)
}
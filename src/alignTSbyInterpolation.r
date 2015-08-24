alignTSbyInterpolation <- function(ts1, ts2, method = "spline", removeNA = TRUE) {
  
  # This function aligns two timeseries which have different time resolution (i.e. length)
  # using interpolation (i.e., linear or spline). See also 'alignTSbyDTW.r' for align
  # using Dynamic Time Warping (DTW).
  #
  # Note: This function assumes that the timeseries are represented as "xts" object.
  #
  #   Input:
  #       ts1: first timeseries as an "xts" object
  #
  #       ts2: second timeseries as an "xts" object
  #
  #       method: either "linear" or "spline" (default)
  #
  #       removeNA: whether remove "NA" values from the resulting interpolation,
  #                 default to TRUE
  #
  #   Output:
  #       alignedTS: aligned timeseries, as an "xts" object, first aligned time series 
  #                  can be accessed through "alignedTS$ts1", second through 
  #                  "alignedTS$ts2"

  
  require(xts)
  
  if (!(is(ts1, "xts") && is(ts2, "xts"))) {
    stop("Timeseries should be 'xts' object")
  }
  
  mergedTS = merge(ts1, ts2)
  
  switch (method,
          "linear" =  (alignedTS = na.approx(mergedTS, na.rm = removeNA)),
          "spline" =  (alignedTS = na.spline(mergedTS, na.rm = removeNA))
  )
  
  if (removeNA) {
    # even we specify na.rm to be 'TRUE' when calling 'na.approx'/'na.spline',
    # the results may still have 'NA' values. Hence we need following processing
    # to remove those NAs
    
    alignedTS = alignedTS[complete.cases(alignedTS)]
  } 
  
  return(alignedTS)
  
}
subsetByCommonRange <- function(ts1, ts2) {
  
  ####### ts1 ########
  
  ts1_time = as.numeric(index(ts1))
  
  ts1_min_time = min(ts1_time)
  ts1_max_time = max(ts1_time)
  
  message(sprintf("ts1: min date = [%s], max date = [%s]", 
                  as.POSIXlt(ts1_min_time, tz = "GMT", origin = "1970-01-01"), 
                  as.POSIXlt(ts1_max_time, tz = "GMT", origin = "1970-01-01")))
  
  ####### ts2 ########
  
  ts2_time = as.numeric(index(ts2))
  
  ts2_min_time = min(ts2_time)
  ts2_max_time = max(ts2_time)
  
  message(sprintf("ts2: min date = [%s], max date = [%s]", 
                  as.POSIXlt(ts2_min_time, tz = "GMT", origin = "1970-01-01"), 
                  as.POSIXlt(ts2_max_time, tz = "GMT", origin = "1970-01-01"))) 
  
  ################################
  # ts1 vs. ts2 time range overlap
  ################################
  
  min_time_range = as.POSIXlt(max(ts1_min_time, ts2_min_time), tz = "GMT", origin = "1970-01-01")
  max_time_range = as.POSIXlt(min(ts1_max_time, ts2_max_time), tz = "GMT", origin = "1970-01-01")
  
  time_range = sprintf("%s/%s", min_time_range, max_time_range)
  
  message(sprintf("ts1 and ts2 common range: %s", time_range))
  
  return(list(ts1 = ts1[time_range], ts2 = ts2[time_range])) 

  
}
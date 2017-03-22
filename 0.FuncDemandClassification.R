#
# This function accepts the following arguments:
# dataframe with the following fields:
#   Date, Grade, Caliper, Width, Diam, Wind, Tons
# lead time in days,
# start_date,
# end_date,
# aggreg_period, (one of "daily", "weekly", "monthly")
# dmd_class_params
# 
# Dates should be a string "YYYY-mm-dd", e.g., "2015-01-01"
# 
# If you have other attributes, such as Plant, that you want to include, 
# concatenate them 
# into the Grade field
#
# Sample code to set dmd_class_params list:
# dmd_parameters <- c(nbr_dmd = 5, # Extremely Slow nbr demands
#                     dmd_mean = 10, # Extremely Small demand mean
#                     outlier = 10, 
#                     intermit = 1.9, # Intermittency demand interval
#                     variability = 4, # non-zero dmd std dev
#                     dispersion = 0.49) # CV^2, non inter:smooth
#                     
# (https://learningcenter.llamasoft.com/wp-content/helpfiles/
# SupplyChainGuru/WebHelp/SupplyChainGuru/Using_SafetyStockOptimization/
# Demand_Analysis.htm)
#
demand_class <- function(tsdf, lt_days, start, end, dagg, dparams) {
  
  # splitting puts a '.' between each field, which makes dealing with decimal
  # points tricky.  First change the decimal point in any field that
  # might have one to a '^'
  tsdf[c("Caliper", "Width", "Diam")] <- 
    lapply(tsdf[c("Caliper", "Width", "Diam")], function(x) 
      gsub("\\.", "^", x))
  
  # Split the data frame into a list of data frames, one data frame for each
  # combination of Grade, Caliper, Width, Diam, Wind
  ts_split <- dlply(tsdf, .(Grade, Caliper, Width, Diam, Wind))
  
  # Create a data frame with all the days of the year to be used for 
  # Standard Deviation and safety stock calculations
  tDays <- data.frame(seq.Date(as.Date(start), 
                               as.Date(end), by = "day" ))
  
  colnames(tDays)[1] <- "tDate"
  
  ts_split <- lapply(ts_split, function(x) full_join(tDays, x, 
                                                     by = c("tDate" = "Date")))
  
  ts_split <- lapply(ts_split, function(x) 
    mutate(x, Tons = ifelse(is.na(Tons), 0, Tons)))
  ts_split <- lapply(ts_split, function(x)
    dplyr::select(x, -Grade, -Caliper, -Diam, -Wind, -Width))
  
  # Aggregate the data by dagg
  # Create a new field to hold the date on which we will aggregate the demands
  # If we are "weekly", then this would be the first day of the week starting on 
  # the day where our data starts.  We then aggregate based on the first day 
  # of the week for every date.
  if (dagg == "weekly") {
    first_day <-  as.numeric(format(as.Date(start), "%w"))
    ts_split <- lapply(ts_split, function(x)
      mutate(x, DOW = as.numeric(format(
        as.Date(tDate), "%w"))))
    ts_split <- lapply(ts_split, function(x)
      mutate(x, DOW.Adj = ifelse(DOW < first_day, 7 - first_day + DOW,
                             DOW - first_day)))
    ts_split <- lapply(ts_split, function(x)
      mutate(x, tDate = tDate - DOW.Adj))
    ts_split <- lapply(ts_split, function(x)
      summarize(group_by(x, tDate), Tons = sum(Tons)))
    ts_split <- lapply(ts_split, function(x)
      ungroup(x))
  }
  
  if (dagg == "weekly") {
    lt_fact <- 7
  } else {
    lt_fact <= 1
  }
  
  ts_stats <- lapply(ts_split, function(x) 
    summarize(x, 
              agg_per = dagg,
              dmd_sum = sum(Tons),
              dmd_max = max(Tons), 
              lead_time = lt_days, 
              dmd_mean = mean(Tons),
              dmd_mean_lt = dmd_mean * lt_days/lt_fact,
              dmd_sd = sd(Tons),
              dmd_sd_lt = dmd_sd * sqrt(lt_days / lt_fact),
              nz_dmd_count = nrow(filter(x, Tons > 0)),
              nz_dmd_mean = mean(x$Tons[x$Tons > 0]),
              nz_dmd_mean_lt = nz_dmd_mean * lt_days / lt_fact,
              nz_dmd_sd = sd(x$Tons[x$Tons > 0]),
              nz_dmd_sd_lt = nz_dmd_sd * sqrt(lt_days / lt_fact),
              nz_cv2 = (nz_dmd_sd / nz_dmd_mean)^2,
              nz_cv2_lt = (nz_dmd_sd_lt / nz_dmd_mean_lt)^2,
              nperiods = n(),
              p = n()/nrow(filter(x, Tons > 0))))
  
  rm(ts_split)
  
  # Function to determine demand classification
  dmd_class <- function(x) {
    if (x$nz_dmd_count < dparams["nbr_dmd"]) {
      x$dclass <- "Extremely Slow"
      x$distrib <- "MTO"
    } else { 
      if (x$nz_dmd_mean_lt < dparams["dmd_mean"]) {
        x$dclass <- "Extremely Small"
        x$distrib <- "MTO"
      } else {
        if (x$p < dparams["intermit"]) {
          if (x$nz_cv2_lt < dparams["dispersion"]) {
            x$dclass <- "Non-Intermittent, Smooth"
            x$distrib <- "Normal"
          } else {
            x$dclass <- "Non-Intermittent, Erratic"
            x$distrib <- "Gamma"
          }
        } else {
          if (x$nz_dmd_sd_lt < dparams["variability"]) {
            if (x$nz_cv2_lt < dparams["dispersion"]) {
              x$dclass <- "Intermittent, Low Variable, Slow"
              x$distrib <- "Gamma"
            } else {
              x$dclass <- "Intermittent, Low Variable, Lumpy"
              x$distrib <- "Gamma"
            }
          } else {
            if (x$nz_cv2_lt < dparams["dispersion"]) {
              x$dclass <- "Intermittent, Highly Variable, Slow"
              x$distrib <- "Gamma"
            } else {
              x$dclass <- "Intermittent, Highly Variable, Lumpy"
              x$distrib <- "Gamma"
            }
          }
        }
      }
    }
    return(x)
  }
  
  # Compute the demand classification
  ts_stats <- lapply(ts_stats, dmd_class)
  
  # Convert back into a data frame
  ts_stats_df <- as.data.frame(do.call(rbind, ts_stats))
  ts_stats_df <- tibble::rownames_to_column(ts_stats_df)
  # convert the column lists
  #ts_stats_df <- unnest(ts_stats_df)
  ts_stats_df <- ts_stats_df %>%
    separate(rowname, c("Grade", "Caliper", "Width", "Diam", "Wind"), 
             sep = "\\.")
  
  ts_stats_df[c("Caliper", "Width", "Diam")] <- 
    lapply(ts_stats_df[c("Caliper", "Width", "Diam")], function(x) 
      gsub("\\^", "\\.", x))
  
  return(ts_stats_df)
}

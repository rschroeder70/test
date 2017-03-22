# Functions

# Function to fill dates of of a sparse time series date dataframe
pad_dates <- function(x) {
  full_join(tDays, x, by = c("tDate" = "Date"))
}

# Function to fill in missng column data after padding with dates
fill_dates <- function(x, colName) {
  x[[colName]] <- na.locf(x[[colName]], na.rm = FALSE)
  x[[colName]] <- na.locf(x[[colName]], na.rm = FALSE, fromLast = TRUE)
  return(x)
}

# Function to copy data onto the clipboard
#   use for pasting the production orders into SAP
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = F, 
              col.names = T,
              sep = '\t', quote = F,
              append = F)
  close(f)  
}

# Paste data into R from the clipboard
paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}

# Run parent roll selection-------------------------------------------------
# 
# For each grade/CalDW combination, determine the lowest trim cost
# for each possible numer of parent rolls, constrained by the various
# sheeter and cutter trim limits.
#
# Here is a small example that you can run through by selecting the lines
# starting with a '-' to load the data and then running the functions 
# in the body of the file
# 
# Assume we have 3 rolls, 1 = 20", 2 = 30" & 3 = 41", trim_cost = 1, 
#  demands = 1 and we want a solution with 2 parent rolls
# 
# -pwidth <- data.frame(width = c(20, 30, 41), dmd_lt = c(10, 8, 20), 
#             sd_lt = c(5, 6, 7))
#           
# -rwidth <- c(20, 30, 41)
# -trim_cost <- 1
# -bigM <- 50
#  
# f_rp = 
#       P 
#         1       2       3
#       --------------------
# R  1 |  1
#    2 |  1.5     1 
#    3 |  1.05    1.37    1
# 
# Assume we just want to minimize the trim tons (trim cost = 1.0)
# c_rp = 
#       P 
#         1       2       3
#       --------------------
# R  1 |  0
#    2 |  0.5     0 
#    3 |  0.05    0.37    0
# 
# obj = c_rp * demand_p
# obj: 0*x_1,1 + 0*x_1,2 + 0*x_1,3 + 5*x_2,1 + 0*x_2,2 + 0*x_2,3 +
#         0.5*x_3,1 + 2.933*x_3,2 + 0*x_3,3 + 0*z_1 + 0*z_2 + 0*z_3
# 
# mat:                                       rhs
#      1  0  0  1  0  0  1  0  0  0  0  0  =  1
#      0  0  0  0  1  0  0  1  0  0  0  0  =  1
#      0  0  0  0  0  0  0  0  1  0  0  0  =  1
#     -1  0  0  0  0  0  0  0  0  50 0  0  >= 0
#      0  0  0 -1 -1  0  0  0  0  0  50 0  >= 0
#      0  0  0  0  0  0 -1 -1 -1  0  0 50  >= 0
#      0  0  0  0  0  0  0  0  0  1  1  1  =  2
#      1  0  0  0  0  0  0  0  0  -1 0  0  =  0
#      0  0  0  0  1  0  0  0  0  0  -1 0  =  0
#      0  0  0  0  0  0  0  0  1  0  0  -1 =  0
#      
# 
# Solution: Obj = 0.5
# -matrix(sol$solution, 4, 3, byrow = T)
# 
#      [,1] [,2] [,3]
# [1,]    0    0    0  - no parent
# [2,]    0    1    0  - parent for product 2
# [3,]    1    0    1  - parent for products 1 and 3
# [4,]    0    1    1  - use parents 2 & 3 for a total of 2
#
#   
# This is called once for each grade/CalDW (parent) type by the
# 4.Parent Roll Selection.R script
#  
# define number of potential parent rolls and products
# rwidth and pwidth are defined in the 3.Read Data.R script
# 
# number of parents, 1...R
numr <- length(rwidth) 
# number of products, 1...P
nump <- nrow(pwidth) 

# Set up all the LP components

# Create f_rp and c_rp as numr x nump matrix
# f_rp = number of tons of roll r required to make one ton of product p 
#      = 1 + mod(rwidth,pwidth)/pwidth if > 1 pocket
#      = 1 + (rwidth - pwidth)/pwidth if 1 pocket 
f <- matrix(data=0, nrow=numr, ncol=nump)
# trim loss cost c_rp associated with production of p from r 
# cost to ship 1 ton of trim back to the closest mill
c <- matrix(data=0, nrow=numr, ncol=nump)
# fill the matrices
for (r in 1:numr){
  for (p in 1:nump){
    if (pwidth$width[p] <= rwidth[r]){
      if(pwidth$width[p] %in% slitter_limits_list == FALSE) {# Multi Pocket 
        f[r,p] <- 1 + ((rwidth[r] %% pwidth$width[p]) / pwidth$width[p])
      } else {  # Cannot split rolls
        f[r,p] <- 1 + ((rwidth[r] - pwidth$width[p]) / pwidth$width[p])
      }
      c[r,p] <- (f[r,p] - 1) #* trim_cost -- assume cost is 1, minimize tons
    }
  }
}

# Variables:
# x_1,1....x_1,P x_2,1...x_2_P.....x_R,1...x_R,P z1...zR
# x_r,p is the decision variable, constrained to be 0 or 1
# x_r,p --> if 1 the product p is assigned to parent r, 0 otherwise 
# zr --> if 1 then roll r is a parent roll (binary) 

# Create the objective function vector.  This needs to include 0's
# for the z variables that will show up in the constraint matrix.
# Basically take the c_rp cost matrix and turn it into a vector
obj <- rep_len(c(0), numr * nump + numr)
for (r in 1:numr){
  # index for row (parent roll) to update
  rstartrow <- (r-1)*nump
  for (p in 1:nump) { 
    #check if product p can be made by parent r 
    if (pwidth$width[p] > rwidth[r]) {next}
    #total trim cost making p with r 
    trimtons <- c[r,p] * pwidth$dmd_sum[p] 
    #update obj function coefficient matrix
    obj[rstartrow+p] <- trimtons
  }
}
# To view the objective array as a matrix, use 
# matrix(obj, nrow = numr + 1, byrow = TRUE)

# create empty coefficient matrix for constraints
# columns are x_1,1..x_1,P x_2,1...x_2,R....x_R,1...x_R,P z1...zR
# The first R rows are based on roll and parent sizes
# and are used to set just one parent roll per product
# The next R rows are for the constraint to determine
# if the roll is used as a parent 
# The next row is for the z count constraints and the last
# R rows ensure that if a roll is selected as a parent, it 
# is not allowed to be the child of another parent.
mat <- matrix(data = 0, nrow = nump + numr + 1 + numr, 
              ncol = numr * nump + numr)

# constraints 1...P
# constraint that there is only one parent roll per product
# for p in PRODUCTS, sum(x_rp) over r in Rp = 1 
# We are also accounting for infeasible sizes by setting
# the coefficient to zero in those cases where the product
# is bigger than the parent
for (p in 1:nump){
  for (r in 1:numr){
    #check if product p can be made from parent r 
    if (rwidth[r] < pwidth$width[p]) {next}
    #index for what column (product) to update
    rCol <- p + numr * (r - 1)
    #set coefficient for x_r,p to 1 
    mat[p, rCol] <- 1 
  }
}

# Are there any product in the prod_fixed table
# If so,  make the coefficients for all the other sizes
# equal to zero.  Since the sum equals 1, this will ensure that
# we keep that roll as a parent to itself.  
# TODO:  adjust the coefficients.
if (one_parent_mill %in% prod_fixed$Mill &
  one_parent_grade %in% prod_fixed$Grade &
    one_parent_calDW %in% prod_fixed$CalDW) {
  # Get the rows that match the one_parent
  fxd <- filter(prod_fixed, Mill == one_parent_mill,
         Grade == one_parent_grade,
         CalDW == one_parent_calDW)
  for (ifx in 1:nrow(fxd)) {
    idx <- match(fxd$Width[ifx], pwidth$width)
    mat[idx, ] <- 0  # first make the whole row zero
    mat[idx, (nump * (idx - 1) + idx)] <- 1
  }
}
  

# constraints P+1...P+R
# constraint that determines if roll r was used
# Use the bigM value to force the value to be non-zero
# if we use this roll.  We will define it as binary below
# sum(-x_r,p)+bigM*zr >= 0 
for (r in 1:numr){
  for(p in 1:nump){
    #check if product p can be made from r 
    if(pwidth$width[p] > rwidth[r]) {next}
    #index for what column to update
    pCol <- (r - 1) * nump + p 
    #udpate constraint matrix 
    #set coefficient for zr to bigM
    #bigM defined in User Inputs.R
    mat[(nump + r),(nump * numr + r)] <- bigM
    #set coefficient for x_r,p to -1
    mat[(nump + r), pCol] <- -1 
  }
}

# constraint P+R+1
# constraint limiting selected number of parent rolls
# sum(zr) over r in R = number of parent rolls 
for (r in 1:numr) { 
  #set coefficient of zr to 1 
  mat[(nump + numr + 1), numr * nump + r]<- 1
}

# Constraints P+R+2..P+R+2+R
# Constraint to ensure that a parent roll, if selected
# is assigned to itself.
# s.t. {r in ROLLS}: x[r,r] - z[r] = 0;
for (r in 1:numr) {
  mat[nump + numr + 1 + r, r + numr*(r-1)] <- 1
  mat[nump + numr + 1 + r, nump * numr + r] <- -1
}

# The following constraints force x_r,p to zero by assigning it's
# coefficient to 1 and then requiring the sum to be zero.  I think we
# could do this in the first P rows by setting the x_r,p we want out
# to a zero because we already require the sum to be 1 to force
# the product to come from only 1 parent.

# constraints to limit trim size
# will force x_r,p to 0 if p is smaller than r by more than trimLimit
# will add row in matrix if constraint is necessary
# add a new row for a constraint
mat <- rbind(mat, c(rep_len(0, numr * nump + numr)))
# index for what row to update
rowNum <- nrow(mat)
for (r in 1:numr){
  for (p in 1:nump) {
    #check if product p can be made from r
    if (pwidth$width[p] > rwidth[r]) {next}
    #check if trim exceeds trim limit
    #trimLimit defined in User Inputs.R
    if (rwidth[r] %% pwidth$width[p] <= trimLimit) {next}
    #update constraint matrix
    #set coefficient of x_r,p to 1 s.t. 1*x_r,p = 0
    mat[rowNum, nump * (r - 1) + p] <- 1
  }
}

  # Constraint to adhere to cutter sizes in converting plants
  # will force x_r,p to 0 if p cannot be cut from r because of a cutter size
  # In other words, a product cannot be cut from a roll that is larger than the
  #   next larger cutter width.
  # will add row in matrix if constraint is necessary 
  # cutters list defined in User Inputs.R, list of sizes in inches
  # add new row in constraint matrix
if (use_cutter_limits) {
  mat <- rbind(mat, c(rep_len(0, numr * nump + numr)))
  # index for what row to update
  rowNum <- nrow(mat)
  for (cutter in cutters) {
    for (r in 1:numr){
      #check that parent is above cutter size
      if (rwidth[r]<=cutter) {next}
      #check that parent is within trimLimit of cutter size
      #trimLimit defined in User Inputs.R, same as constraint above
      if (rwidth[r] > cutter + trimLimit) {next}
      for (p in 1:nump){
        #check if product p can be made from parent r
        if (pwidth$width[p] > rwidth[r]) {next}
        #check if product p is within trimLimit of cutter size
        #same trimLimit as above, defined in User Inputs.R
        if (pwidth$width[p] <= cutter - trimLimit) {next}
        #check that product p is smaller than cutter
        if (pwidth$width[p] > cutter) {next}
        #set coefficient of x_r,p to 1
        mat[rowNum, nump * (r - 1) + p] <- 1
      }
    }
  }
}

# constraint to adhere to slitter capabilities in converting plants
# slitter_limits_list created in Data Read In.R 
# will force x_r,p to 0 if p cannot be slit from r 
# will add new row to constraint matrix 
# Note: this will create a row even if not needed
# add new row to constraint matrix
mat <- rbind(mat, c(rep_len(0, numr * nump + numr)))
# index for what row to update
rowNum <- nrow(mat)
for (r in 1:numr){
  for (p in 1:nump){
    # check that product p can be made from parent r
    if(pwidth$width[p]>rwidth[r]) {next}
    # check that product p can be double cut from parent r
    if(rwidth[r]/pwidth$width[p] < 2) {next}
    # check if product is part of list that doesn't allow slitting
    if(pwidth$width[p] %in% slitter_limits_list == FALSE) {next}
    # it can, set coefficient of x_r,p to 1
    mat[rowNum, nump * (r - 1) + p] <- 1
  }
}

#counts the number of trimLimit, cutter, and slitter constraints
numLastConstraint <- nrow(mat) - (nump + numr + numr + 1)

#define constraint operators 
dir <- c(rep_len("==", nump), rep_len(">=", numr), "==",
         rep_len("==", numr), rep_len("==", numLastConstraint))

#define variable types 
#x_1,1...x_R,P are binary, z1..zR are binary, y1...yR are integer
# In theory we should be able to make the first numr*nump constraints >= 0, but 
# we get non integer solutions (floating point stuff?)
types<-c(rep_len("B", numr * nump), rep_len("B", numr))
#define objective type (TRUE=maximum, FALSE=minimum)
max <- FALSE

# define right hand side of constraint equations.  Only the "number
# of rolls" constraint needs to be updated as we iterate through the number of
# possible number of parent rolls
# constraints 1..R = 0, R+1..R+P = 1, R+P+1...R+P+R >= 0,  
# R+P+R+1 = 0, remaining = 0 

# start with a big number, and We will adjust numparents in the loop
numparents <- 999

rhs <- c(rep_len(1,nump), rep_len(0,numr),numparents, 
         rep_len(0, numr), rep_len(0,numLastConstraint))


# Create an empty list to store the results of each possible number of sub
# rolls
RollSelOptions <- list()

# create counter for possible scenarios
scenario <- 0 
# run selection optimization problem for 1 roll through all rolls
prev_time <- Sys.time()
for (r in 1:numr){
  #define number of parents 
  numparents <- r
  print(Sys.time() - prev_time)
  prev_time = Sys.time()
  print(paste("   Solving for Nbr Parents = ", r, "/", numr, sep = ""),
        quote = FALSE)
  # UPdate the rhs for this parent
  rhs[nump + numr + 1] <- numparents
  
  # solve optimization problem 
  sol <- NULL
  sol <- Rsymphony_solve_LP(obj, mat, dir, rhs, 
                                      types = types, max=max,
                                      verbosity = -2)
  #define assignment matrix 
  #assignment <- matrix (sol$solution, nrow=nump)
  # if solution is impossible the sum of assignment == 0
  #if(sum(assignment)==0) {next}
  # Use the status to check if we got a feasible solution
  # NOTE:  This doesn't originally work, but seems to now
  # with the new release of RStudio
  if (sol$status != 0) {next}   
  # if solution is possible, add to scenario count
  scenario <- scenario + 1 

  # Calculate the safety stock for this selection of parent rolls
  # 
  # First put the assignment matrix in the standard form where 
  #   x_rp = 1 if p comes from r
  assignment <- matrix(sol$solution, nrow = numr + 1, byrow = TRUE)
  # delete the last row of the matrix (the z_i)
  # keep even single items a matrix
  assignment <- as.matrix(assignment[-(numr + 1), ])
  
  # Compute parent roll statistics via the assignment table and
  # some matrix math.  Note that in some cases we need to account for
  # the trim loss (via the f matrix) when computing values for the parent 
  # rolls
  # Parent roll Variance: Var_r = sum_p in r (f_rp^2 * sd_p^2 * x_rp)
  #parentss <- sfty_factor * sqrt((f^2 * assignment) %*% (pwidth$sd_lt^2))
  ## ss_cost <- sum(parentss) * ihc_per_day[str_sub(ifac, 1, 1)] * roll_lead_time
  #ss_cost <- sum(parentss) * ihc_per_year[[str_sub(ifac, 1, 1)]]
  #parent_dmd_count <- assignment %*% pwidth$dmd_count
  #parent_dmd_lt <- (f * assignment) %*% pwidth$dmd_lt
  #parent_dmd_review <- (f * assignment) %*% pwidth$dmd_review
  #parent_dmd_sum <- (f * assignment) %*% pwidth$dmd_sum
  
  # add solutions to list of roll selection options
  # for each product we need to know which roll it comes from
  # loop over each product p
  datalist <- list()
  for(prd in 1:nump){
    temp_row <- list()
    #browser()  # debug command
    temp_row$Prod.Width <- pwidth$width[prd]
    temp_row$Prod.DMD_LT <- pwidth$dmd_lt[prd]
    temp_row$Prod.DMD <- pwidth$dmd_sum[prd]
    # set number of parents 
    temp_row$Sol.Num.Parents <- numparents
    #temp_row$Sol.DMD <- sum(parent_dmd_sum)
    # set trim tons to objective value 
    ##temp_row$Sol.LT.Trim.Cost <- sol$objval
    temp_row$Sol.Trim.Tons <- sol$objval
    ##temp_row$Sol.LT.Trim.Tons <- sol$objval / trim_cost
    #temp_row$Sol.SS.Cost <- ss_cost
    #temp_row$Sol.SS.Tons <- 
    #  ss_cost / (ihc_per_year[[str_sub(ifac, 1, 1)]])
    ##temp_row$Sol.LT.TotalCost <- temp_row$Sol.LT.Trim.Cost +
    ##  temp_row$Sol.LT.SS.Cost

    # for this product (column), what is the parent (row)
    for (x in 1:numr){ 
      # if assignment (x_r,p) is 1 then save parent index number
      parent_roll_index <- if (assignment[x,prd] == 1) x else next
    }
    # set parent of product
    temp_row$Parent.Width <- rwidth[parent_roll_index]
    #temp_row$Parent.DMD.Count <- parent_dmd_count[parent_roll_index]
    #temp_row$Parent.DMD.LT <- parent_dmd_lt[parent_roll_index]
    #temp_row$Parent.DMD <- parent_dmd_sum[parent_roll_index]
    #temp_row$Parent.SS.Tons <- parentss[parent_roll_index]
    #temp_row$Parent.SS.DOH <-  parentss[parent_roll_index] /
    #  (parent_dmd_sum[parent_roll_index]/as.numeric(num_days))
    #temp_row$Parent.OTL <- temp_row$Parent.SS.Tons + 
    #  temp_row$Parent.DMD.LT + parent_dmd_review[parent_roll_index]
    temp_row$Prod.Trim.Width <- rwidth[parent_roll_index] %% 
      pwidth$width[prd]
    temp_row$Prod.Trim.Fact <- f[parent_roll_index, prd]
    # Get the total trim cost for this product accounting for the
    #   freight back from plant to this mill
    temp_row$Prod.Trim.Tons <- 
      (f[parent_roll_index, prd] - 1) * pwidth$dmd_sum[prd] 
    temp_row$Prod.Trim.Freight <- temp_row$Prod.Trim.Tons *
      pwidth$Trim.Frt.Cost[prd]
    temp_row$Prod.DMD.Tons <- pwidth$dmd_sum[prd]
    temp_row$Prod.Gross.Tons <- 
      f[parent_roll_index, prd] * pwidth$dmd_sum[prd]
    temp_row$Prod.Handling.Cost <- 
      temp_row$Prod.Gross.Tons * handling[[str_sub(ifac, 1, 1)]]
    temp_row$Prod.LT.Tons <- 
      f[parent_roll_index, prd] * pwidth$dmd_lt[prd]
    temp_row$Prod.DMD.Count <- pwidth$dmd_count[prd]
    #temp_row$Prod.SS.Tons <- sfty_factor * pwidth$sd_lt[prd]
    #temp_row$Prod.SS.DOH <- temp_row$Prod.SS.Tons / 
    #  (temp_row$Prod.SUM.Tons/as.numeric(num_days))
    temp_row$Prod.dclass <- pwidth$dclass[prd]
    
    datalist[[prd]] <- temp_row
  }
  RollSelOptions <- bind_rows(RollSelOptions, datalist)
}

# Count the number of sub rolls for each parent
RollSelOptions <- RollSelOptions %>%
  group_by(Sol.Num.Parents, Parent.Width) %>% 
  mutate(Parent.Nbr.Subs = n()) %>% ungroup()

# Compute the solution trim cost.  We cando this by summing the info
# for the products. The inventory costs have to be done at the parent
# level after this.
RollSelOptions <- RollSelOptions %>%
  group_by(Sol.Num.Parents) %>%
  mutate(Sol.Trim.Freight = sum(Prod.Trim.Freight),
         Sol.Handling.Cost = sum(Prod.Handling.Cost))

#add information about grade and caliper 
RollSelOptions$Grade <- one_parent_grade
RollSelOptions$CalDW <- one_parent_calDW
RollSelOptions$Mill <- ifac

RollSelOptions <- ungroup(RollSelOptions)

# Calculate the safety stock for each solution based on 
# aggregating the sub rolls for each parent in the solution
# Pick up the Parent stats along the way.  
# Isolate the unique parents
# and just do the calculations for them as the same parents show
# up multiple times for different Sol.Num.Parents

# for each parent width and number of subs, identify the sub widths
parent_options <- RollSelOptions %>% 
  select(Parent.Width, Parent.Nbr.Subs, Prod.Width, 
         Prod.Trim.Fact) %>%
  arrange(Parent.Width, Parent.Nbr.Subs, Prod.Width) %>%
  distinct() %>%
  ungroup()

# Create this to join back to the parents later
# It creates a field listing all the subs for each parent/nbr subs
# For some reason, this next command wasn't working until removed the
# Prod.Trim.Fact field
parent_subs <- select(parent_options, -Prod.Trim.Fact) %>%
  group_by(Parent.Width, Parent.Nbr.Subs) %>%
  mutate(Sub.Widths = paste0(Prod.Width, collapse = ", ")) %>%
  select(-Prod.Width) %>%
  ungroup() %>%
  mutate(Parent.Width = as.character(Parent.Width),
                  Parent.Nbr.Subs = as.character(Parent.Nbr.Subs)) %>%
  distinct()

# Summarize the rawData over the plants
raw_data_np <- rawData %>% 
  filter(Grade == one_parent_grade & CalDW == one_parent_calDW &
           Year == mYear & Mill == ifac) %>%
  group_by(Year, Date, Mill, Grade, CalDW, Width) %>%
  summarize(Tons = sum(Tons)) %>%
  ungroup()

# identify the parent and nbr subs that each product would come from 
rd_parents <- inner_join(raw_data_np, parent_options,
                     by = c("Width" = "Prod.Width"))
rm(raw_data_np)

# Multiply the Tons by the trim factor to get the gross tons for each
# product that will get accumulated to each parent
rd_parents <- rd_parents %>%
  mutate(Prod.GR.Tons = Tons * Prod.Trim.Fact)

# Aggregate by the parents/nbr subs (if there are demands on the same date)
# The Tons are the tons of each product that make up the parent
rd_parents <- rd_parents %>%
  group_by(Year, Date, Mill, Grade, CalDW, Parent.Width, Parent.Nbr.Subs) %>%
  summarize(Tons = sum(Prod.GR.Tons))

# Combine the Parent.Width and Parent.Nbr.Subs to create unique item field
rd_parents <- rd_parents %>%
  unite(Width, Parent.Width, Parent.Nbr.Subs, sep = "_")

# Pass this to the demand classification logic
# First separate the CalDW field
parent_stats <- rd_parents %>% 
  separate(CalDW, c("Caliper", "Diam", "Wind"), sep = "-") %>%
  ungroup()

# start_date and end_date come from the 3.5.Demand Profiler.R script
parent_stats <- demand_class(parent_stats, roll_lead_time,
                             start_date, end_date,
                             dmd_aggreg_period, dmd_parameters)

# Add the facility and the cycle DBR
parent_stats <- parent_stats %>%
  mutate(Fac = ifac,
         cycle_dbr = cycle_dbr) %>%
  select(Fac, everything())

# Unite the CalDW field
parent_stats <- parent_stats %>%
  unite(CalDW, Caliper, Diam, Wind, sep = "-") %>%
  separate(Width, c("Parent.Width", "Parent.Nbr.Subs"), sep = "_")

# Compute the distribution parameters via a function.  
# Also compute the stats over the lead time here.  
calc_params <- function(x) {
  y <- list()
  if (use_gamma & x$distrib == "Gamma") {
    y$shape <- (x$dmd_mean_lt) ^ 2 / (x$dmd_sd_lt) ^ 2
    y$scale <- (x$dmd_sd_lt) ^ 2 / x$dmd_mean_lt
    y$rop <- qgamma(service_level,
                    y$shape,
                    scale = y$scale,
                    lower.tail = TRUE)
    y$sfty_stock <- y$rop - x$dmd_mean_lt
    y$act_distrib <- "Gamma"
    
  } else {
    y$shape <- 0
    y$scale <- 0
    y$sfty_stock <- x$dmd_sd_lt * sfty_factor
    y$rop <- y$sfty_stock + x$dmd_mean_lt
    y$act_distrib <- "Normal"
  }
  return(as.data.frame(y))
}

# replace t with parent_stats after debugging
parent_stats <- adply(parent_stats, 1, calc_params)

# Rename some columns and compute the safetty stock cost for each parent
parent_stats <- parent_stats %>%
  rename(Parent.DMD.Count = nz_dmd_count,
         Parent.DMD = dmd_sum,
         Parent.DMD.Mean = dmd_mean, 
         Parent.DMD.LT = dmd_mean_lt,
         Parent.SS.Tons = sfty_stock,
         Parent.dclass = dclass,
         Parent.Distrib = distrib,
         Parent.SS_Calc = act_distrib,
         Parent.ROP = rop)

# Calculate the costs for the parent rolls
# Exp.OH = safety stock + DR/2
parent_stats <- parent_stats %>%
  mutate(Parent.SS.Cost = Parent.SS.Tons * ihc_per_year,
         Parent.SS.DOH = Parent.SS.Tons / 
           (Parent.DMD / as.numeric(num_days)),
         Parent.OTL = Parent.SS.Tons +
           Parent.DMD.LT + Parent.DMD.Mean * cycle_dbr / dmd_lt_conv,
         Parent.Exp.OH.Tons = Parent.SS.Tons + 
           Parent.DMD.Mean * (cycle_dbr / dmd_lt_conv) / 2,
         Parent.Exp.OH.Storage.Cost = Parent.Exp.OH.Tons *
           storage[[str_sub(ifac, 1, 1)]],
         Parent.Exp.OH.Inv.Cost = Parent.Exp.OH.Tons * 
           ihc_per_year,
         Parent.Exp.OH.DOH = Parent.Exp.OH.Tons /
           (Parent.DMD / as.numeric(num_days)))

# Just use a subset of the fields to join back to the RollSelOptions table
parent_stats_final <- parent_stats %>%
  select(Parent.Width, Parent.Nbr.Subs, Parent.DMD.Count,
         Parent.DMD, Parent.SS.Tons, Parent.SS.Cost, 
         Parent.SS.DOH, Parent.OTL, Parent.Exp.OH.Tons,
         Parent.Exp.OH.Storage.Cost,
         Parent.Exp.OH.Inv.Cost, Parent.Exp.OH.DOH, Parent.dclass,
         Parent.Distrib, Parent.SS_Calc)

# Add the list of sub rolls so we can see what's in a parent
parent_stats_final <- inner_join(parent_stats_final, parent_subs,
                           by = c("Parent.Width" = "Parent.Width",
                                  "Parent.Nbr.Subs" = "Parent.Nbr.Subs"))

# Now with the parent info, we can calculate the Sol results for
# the safety stock (we have the trim already)

# Join the parent stats back to the RollSelOptions table
RollSelOptions$Parent.Width <- as.character(RollSelOptions$Parent.Width)
RollSelOptions$Parent.Nbr.Subs <- 
  as.character(RollSelOptions$Parent.Nbr.Subs)

RollSelOptions <- 
  left_join(RollSelOptions, parent_stats_final,
            by = c("Parent.Width" = "Parent.Width",
                   "Parent.Nbr.Subs" = "Parent.Nbr.Subs"))

# Trim  & handling tons and cost for each parent
RollSelOptions <- RollSelOptions %>%
  group_by(Parent.Width, Sol.Num.Parents) %>%
  mutate(Parent.Trim.Tons = sum(Prod.Trim.Tons),
         Parent.Trim.Cost = sum(Prod.Trim.Freight),
         parent.Handling.Cost = sum(Prod.Handling.Cost))

# Compute total tons for each solution including trim loss
RollSelOptions <- RollSelOptions %>%
  group_by(Sol.Num.Parents) %>%
  mutate(Sol.Gross.Tons = sum(Prod.Gross.Tons)) %>%
  ungroup()

# Compute the safety stock tons and expected average OH for each solution by 
# aggregating over the parents in a sub query 
sol_inv_stats <- RollSelOptions %>%
  select(Sol.Num.Parents, Parent.Width, Parent.SS.Tons, Parent.SS.Cost,
         Parent.OTL, Parent.Exp.OH.Tons, Parent.Exp.OH.Inv.Cost,
         Parent.Exp.OH.Storage.Cost) %>%
  distinct() %>% 
  group_by(Sol.Num.Parents) %>%
  summarize(Sol.SS.Tons = sum(Parent.SS.Tons),
            Sol.SS.Cost = sum(Parent.SS.Cost),
            Sol.OTL.Tons = sum(Parent.OTL),
            Sol.Exp.OH.Storage.Cost = sum(Parent.Exp.OH.Storage.Cost),
            Sol.Exp.OH.Tons = sum(Parent.Exp.OH.Tons),
            Sol.Exp.OH.Inv.Cost = sum(Parent.Exp.OH.Inv.Cost))

RollSelOptions <- full_join(RollSelOptions, sol_inv_stats,
                             by = c("Sol.Num.Parents" = "Sol.Num.Parents"))
rm(sol_inv_stats)

# The trim freight cost includes the $150 opportunity
RollSelOptions <- RollSelOptions %>%
  mutate(
    Sol.Trim.Cost = Sol.Trim.Freight,
    Sol.Total.Cost = Sol.Trim.Cost + Sol.Handling.Cost +
      Sol.Exp.OH.Storage.Cost + Sol.Exp.OH.Inv.Cost)


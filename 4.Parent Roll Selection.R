#
# Perform the parant roll selection optimization
# 
print("Seleting Optimal Parent Rolls for each Possible Number of Parents")

# Test this with a specific grade and calipeer
#rawData <- filter(rawData, Grade == "SUS" & CalDW == "26-72-W")

# select all the unique parents
parent_types <- rawData %>% group_by(Grade, CalDW, Mill) %>%
  summarize(Sizes = n_distinct(Width))

# create data frame to hold slitter limitation information
allNoSlitter <- data.frame("Grade"=c(0),"CalDW"=c(0),"Width"=c(0),
                           "Plant"=c(0))
# iterate through each plant with slitter limiation
# slitterLimitPlants defined in User Inputs.R 
for (tplant in slitterLimitPlants){
  # selects data from specified plant with slitter limitation
  # select only Grade, CalDW, Width, Plant
  noSlitter <- rawData %>% select (Grade, CalDW, Width, Plant)
  # select data only from Plant that has slitter limitation
  noSlitter <- noSlitter %>% filter (Plant == tplant)
  # select only the distinct Grade, CalDW, Widths from that plant
  noSlitter <- noSlitter %>% distinct(Grade, CalDW, Width, Plant)
  # append to larger data frame
  allNoSlitter <- rbind(allNoSlitter,noSlitter)
}
# remove first row of zeros
allNoSlitter <- allNoSlitter[-c(1),]

# create empty data frame to hold options for all grade, CalDW combinations
AllRollSelOptions <- data.frame(list())

parent_stats_all <- list()

# specify number of cores to use for parallel processing
#registerDoParallel(cores=3)
#print("Par Registered")

# Run optimization to get the cost of each possible number of rolls
# for each Grade/CalDW combination
print("Selecting Parent Roll Assignments")
ptype <- 2 # For testing
for (ptype in 1:nrow(parent_types)) {

  # filter to find data for one parent
  one_parent_grade <- parent_types$Grade[ptype]  # Grade
  one_parent_calDW <- parent_types$CalDW[ptype] # CalDW
  one_parent_mill <- parent_types$Mill[ptype]

  print(paste("Parent ", ptype, "/", nrow(parent_types), ", Grade = ", 
              one_parent_grade, ", CalDW = ",
              one_parent_calDW, ", ", parent_types$Sizes[ptype], 
              " Sizes", sep = ""))
  
  # Get the lead time for this grade/CalDW.  Note we use the same logic
  # in the simulation file.
  cycle_dbr <- cycle_length_list$Mean.DBR[which(
    cycle_length_list$Mill.Machine == one_parent_mill &
      cycle_length_list$Grade == one_parent_grade &
      cycle_length_list$Caliper == str_sub(one_parent_calDW, 1, 2))]

  # If we didn't find the lead time, try without the machine number
  # gsub("[0-9]", "", str) will remove numbers from a string
  if (length(cycle_dbr) == 0 | is.null(cycle_dbr)) {
    cycle_dbr <- cycle_length_list$Mean.DBR[which(
      gsub("[0-9]", "", cycle_length_list$Mill.Machine) == 
             gsub("[0-9]", "", one_parent_mill) &
        cycle_length_list$Grade == one_parent_grade &
        cycle_length_list$Caliper == str_sub(one_parent_calDW, 1, 2))]
  }

  # If we still didn't get a match, stop
  if (length(cycle_dbr) == 0 | is.null(cycle_dbr)) {
    stop("Lead Time not Found")
  }

  roll_lead_time <- cycle_dbr + order_proc_length + transportation_length

  # Get the demand statistics out of the rd_stats_df and convert to the 
  # appropriate value over the lead time
  op_lt_stats <- rd_stats_df %>%
    filter(Grade == one_parent_grade, CalDW == one_parent_calDW) %>%
    select(Grade, CalDW, Width, dmd_count = nz_dmd_count, dmd_sum, 
           dmd_mean, dmd_sd, dclass) %>%
    mutate(sd_lt = dmd_sd * sqrt(roll_lead_time/dmd_lt_conv),
           dmd_lt = dmd_mean * roll_lead_time/dmd_lt_conv,
           dmd_review = dmd_mean * cycle_dbr/dmd_lt_conv)
  
  # Add the stats back to the one_parent table
  one_parent <- op_lt_stats %>% 
    select(Grade, CalDW, Width, dmd_count, dmd_sum, sd_lt, dmd_lt,
           dmd_review, dclass) %>%
    arrange(Grade, CalDW, Width) %>%
    ungroup()
  
  # Add the trim freight to be used in the trim cost calculation later
  one_parent <- 
    left_join(one_parent, filter(trim_freight_plant, Mill == ifac),
              c("Grade" = "Grade",
                "CalDW" = "CalDW",
                "Width" = "Width"))
  
    # list of potential parent roll widths
  rwidth <- as.numeric(one_parent$Width)
  
  # list of products and rename some columns
  pwidth <- one_parent %>% select(width = Width, dmd_count, dmd_sum, dmd_lt, 
                   sd_lt, dmd_review, dclass, Trim.Frt.Cost)
  pwidth$width <- as.numeric(pwidth$width)
  
  # noSlitter subset - widths that cannot be split
  slitter_limits <- allNoSlitter %>% 
    filter (Grade == one_parent_grade & CalDW == one_parent_calDW) %>% 
    select (Width)
  # formatting
  slitter_limits <- as.data.frame(slitter_limits)
  # make into list 
  slitter_limits_list <- slitter_limits[,1]
  
  # run parent roll selection
  source(file.path(basepath, "R", "4a.Sub Parent Roll Optimization.R"))
  # the output of this is the RollSelOptions dataframe which includes
  # the trim cost and safety stock costs
  # 
  # add roll selection options to list of all roll selection options
  AllRollSelOptions <- rbind(AllRollSelOptions,
                                   RollSelOptions)
  
  # Append the parent_stats to the all table to be used in the simulation
  parent_stats_all <- rbind(parent_stats_all, parent_stats)
}

# ungroup data frame and rearrange the columns to make it look nice
AllRollSelOptions <- AllRollSelOptions %>% ungroup() %>%
  select(Grade, CalDW, Sol.Num.Parents, Sol.Gross.Tons, Sol.Trim.Tons,
         Sol.Trim.Cost, Sol.Handling.Cost, Sol.SS.Tons, Sol.SS.Cost, 
         Sol.OTL.Tons, Sol.Exp.OH.Tons, Sol.Exp.OH.Inv.Cost, 
         Sol.Exp.OH.Storage.Cost, Sol.Total.Cost,  
         Parent.Width, Parent.dclass, Parent.Distrib, Parent.SS_Calc,
         Parent.Nbr.Subs, Parent.DMD.Count,
         Parent.DMD, Parent.SS.Tons, Parent.SS.DOH, Parent.Exp.OH.Tons,
         Parent.Exp.OH.Inv.Cost,
         Parent.OTL, Prod.Width, Prod.DMD.Tons, Prod.DMD.Count, 
         Prod.Gross.Tons, Prod.LT.Tons, Prod.dclass, 
         Prod.Trim.Fact, Prod.Trim.Width, Prod.Trim.Tons)

# select Grade, CalDW, # Parents, & Costs
numOptions <- AllRollSelOptions %>% 
  select(Grade, CalDW, Sol.Num.Parents, Sol.Trim.Cost, 
         Sol.Trim.Tons, Sol.Handling.Cost, 
         Sol.Exp.OH.Storage.Cost, Sol.Exp.OH.Inv.Cost,
         Sol.Exp.OH.Tons, 
         Sol.SS.Cost, Sol.SS.Tons, 
         Sol.Total.Cost) %>% distinct()

# Determine the optimal grouping of parent rolls for all the
# possible combinations from min to max parent rolls.  This will be based on
# minimum total cost: trim cost plus safety stock cost.  The output
# goes into the tresults data frame
source(file.path(basepath, "R", "4b.Sub Parent Roll Grouping.R"))

# Now that we know the optimal groupings of parents, add this info
# back tothe AllRollSelOptions table
solutionRollDetails <- 
  left_join(AllRollSelOptions, 
            select(solresults, solutionGrade, solutionCalDW, parentNbrs,
                   solutionNbr, SolParentCount), 
            by = c("Grade" = "solutionGrade",
                   "CalDW" = "solutionCalDW",
                   "Sol.Num.Parents" = "parentNbrs"))

# Remove the options that are not part of an optimal solution
solutionRollDetails <- solutionRollDetails %>%
  filter(!is.na(solutionNbr))

# solresults_sum comes from the Sub Parent Roll Grouping.R script
solresults_sum %>% ungroup() %>%
  filter(Total.Cost == min(Total.Cost))

# Add the facility we are working on
solresults_sum <- solresults_sum %>%
  mutate(Fac = ifac) %>%
  select(Fac, everything())

min_sol <- solresults_sum %>% slice(which.min(Total.Cost)) %>%
  select(NbrParents, Total.Cost)

solresults_sum$Min.Nbr.Parents <- min_sol$NbrParents
solresults_sum$Min.Total.Cost <- min_sol$Total.Cost

# Put the data in long format for plotting
solresults_sum_plot <- solresults_sum %>% 
  select(Fac, NbrParents, Total.Cost, Trim.Cost, Handling.Cost, Storage.Cost,
         Inv.Carrying.Cost, Min.Nbr.Parents, Min.Total.Cost) %>%
  gather(key = `Cost Component`, value = Cost, 
         Total.Cost:Inv.Carrying.Cost)

psol <- ggplot(solresults_sum_plot, 
       aes(x = NbrParents, color = `Cost Component`)) +
  geom_line(aes(y = Cost), size = 1) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_text(aes(label = as.character(Min.Nbr.Parents),
                x = Min.Nbr.Parents, y = Min.Total.Cost/2), color = "black") +
  geom_segment(aes(x = Min.Nbr.Parents, xend = Min.Nbr.Parents, 
                   y = 0, yend = Min.Total.Cost),
               size = 0.4, color = "black") +
  scale_color_discrete(breaks = c("Total.Cost", "Handling.Cost",
                                  "Inv.Carrying.Cost", 
                                  "Storage.Cost", "Trim.Cost")) 
rm(solresults_sum_plot)
ggplotly(psol)

# Plot the trim width by roll demand
min_sol_trim <- solutionRollDetails %>%
  filter(SolParentCount == min_sol$NbrParents)

# Try dummy faceting
d1 <- min_sol_trim %>%
  select(CalDW, Prod.Width, Prod.Trim.Width) %>%
  mutate(y = Prod.Trim.Width, Prod.Roll = paste(CalDW, Prod.Width))
d1$panel <- "Trim Loss Width"
d2 <- min_sol_trim %>%
  select(CalDW, Prod.Width, Prod.Trim.Width, y = Prod.Gross.Tons) %>%
  mutate(Prod.Roll = paste(CalDW, Prod.Width))
d2$panel <- "Tons"
d <- bind_rows(d1, d2) %>%
  arrange(desc(Prod.Trim.Width)) %>%
  mutate(Prod.Roll = factor(Prod.Roll, unique(Prod.Roll)))

# use faceting - I think this looks the best
trimplot_all <- ggplot(data = d, aes(x = Prod.Roll, fill = CalDW)) + 
  geom_bar(stat = "identity", aes(y = y)) + 
  theme(axis.text.x=element_text(size = 3, angle = 90,hjust = 1,vjust = 0.5)) +
  facet_grid(panel ~ ., scales = "free_y") +
  scale_y_continuous(labels = comma) +
  ggtitle(paste(ifac, "Trim Width by Roll"))

# Make sure we have trim over 2"
if (nrow(filter(d, Prod.Trim.Width >= 2)) > 0) {
  trimplot_gt2 <- ggplot(data = filter(d, Prod.Trim.Width >= 2),
                         aes(x = Prod.Roll, fill = CalDW)) + 
    geom_bar(stat = "identity", aes(y = y)) + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    facet_grid(panel ~ ., scales = "free_y") +
    scale_y_continuous(labels = comma) +
    ggtitle(paste(ifac, "Demand Tons & Trim Width for Rolls > 2 inches"))
  
  ggplotly(trimplot_all)
} else (trimplot_gt2 <- NULL)
# # Back to Back plot tests
# ggplot(data = d, aes(x = Prod.Roll, fill = CalDW)) + 
#   geom_bar(data = subset(d, panel == "Trim Loss Width"), stat = "identity",
#            aes(y = y)) + 
#   geom_bar(data = subset(d, panel == "Tons"), stat = "identity",
#            aes(y = -y)) +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) 
# 
# 
# # Use Gridextra
# twidth <- ggplot(data = d, aes(x = Prod.Roll)) + 
#   geom_bar(data = subset(d, panel == "Trim Loss Width"), stat = "identity",
#            aes(y = y))
# tons <- ggplot(data = d, aes(x = Prod.Roll)) + 
#   geom_bar(data = subset(d, panel == "Tons"), stat = "identity",
#            aes(y = y)) +
#   scale_y_reverse()
# library(gridExtra)
# grid.arrange(twidth, tons, ncol = 1)

print("Finished Seleting Optimal Parent Rolls for each Possible Number of Parents")
# First get the raw data


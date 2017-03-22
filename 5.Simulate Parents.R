#------------------------------------------------------------------------------
#
# V1 - Use the parent_stats_all dataframe from the 
# "4.Parent Roll Optimization.R"  instead of calculating the
# parent statistics again here.
#
#
print("Simulating All Selected Parent Rolls")
# Starting with the solutionRollDetails dataframe, simulate a year's worth of 
# demand for each parent roll for each number of sub rolls

# Build the data frame of unique parents
solutionRollDetails <- ungroup(solutionRollDetails)
# Make the parent width a character
solutionRollDetails$Parent.Width <- 
  as.character(solutionRollDetails$Parent.Width)

# Unique combinations of parent widths
parent_list <- solutionRollDetails %>%
  select(Grade, CalDW, Parent.Width, Parent.Nbr.Subs) %>%
  distinct() %>% arrange(Grade, CalDW, Parent.Width, Parent.Nbr.Subs)

# Filter for one solution
#parent_stats <- parent_stats %>%
#  filter(Grade == "AKPG" & CalDW == "18-72-F"& Parent.Width == 64.5)

# For each parent & nbr subs, compute the Safety Stock and lead-time 
# demand to determine
# the inventory control parameters (s,S).  Then pass this info to the 
# simulation

# Use a data list to store the results from each iteration, and then
# bind them into a dataframe after
datalist <- list()
sim_plotlist <- list()

i <- 17 # for testing, jump over next statement
for (i in 1:nrow(parent_list)){
  print(paste(ifac, ": Simulating ", i, "/", nrow(parent_list), ": Grade = ",
              parent_list$Grade[i], ", CalDW = ", parent_list$CalDW[i],
              ", Parent Width = ", parent_list$Parent.Width[i], 
        ", Nbr sub-rolls = ", parent_list$Parent.Nbr.Subs[i], sep = ""), 
        quote = FALSE)
  
  t_parent <- parent_stats_all %>% 
    filter(Grade == parent_list$Grade[i],
           CalDW == parent_list$CalDW[i],
           Parent.Width == parent_list$Parent.Width[i],
           Parent.Nbr.Subs == parent_list$Parent.Nbr.Subs[i])
  
  
    #t_parent <- data.frame(t_grade = parent_list$Grade[i],
    #                     t_calDW = parent_list$CalDW[i],
    #                     t_parent_width = parent_list$Parent.Width[i],
    #                     t_nbr_subs = parent_list$Parent.Nbr.Subs[i],
    #                     stringsAsFactors = FALSE)
  
  # Use the data we already have in the parent_stats_all dataframe
  
  

      # Aggregate demands for all the rolls associated with this parent and
  # number of subs
  
  # first create a data frame that lists all the parents with their
  # sub rolls
  sub_rolls <- inner_join(select(solutionRollDetails, Grade, 
                                CalDW, Prod.Width, Parent.Width,
                                Parent.Nbr.Subs), t_parent,
                         by = c("Grade" = "Grade",
                                "CalDW" = "CalDW",
                                "Parent.Width" = "Parent.Width",
                                "Parent.Nbr.Subs" = "Parent.Nbr.Subs")) %>%
    distinct()

  # Calculate trim loss factor
  sub_rolls <- sub_rolls %>% 
      mutate(t_fact = 1 + 
               ((as.numeric(Parent.Width) %% Prod.Width) / Prod.Width))

  # get the raw data - this will include the parent and all its 
  # sub roll demands 
  raw_data_parent <- inner_join(sub_rolls, 
                 select(rawData, Date, Grade, CalDW, Width, Tons), 
                               by = c("Grade" = "Grade",
                                      "CalDW" = "CalDW",
                                      "Prod.Width" = "Width"))

  # Calculate demand for parent roll size to account for trim
  raw_data_parent <- raw_data_parent %>%
      mutate(Tons = Tons * t_fact)
    
  # summarise parent demand by Date
  raw_data_parent<- raw_data_parent %>%
      group_by(Date, Grade, CalDW, Parent.Width, Parent.Nbr.Subs) %>%
        summarize(Tons = sum(Tons))
  
  #TODO: Look for a trend component
  
  # Plot the demands
  # first daily
  plot_demand <- F
  if(plot_demand) {
    library(gridExtra) # for the grid.arrange function
    g <- ggplot(data = raw_data_parent, aes(Date, Tons)) +
      geom_point(aes(x = Date, y = Tons), size = 1) +
      geom_segment(aes(x = Date, xend = Date, y = 0, yend = Tons)) + 
      geom_rug(sides = "l")
    g
    # then aggregated by lead time
    raw_data_parent <- ungroup(raw_data_parent)
    tempdf <- data.frame(Date = seq.Date(as.Date(start_date), 
                                         as.Date(end_date), by = 1)) %>%
      left_join(select(raw_data_parent, Date, Tons), 
                by = c("Date" = "Date")) %>%
      mutate(Tons = ifelse(is.na(Tons), 0, Tons))
    tempdf <- tempdf %>% 
      mutate(tw = as.integer(dense_rank(Date)/roll_lead_time))
    tempdf <- aggregate(data = tempdf, . ~ tw, sum)
    scatter_plot <- ggplot(data = tempdf, aes(tw, Tons)) +
      geom_point(size = 1) +
      geom_segment(aes(x = tw, xend = tw, y = 0, yend = Tons))
    hist_right <- ggplot(tempdf, aes(Tons)) + 
      geom_histogram(bins = 30, color = "black") +
      coord_flip()  
    grid.arrange(scatter_plot,hist_right, ncol=2, nrow=1) 
    #, widths=c(2, 1), heights=c(1, 2))
    rm(tempdf)
  }

  # Inventory policies:
  # "sS": s,S (reorder point/order up to)
  # "TS": T, S (periodic revew order up to) where
  #   S = s + Q, were Q = DBR * Daily Demand Mean
   
  # Based on Silver & Peterson, S = safety stock + dmd_lt
  #rd_parent_stats$otl <- rd_parent_stats$sfty_stock + 
  #  rd_parent_stats$dmd_lt
  # Llamasoft rule
  #rd_parent_stats$otl <- rd_parent_stats$rop + rd_parent_stats$dmd_review
  
   
  #littles <- rd_parent_stats$rop # the re-order point
  #bigs <- rd_parent_stats$otl
  inv_policy <- "TS"
  littles <- t_parent$Parent.ROP
  bigs <- t_parent$Parent.OTL
  bigt <- t_parent$cycle_dbr
  
  #t_parent <- bind_cols(t_parent, rd_parent_stats)
  
  sim_days = end_date - start_date

  # first_review will depend on the caliper, mill and machine and the
  # period of the cycles.  But for now, just pick a random number between 
  # 
  first_review <- sample(1:7, 1, TRUE)
  
  # Run the simulation
  sim_res <- dosim(invinitglbls, invreactevnt, invprntrslts, sim_days,
          list(inv_policy = inv_policy, bigs = bigs, littles = littles, 
               bigt = bigt, init_inv_level = bigs,
               lt = t_parent$lead_time, first_review = first_review,
               cycle_dbr = t_parent$cycle_dbr,
               datestart = start_date, 
               warmup = stabilizationPeriod,
               dmd = raw_data_parent, grade = t_parent$Grade,
               calDW = t_parent$CalDW, ifac = ifac,
               parent_width = t_parent$Parent.Width,
               nbr_subs = t_parent$Parent.Nbr.Subs), dbg = FALSE, plot = TRUE)

  dat <- data.frame(Grade = t_parent$Grade, 
                    CalDW = t_parent$CalDW, 
                    Parent_Width = t_parent$Parent.Width, 
                    nbr_rolls = t_parent$Parent.Nbr.Subs,
                    cycle_dbr = t_parent$cycle_dbr,
                    roll_lt = t_parent$lead_time,
                    #distribution = t_parent$Parent.Distrib,
                    #act_distrib = t_parent$Parent.Act_Distrib,
                    safety_stock = t_parent$Parent.SS.Tons,
                    #dclass = t_parent$Parent.dclass,  
                    total_demand = sim_res$results$total_demand,
                    OTL = t_parent$Parent.OTL,
                    avg_oh_qty = sim_res$results$avg_oh, 
                    nbr_bo = sim_res$results$nbr_bo, 
                    avg_bo_qty = sim_res$results$avg_bo, 
                    cum_bo = sim_res$results$cum_bo,
                    order_fill_rate = sim_res$results$order_fill_rate,
                    qty_fill_rate = sim_res$results$qty_fill_rate,
                    nbr_demands = sim_res$results$nbr_demands, 
                    cum_demand = sim_res$results$cum_demand,
                    bo_days = sim_res$results$bo_days, 
                    stringsAsFactors = FALSE)
    
    
  datalist[[i]] <- dat    
  sim_plotlist[[i]] <- sim_res$plot
  #print(sim_res)
}  

# Here's how to get the chart titles
#load(file.path(fpoutput, paste("W6", "Sim Plots.RData")))
#  
#t <- do.call(rbind, lapply(sim_plotlist, function(x) x$labels$title))
# sim_plotlist[177]
#
# Save to PDF, one per page
# pdf("sim_plots.pdf", width = 10, paper = "USr")
# invisible(lapply(sim_plotlist, print))
# dev.off()
sim_results <- bind_rows(datalist)
sim_results <- sim_results %>%
  rename(Sim.Parent.SS = safety_stock,
         #Sim.Parent.dclass = dclass,
         #Sim.Parent.Distribution = distribution,
         Sim.Parent.DMD = total_demand,
         Sim.Parent.OTL = OTL,
         Sim.Parent.Avg.OH = avg_oh_qty,
         Sim.Parent.Nbr.BO = nbr_bo,
         Sim.Parent.BO.Days = bo_days,
         Sim.Parent.Cum.BO = cum_bo,
         Sim.Parent.Avg.BO = avg_bo_qty,
         Sim.Parent.Ord.Fill.Rate = order_fill_rate,
         Sim.Parent.Qty.Fill.Rate = qty_fill_rate,
         Sim.Parent.DMD.Count = nbr_demands,
         Sim.Parent.Cum.DMD = cum_demand)

sim_results <- as.data.frame(sim_results)

# Add the current facility
sim_results <- sim_results %>%
  mutate(Fac = ifac) %>%
  select(Fac, everything())

# Add the simulation results back to the AllRollSelOptions df
# solutionRollDetails comes from 4.Parent Roll Selection
solutionRollDetailsSim <- left_join(solutionRollDetails, 
                                     sim_results, 
                                     by = c("Grade" = "Grade",
                                            "CalDW" = "CalDW",
                                            "Parent.Width" = "Parent_Width",
                                            "Parent.Nbr.Subs" = "nbr_rolls"))
rm(solutionRollDetails)

# Save the optimal solution
sim_solution_opt <- solutionRollDetailsSim %>%
  filter(SolParentCount == min_sol$NbrParents) %>%
  select(Fac, everything())

# Compute summary statistics for each solution by aggregating over the parent 
# widths for each Grade/CalDW
# First remove any columns pertaining to the products (Grade/CalDW/Width)
sim_solution_parent <- solutionRollDetailsSim %>%
  select(-starts_with("Prod")) %>%
  distinct() %>% ungroup()

sim_solution_summary <- sim_solution_parent %>%
  group_by(solutionNbr, SolParentCount) %>%
  summarize(AvgOnHand = sum(Sim.Parent.Avg.OH),
            OTL = sum(Sim.Parent.OTL),
            AvgDOH = 365 * sum(Sim.Parent.Avg.OH)/
              sum(Sim.Parent.DMD),
            BODays = sum(Sim.Parent.BO.Days), 
            TotDemand = sum(Sim.Parent.DMD),
            CumDemand = sum(Sim.Parent.Cum.DMD),
            NbrOrders = sum(Sim.Parent.DMD.Count),
            NbrBackOrders = sum(Sim.Parent.Nbr.BO),
            CumBO = sum(Sim.Parent.Cum.BO),
            OrdFillRate = 1 - sum(Sim.Parent.Nbr.BO)/
              sum(Sim.Parent.DMD.Count),
            QtyFillRate = 1 + sum(Sim.Parent.Cum.BO)/
              sum(Sim.Parent.Cum.DMD)) %>%
  ungroup()

# Create dummy facets to show the fill rate versus inventory for each solution
d1 <- sim_solution_summary %>%
  select(SolParentCount, AvgOnHand) %>%
  mutate(panel = "Inventory Tons")
d2 <- sim_solution_summary %>%
  select(SolParentCount, QtyFillRate) %>%
  mutate(panel = "Service Percent")
d <- bind_rows(d1, d2)
d$SolParentCount <- as.numeric(d$SolParentCount)

# Plot the service versus On Hand
serviceplot <- ggplot(d, aes(x = SolParentCount)) +
  geom_line(aes(y = AvgOnHand, col = "Avg On Hand"), size = 1) +
  #geom_line(aes(y = BODays, col = "BO Days"), size = 1) +
  geom_line(aes(y = QtyFillRate, col = "Fill Rate"), size = 1) +
  scale_y_continuous(labels = comma) +
  facet_grid(panel ~ ., scales = "free_y") +
  ggtitle(paste(ifac, "Service vs. Roll Count")) +
  theme(axis.title.y = element_blank())

#ggplotly(serviceplot)


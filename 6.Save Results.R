#------------------------------------------------------------------------------
#
# Write the output files and clean up
# Use the write_csv function from the readr package
#
# The plots are created in the 4.Parent Roll Selection script

# Demand profile for individual products
write_excel_csv(rd_stats_df,
          file.path(fpoutput, paste(ifac, "Demand Profile-Product.csv")))

# The results of the optimization, total cost for each possible solution
write_excel_csv(solresults_sum,
                file.path(fpoutput,
                          paste(ifac, "Optimization Solutions Summary.csv")))

write_excel_csv(solutionRollDetailsSim,
                file.path(fpoutput,
                          paste(ifac, "All Solution Details.csv")))

# The plot showing the total optimization solution results
ggsave(filename = paste(ifac, "Solution Plot.png"),
       plot = psol, path = fpoutput)

# The plot showing the trim loss for individual rolls
ggsave(filename = paste(ifac, "Solution Trim All.png"),
       plot = trimplot_all, path = fpoutput)
ggsave(filename = paste(ifac, "Solution Trim GT2.png"),
       plot = trimplot_gt2, path = fpoutput)

# Summary statistics for each parent from the simulation run
write_excel_csv(sim_results,
                file.path(fpoutput,
                          paste(ifac, "Simulation Parent Results.csv")))

# The optimal solution details
write_excel_csv(sim_solution_opt,
                file.path(fpoutput,
                          paste(ifac, "Sim Results Opt Detail.csv")))

# Plot showing service fill rate versus parent roll count
ggsave(filename = paste(ifac, "Service by Roll Count.png"),
       plot = serviceplot, path = fpoutput)


# Save the simulation plots
save(sim_plotlist, file = file.path(fpoutput, 
                                    paste(ifac, "Sim Plots.RData")))

# Save to PDF, one per page
sim_plot_file <- file.path(fpoutput, 
                           paste0(ifac, " sim_plots.pdf"))
pdf(file = sim_plot_file, width = 10, paper = "USr", pointsize = 12,
    onefile = TRUE)
invisible(lapply(sim_plotlist, base::print))
dev.off()


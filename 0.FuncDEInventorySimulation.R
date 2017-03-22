# DES.R:  R routines for discrete-event simulation (DES)
# Prof Norm Matloff

# each event will be represented by a data frame row consisting of the
# following components:  evnttime, the time the event is to occur;
# evnttype: a character string for the programmer-defined event type;
# eventpriority: tie breaking value.  
# optional application-specific components, e.g.
# the job's arrival time in a queuing app

# a global list named "sim" holds the events data frame, evnts, and
# current simulated time, currtime; there is also a component dbg, which
# indicates debugging mode

# forms a row for an event of type evntty that will occur at time
# evnttm; see comments in schedevnt() regarding appin
evntrow <- function(evnttm, evntpr, evntty, appin = NULL) {
  rw <- c(list(evnttime = evnttm, evntpriority = evntpr, evnttype=evntty), appin)
  return(as.data.frame(rw))
}

# insert event with time evnttm and type evntty into event list;
# appin is an optional set of application-specific traits of this event,
# specified in the form a list with named components
schedevnt <- function(evnttm, evntpr, evntty, appin = NULL) {
  #browser()
  newevnt <- evntrow(evnttm, evntpr, evntty, appin)
  # if the event list is empty, set it to consist of evnt and return
  if (is.null(sim$evnts)) {
    sim$evnts <<- newevnt
    return()
  }
  # find insertion point
  inspt <- binsearch(sim$evnts[1:2], evnttm, evntpr) 
  # now "insert," by reconstructing the data frame; we find what
  # portion of the current matrix should come before the new event and
  # what portion should come after it, then string everything together
  before <- 
    if (inspt == 1) NULL else sim$evnts[1:(inspt - 1),]
  nr <- nrow(sim$evnts)
  after <- if (inspt <= nr) sim$evnts[inspt:nr,] else NULL
  sim$evnts <<- rbind(before, newevnt, after)
}

# binary search of insertion point of y in the sorted vector x with
# priority z; returns
# the position in x before which y,z should be inserted, with the value
# length(x)+1 if y is larger than x[length(x)]; could be changed to C
# code for efficiency
binsearch <- function(x, y, z) {
  n <- nrow(x)
  lo <- 1
  hi <- n
  # Make the priority the 10ths digit and search as before
  xp <- x$evnttime + 0.1 * x$evntpriority
  yp <- y + 0.1 * z
  while(lo+1 < hi) {
    mid <- floor((lo+hi)/2)
    if (yp == xp[mid]) return(mid)
    if (yp < xp[mid]) hi <- mid else lo <- mid
  }
  if (yp <= xp[lo]) return(lo)
  if (yp < xp[hi]) return(hi)
  return(hi+1)
}

# start to process next event (second half done by application
# programmer via call to reactevnt()) 
getnextevnt <- function() {
  head <- sim$evnts[1,]
  # delete head
  if (nrow(sim$evnts) == 1) {
    sim$evnts <<- NULL
  } else sim$evnts <<- sim$evnts[-1, , drop = FALSE]
  return(head)
}

# simulation body
# arguments:
#    initglbls:  application-specific initialization function; inits
#      globals to statistical totals for the app, etc.; records apppars
#      in globals; schedules the first event
#    reactevnt: application-specific event handling function, coding the
#       proper action for each type of event
#    prntrslts:  prints application-specific results, e.g. mean queue
#       wait
#    apppars:  list of application-specific parameters, e.g.
#      number of servers in a queuing app
#    maxsimtime:  simulation will be run until this simulated time 
#    dbg:  debug flag; if TRUE, sim will be printed after each event
dosim <- function(initglbls, reactevnt, prntrslts, maxsimtime, apppars=NULL,
                  dbg=FALSE, plot=FALSE) {
  sim <<- list()
  sim$currtime <<- 0.0  # current simulated time
  sim$evnts <<- NULL  # events data frame (time, priority, type, quantity)
  sim$dbg <<- dbg
  sim$plot <<- plot
  initglbls(apppars)
  #browser()
  while(sim$currtime < maxsimtime) { 
    if (is.null(sim$evnts)){
      print(paste("Event list empty at time: ", sim$currtime)) 
      #break()
    }
    head <- getnextevnt()
    sim$currtime <<- head$evnttime  # update current simulated time
    reactevnt(head)  # process this event 
    if (dbg) print(sim)
  }
  prntrslts()
}

# DES application: M/M/1 queue, arrival rate 0.5, service rate 1.0
# the call
# dosim(invinitglbls,mm1reactevnt,mm1prntrslts,10000.0,
# list(arrvrate=0.5,srvrate=1.0))
# should return a value of about 2 (may take a while)

# initializes global variables specific to this app
# sample demand data
# dmdtest <- rawData %>%
#  select(Grade, Caliper, Width, Date, Tons) %>% 
#  filter(Grade == "SUS", Caliper == ".024",
#              Width == 46.0)

invinitglbls <- function(apppars) {
  #browser()
  invglbls <<- list()
  # simulation parameters
  invglbls$inv_policy <<- apppars$inv_policy
  invglbls$bigs <<- apppars$bigs  # S
  invglbls$littles <<- apppars$littles # s
  invglbls$bigt <<- apppars$bigt # T, cycle dbr days
  invglbls$lt <<- apppars$lt # lead time
  invglbls$first_review <<- apppars$first_review # Day of first review
  invglbls$datestart <<- apppars$datestart # calendar start date
  invglbls$time_last_event <<- 0.0  # marker to track time average stats
  invglbls$oh_inv_level <<- apppars$init_inv_level # current on hand
  invglbls$bo_inv_level <<- 0.0
  invglbls$open_order_qty <<- 0.0
  invglbls$oh_area <<- 0.0 # area under the on hand inv qty curve; resets
  invglbls$bo_area <<- 0.0 # area under the back order qty curve; resets
  invglbls$nbr_demands <<- 0 # number of demand events; resets
  invglbls$cum_demand <<- 0 # cumulative demand quantity; resets
  invglbls$total_demand <<- 0 # total demand - no resets
  invglbls$cum_bo <<- 0 # cumulative back order quantity; resets
  invglbls$nbr_replen <<- 0 # number of replenishment receipts; no resets
  invglbls$nbr_bo <<- 0 # number of backorder events; resets
  invglbls$days_bo <<- 0 # number of days on backorder; resets
  invglbls$ifac <<- apppars$ifac
  invglbls$grade <<- apppars$grade
  invglbls$calDW <<- apppars$calDW
  invglbls$parent_width <<- apppars$parent_width
  invglbls$nbr_subs <<- apppars$nbr_subs
  invglbls$warmup <<- apppars$warmup
  
  #invglbls$dmd <<- apppars$dmd
  invglbls$inv_trace <<- data.frame(Date = invglbls$datestart, 
                                    `On Hand` = invglbls$oh_inv_level,
                                    `Back Order` = invglbls$bo_inv_level,
                                    `Replen Order` = invglbls$open_order_qty)
  
  # schedule all the demand events
  for (i in 1:nrow(apppars$dmd)) {
    dmdday <- as.integer(apppars$dmd$Date[i] - invglbls$datestart)
    # demand events are priority 0 - do these first
    schedevnt(dmdday, 0, "dmd", list(qty = apppars$dmd$Tons[i]))
  }
  
  # scheule the stats reset to occur at the end of the warmup period
  schedevnt(apppars$warmup, 0, "reset", list(qty = 0))
  
  # if (T,S) policy, schedule the first review
  if (invglbls$inv_policy == "TS") {
    # schedule with priority = 1, after demands and replens
    schedevnt(invglbls$first_review, 1, "review", list(qty = 0))
  }
  
  # schedule an end of simulation event
  schedevnt(365, 9, "end", list(qty = 0))
}

# application-specific event processing function called by dosim()
# in the general DES library
invreactevnt <- function(head) {
  # Compute time since last event and update the last event time marker
  time_since_last_event <- sim$currtime - invglbls$time_last_event
  invglbls$time_last_event <<- sim$currtime
  
  # update the appropriate area up to this event time
  invglbls$oh_area <<- invglbls$oh_area + 
      time_since_last_event * invglbls$oh_inv_level
  invglbls$bo_area <<- invglbls$bo_area + 
      time_since_last_event * invglbls$bo_inv_level
  # days on backorder
  if (invglbls$bo_inv_level < 0) invglbls$days_bo <<- invglbls$days_bo +
    time_since_last_event
  
  if (head$evnttype == "reset") {
    invglbls$oh_area <<- 0
    invglbls$bo_area <<- 0
    invglbls$days_bo <<- 0
    invglbls$nbr_bo <<- 0
    invglbls$cum_bo <<- 0
    invglbls$cum_demand <<- 0
    invglbls$nbr_demands <<- 0
  }
  
  # There are 3 event types (not counting the end) 
  #   1. Demand Event ("dmd")
  #   2. Replenishment Arrives ("repl")
  #   3. Demand Review/Replen Order Creation ("review")
  
  if (head$evnttype == "dmd") { # demand event
    invglbls$nbr_demands <<- invglbls$nbr_demands + 1
    invglbls$cum_demand <<- invglbls$cum_demand + head$qty
    invglbls$total_demand <<- invglbls$total_demand + head$qty  # No reset
    # can we fill it?
    if (head$qty > invglbls$oh_inv_level) {  #No
      invglbls$nbr_bo <<- invglbls$nbr_bo + 1  # increment the number of bo's
      invglbls$bo_inv_level <<- invglbls$bo_inv_level - 
        (head$qty - invglbls$oh_inv_level)
      invglbls$cum_bo <<- invglbls$cum_bo - 
        (head$qty - invglbls$oh_inv_level)
      invglbls$oh_inv_level <<- 0
    } else {
      # decrement the on-hand
      invglbls$oh_inv_level <<- invglbls$oh_inv_level - head$qty
    }
    # below the re-order point and (s,S) policy?
    if (invglbls$inv_policy == "sS") {
      if (invglbls$oh_inv_level + invglbls$open_order_qty +
          invglbls$bo_inv_level < invglbls$littles) {
        # enter a replen order
        order_qty <- 
          invglbls$bigs - invglbls$oh_inv_level - invglbls$open_order_qty -
          invglbls$bo_inv_level
        invglbls$open_order_qty <<- invglbls$open_order_qty + order_qty
        invglbls$nbr_replen <<- invglbls$nbr_replen + 1
        replen_time <- sim$currtime + invglbls$lt
        # schedule the arrival with priority 0
        schedevnt(replen_time, 0, "repl", list(qty = order_qty))
      }
    }
    
  } 
  
  if (head$evnttype == "repl") { # replen order arrives
    # adjust the inventory levels
    # first reduce backorder quantity
    net_inv <- head$qty + invglbls$bo_inv_level + invglbls$oh_inv_level
    invglbls$bo_inv_level <<- min(net_inv, 0)
    invglbls$oh_inv_level <<- max(net_inv, 0)
    invglbls$open_order_qty <<- invglbls$open_order_qty - head$qty
  }
  
  if (head$evnttype == "review") {
    # compute the inventory position
    invpos <- invglbls$oh_inv_level + invglbls$open_order_qty +
      invglbls$bo_inv_level  # bo is negative
    
    if (invpos < invglbls$bigs) {
      # enter a replen order
      order_qty <- 
        invglbls$bigs - invglbls$oh_inv_level - invglbls$open_order_qty -
        invglbls$bo_inv_level
      invglbls$open_order_qty <<- invglbls$open_order_qty + order_qty
      invglbls$nbr_replen <<- invglbls$nbr_replen + 1
      replen_time <- sim$currtime + invglbls$lt
      # schedule the arrival with priority 0
      schedevnt(replen_time, 0, "repl", list(qty = order_qty))
    }
    # schedule another review after all other transactions that might
    # occur on that same day (priority = day.1)
    schedevnt(sim$currtime + invglbls$bigt, 1, "review", list(qty = 0))
  }
    
  # Add the data to the trace data frame
  new_row <- data.frame(Date = invglbls$datestart + sim$currtime, 
                        `On Hand` = invglbls$oh_inv_level,
                        `Back Order` = invglbls$bo_inv_level,
                        `Replen Order` = invglbls$open_order_qty)
  invglbls$inv_trace <<- rbind(invglbls$inv_trace, new_row)
}
invprntrslts <- function() {
  #print(paste("Avg OH = ", invglbls$oh_area/365))
  #print(paste("Nbr Backorders = ", invglbls$nbr_bo))
  #print(paste("Avg BO Qty = ", invglbls$bo_area/365))
  #print(paste("Demand Fill Pct = ", invglbls$nbr_bo / invglbls$nbr_demands))
  #print(paste("BO Days = ", invglbls$days_bo))
  #print(invglbls)

  res <- data.frame(total_demand = invglbls$total_demand,
                    avg_oh = invglbls$oh_area/(365 - invglbls$warmup), 
                    nbr_bo = invglbls$nbr_bo,
                    avg_bo = invglbls$bo_area/(365 - invglbls$warmup),
                    nbr_demands = invglbls$nbr_demands,
                    cum_demand = invglbls$cum_demand,
                    cum_bo = invglbls$cum_bo,
                    bo_days = invglbls$days_bo,
                    order_fill_rate = 1 - invglbls$nbr_bo / invglbls$nbr_demands,
                    qty_fill_rate = 1 + invglbls$cum_bo / invglbls$cum_demand # cum_bo is neg
  ) 
  
  if (sim$plot) {
    # Return the plot along with the dat
    p <- ggplot(data = invglbls$inv_trace, aes(Date, On.Hand)) +
      geom_step(aes(x = Date, y = On.Hand), size = 1, direction = "hv") +
      geom_step(aes(x = Date, y = Back.Order), size = .5, color = "red", 
                direction = "hv") +
      geom_step(aes(x = Date, y = Replen.Order), size = .5, 
              color = "blue", direction = "hv", linetype = 2) +
      ggtitle(paste("Inventory History:", invglbls$ifac, invglbls$grade, invglbls$calDW,
                       invglbls$parent_width, "Subs =", invglbls$nbr_subs,
                    "Avg OH =", round(invglbls$oh_area/(365 - invglbls$warmup), 1), "FR =",
                    round(100*(1 + invglbls$cum_bo / invglbls$cum_demand), 1)))
    
    return(list(results = res, plot = p))
  } else {
  return(list(results = res))
  }
}



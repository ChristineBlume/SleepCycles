split <- list(

#' @description Auxiliary function. Checks if any NREM part (excluding Wake) of a NREM period is > 120 minutes.
  is.toolong <- function(data){
    ## is any NREM part (excl. wake) of a NREMP longer than 120min?
    cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
    
    toolong <- NA
    for (k in seq(2,length(cycs),2)){ #check every second one as only every second is a NREMP
      subset <- data[c(cycs[k-1]:(cycs[k]-1)),]
      wake_eps <- sum(subset$Descr2 == "W")
      if (((cycs[k]-cycs[k-1])-wake_eps)>=240){ #<= as cycs[2] (cf. line 7) is already the beginning of a REMP
        toolong <- c(toolong, cycs[k-1])
      }
    }
    toolong <- na.omit(toolong)
    return(toolong)
  },
  
#' @description   Auxiliary function. Splits NREMPs that are too long (i.e., > 120 min). The function
#' makes suggestions for splitting at N3 epoch(s) following "lightening" of sleep. The user can also
#' decide to not split, enter a pre-defined epoch number to split at, or skip the night.
  
toolong_split <- function(data, toolong, filename){
  Description <- NULL #necessary to remove NOTE
  
  ## now split NREMPs that are too long
  for (zz in 1:length(toolong)){
    cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
    curr_toolong <- toolong[c(zz)]
    
    message(paste0("Attempting to split NREMP ", as.character(zz), " out of ", as.character(length(toolong)), "."))
    
    beg_end <- c(cycs[which(cycs==curr_toolong)], cycs[which(cycs==curr_toolong)+1]) #find beginning and end of NREMP that is too long
    
    # find RWN12 episodes that are > 12min within detected NREMP -> "lightening of sleep" = potential splitting points
    RWN12s <- which(data$Descr2 == "RWN12") #which 30s epochs are R, W, or N1/2
    RWN12s <- RWN12s[c(RWN12s>=beg_end[1] & RWN12s<=beg_end[2])] # search for R, W, N1, or N2 epochs between beginning and end of current NREMP
    RWN12s_start <- NA
    for (kk in 1:(length(RWN12s)-23)){
      if (all(seq(RWN12s[kk],length.out = 24) == RWN12s[seq(kk,kk+23)])){ # check if the sequence of R, W, N12 epochs is continuous min. 12min
        RWN12s_start <- c(RWN12s_start, RWN12s[kk])
      }
    }
    RWN12s_start <- RWN12s_start[-c(1)] #first was NA, remove
    rm(kk)
    
    # do not consider 12min period as potential splitting point if it marks the beginning of the NREMP to be split
    RWN12s_start2 <- RWN12s_start[c(RWN12s_start > beg_end[1])]
    
    if (length(RWN12s_start2) > 0){ #only split if there is a lightening of sleep following the onset of the NREMP
      # find beginnings of >12min RWN12 sequences of 'lighter' sleep
      RWN12s_start2 <- RWN12s_start[1]
      for (kk in 1:(length(RWN12s_start)-1)){
        if ((RWN12s_start[kk+1]-RWN12s_start[kk])>1){
          RWN12s_start2 <- c(RWN12s_start2, RWN12s_start[kk+1]) #if there is a discontinuity in the sequence, mark the beginning of a new 12min RWN12 sequence
        }
      }
      
      # find N3 episodes within period to split
      N3s <- which(data$Descr2 == "N3") #which are N3 epochs?
      N3s <- N3s[c(beg_end[1] < N3s) & c(beg_end[2] > N3s)]
      N3s <- N3s[c(N3s > RWN12s_start2[1])] # only N3s after start of 12min of R/W/N1/N2
      
      if (length(N3s) > 0){
        if(length(N3s) == 1){
          N3_start <- N3s
        }else{
          # find starting points of continuous N3 sequences
          N3_start <- N3s[1]
          for (kk in 1:(length(N3s)-1)){
            if ((N3s[kk+1]-N3s[kk])>1){
              N3_start <- c(N3_start, N3s[kk+1]) #if there is a discontinuity in the sequence, mark the beginning of a new NREM sequence
            }
          }
        }
        
        # select starting points of N3 following 12min of R/W/N1/2
        RWN12s_start2 <- RWN12s_start2[RWN12s_start2<tail(N3_start,1)] # RWN12s episode must start before start of last N3
        n <- NA
        for (zzz in 1:length(RWN12s_start2)){
          minpositive = function(x) min(x[x > 0])
          val <- which(minpositive(N3_start - RWN12s_start2[zzz]) == (N3_start - RWN12s_start2[zzz]))
          n <- c(n, val)
        }
        n <- na.omit(n)
        N3_start2 <- N3_start[n] 
        
        # second NREMP starts with N3 following 12min of R/W/N1/2
        if(length(N3_start2)>0){
          data$CycleStart[c(N3_start2)] <- "NREMP"
          
          splits <- N3_start2 # all potential splitting points
          splits <- unique(splits)
          
          # ask user if s/he is happy with the result of the splitting
          # plot results
          dfplot <- data
          dfplot$time <- seq(1, nrow(dfplot)) # gives epochs
          dfplot$Description[dfplot$Description == 1] <- -1
          dfplot$Description[dfplot$Description == 2] <- -2
          dfplot$Description[dfplot$Description == 3] <- -3
          dfplot$Description[dfplot$Description == 5] <- 1
          
          pp <- ggplot(dfplot, aes(x=time, y=Description, colour=Description)) 
          pp <- pp + theme_bw()+
            geom_point() +
            geom_line(aes(x=time, y=Description))+
            ggtitle(as.character(filename))+
            xlab("Time") +
            ylab("Sleep Stage")+
            scale_y_continuous(limits = c(-3,2), breaks = c(-3, -2, -1, 0, 1), labels = c("N3", "N2", "N1", "W", "REM"))+
            scale_color_viridis(name = "Sleep Stage", option = "D")+
            geom_vline(xintercept = c(splits), lty = 2, colour = "red")+
            annotate(geom="text", x = 500, y = 2, label = paste("can split at epoch(s):", paste(splits, collapse = ","), sep = " "))
          print(pp)
          
          # check if both NREM parts would still be >=20min 
          part1 <- splits-beg_end[1]
          part2 <- beg_end[2]-splits+1
          print(paste0("When splitting at 1st/ 2nd/... suggestion, NREMP1 would be ", (as.character(part1/2)), " min.", 
                       "and NREMP2 would be ", (as.character(part2/2)), " min.", sep = " "))
          if (any(part1 < 30) | any(part2 < 30)){
            message("BE CAREFUL: Splitting might result in NREM period that is shorter than 15 min.")
          }
          
          # Ask about happiness level
          val <- readline("Where do you want to split? Type the 1/2/3/... to select a suggestion, n to split at specific epoch or not at all, or skip to skip this night for now. ") #  1 splits at 1st suggested epoch, n offers to choose, skip skips this night
          
          if(val == "skip"){
            message("This period/night is skipped.")
            next
          }else if (val == "n"){
            newperiod <- readline("At which epoch do you want to start the new NREM period instead? Please type epoch number or NA to not split. ") 
            data$CycleStart[c(N3_start)] <- NA #rm old indicator
            if (newperiod != "NA"){
              data$CycleStart[as.numeric(newperiod)] <- "NREMP"
            }
          }else if (val == "1" | val == "2" | val == "3" | val == "4" | val == "5" | val == "6" | val == "7" | val == "8" | val == "9" | val == "10"){
            data$CycleStart[c(N3_start2)] <- NA #rm old indicator
            y <- as.numeric(val)
            data$CycleStart[N3_start2[y]] <- "NREMP"
          }else{
            message("Missing entry. This night is skipped.")
          }
          rm(dfplot, pp)
        }else{
          message("Cannot split. No N3 following 'lightening' of sleep has been detected.")
        }
      }else{
        message("Cannot split. No N3 following 'lightening' of sleep has been detected.")
      }
    }else{
      message("No 'lightening' of sleep or N3 detected after the onset of the NREMP. Cannot split.")
    }
  }
  return(data)
}
)

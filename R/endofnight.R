endofnight <- list(
  
  #' @description Auxiliary function. Removes incomplete NREM-REM cycle at the end of the night (i.e., cycles followed by <5min NREM or W) if rm_incompletecycs = T.
  
  rm.incompletecycs <- function(data){
    cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP") #remove NREM/W following last REMP or REM/W following last NREMP
    if (data$CycleStart[cycs[length(cycs)]] == "REMP"){
      REMs <- which(data$Descr3 == "REM") #which 30s epochs are REM
      stop <- REMs[length(REMs)]+1 # stop after last REM epoch
      end <- which(data$Descr3 == "NREM")
      end <- end[end>=stop]
      if(length(end)>10){
        data$CycleStart[stop] <- "stop"
        data$cycles[stop:nrow(data)] <- NA
        data$REM.NREM[stop:nrow(data)] <- NA
      }else{
        # remove last REMP
        stop <- cycs[length(cycs)]
        data$CycleStart[cycs[length(cycs)]] <- "stop"
        data$cycles[stop:nrow(data)] <- NA
        data$REM.NREM[stop:nrow(data)] <- NA
      }
    }else{
      NREMs <- which(data$Descr3 == "NREM") #which 30s epochs are NREM
      stop <- NREMs[length(NREMs)]+1 # stop after last NREM epoch
      end <- which(data$Descr3 == "REM")
      end <- end[end>=stop]
      if(length(end)>10){
        data$CycleStart[stop] <- "stop"
        data$cycles[stop:nrow(data)] <- NA
        data$REM.NREM[stop:nrow(data)] <- NA
      }else{
        # remove last NREMP
        stop <- cycs[length(cycs)]
        data$CycleStart[cycs[length(cycs)]] <- "stop"
        data$cycles[stop:nrow(data)] <- NA
        data$REM.NREM[stop:nrow(data)] <- NA
      }
    }
    return(data)
  },
  
#' @description Auxiliary function. Removes (N)REM or W at the end of the night if criteria for another (N)REMP are not fulfilled.
  
  clean_endofnight <- function(data){
    cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP") 
    # if last period is a REMP
    if (data$CycleStart[cycs[length(cycs)]] == "REMP"){ # if last period is a REMP
      REMs <- which(data$Descr3 == "REM") #which 30s epochs are REM
      if(tail(REMs, n = 1) == nrow(data)){ #check if the last REM is the last staged epoch
        stop <- tail(REMs, n = 1) # stop at last (REM) epoch
        data$CycleStart[stop] <- "stop"
      }else{
        stop <- tail(REMs, n = 1)+1 # stop after last REM epoch
        data$CycleStart[stop] <- "stop"
        data$cycles[stop:nrow(data)] <- NA #rm info following "stop"
        data$REM.NREM[stop:nrow(data)] <- NA
      }
    }else{
      # if last period is a NREMP
      lastNREMP <-  tail(which(data$CycleStart == "NREMP"),1)
      # find beginning of last continuous W
      Ws <- which(data$Descr3 == "W") #which 30s epochs are W
      Ws <- Ws[Ws > lastNREMP] # where are the W episodes following the last NREMP onset
      if (length(Ws) > 2){
        # find last continuous W episode (i.e., final W)
        Ws_start <- head(Ws,1)
        for (kk in 2:(length(Ws))){
          if (Ws[kk-1] != Ws[kk]-1){ 
            Ws_start <- c(Ws_start, Ws[kk])
          }
        }
        Ws_start <- tail(Ws_start,1) #last is the onset of the last wake episode
      }else{
        Ws_start <- c()
      }
      
      # check if last W is again followed by NREM
      NREMs <- which(data$Descr3 == "NREM") #which 30s epochs are NREM
      if (length(Ws_start) > 0){
        if(Ws_start < tail(NREMs,1))   Ws_start <- c()
      }
      
      # check if last epoch is W
      if (length(Ws_start) == 0 & data$Descr3[nrow(data)] == "W")   Ws_start <- nrow(data)
      
      # place stopping criterion if necessary
      if (length(Ws_start) > 0){
        data$CycleStart[data$CycleStart == "stop"] <- NA
        stop <- Ws_start
        data$CycleStart[stop] <- "stop"
      }
      
      #Now REMP could be last again > remove NREM/W following last REMP
      cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP") 
      if (data$CycleStart[cycs[length(cycs)]] == "REMP"){
        REMs <- which(data$Descr3 == "REM") #which 30s epochs are REM
        if(tail(REMs, n = 1) == nrow(data)){ #check if the last REM is the last staged epoch
          data$CycleStart[data$CycleStart == "stop"] <- NA
          stop <- tail(REMs, n = 1) # stop at last (REM) epoch
          data$CycleStart[stop] <- "stop"
        }else{
          data$CycleStart[data$CycleStart == "stop"] <- NA
          stop <- tail(REMs, n = 1)+1 # stop after last REM epoch
          data$CycleStart[stop] <- "stop"
          data$cycles[stop:nrow(data)] <- NA
          data$REM.NREM[stop:nrow(data)] <- NA
        }
      }
      
      # check if last NREMP is still >= 15min if a stopping criterion has been introduced
      check <- which(data$CycleStart == "stop")
      if (length(check)>0){
        if(stop - tail(which(data$CycleStart == "NREMP"),1)>=30){
          data$cycles[stop:nrow(data)] <- NA
          data$REM.NREM[stop:nrow(data)] <- NA
        }else{
          # if not, remove last NREMP completely
          data$CycleStart[tail(which(data$CycleStart == "NREMP"),1)] <- "stop"
          data$CycleStart[stop] <- NA
          data$cycles[tail(which(data$CycleStart == "stop"),1):nrow(data)] <- NA
          data$REM.NREM[tail(which(data$CycleStart == "stop"),1):nrow(data)] <- NA
          
          #now again remove NREM/W following last REMP
          cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP") 
          if (data$CycleStart[cycs[length(cycs)]] == "REMP"){
            REMs <- which(data$Descr3 == "REM") #which 30s epochs are REM
            stop <- REMs[length(REMs)]+1 # stop after last REM epoch
            data$CycleStart[stop] <- "stop"
            data$cycles[stop:nrow(data)] <- NA
            data$REM.NREM[stop:nrow(data)] <- NA
          }
        }
      }
    }
    return(data)
  }
)
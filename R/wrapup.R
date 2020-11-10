wrapup <- list(

addinfo1 <- function(data){
  # add cycle markers to file (only for NREM cycles)
  NREMPs <- which(data$CycleStart == "NREMP") #where do NREMPs begin?
  data$cycles <- NA
  for (k in 1:(length(NREMPs))){
    if (k<length(NREMPs)){
      data$cycles[NREMPs[k]:(NREMPs[k+1]-1)] <- k 
    }else{
      data$cycles[NREMPs[k]:(nrow(data))] <- k 
    }
  }
  
  # add NREM vs. REM part info
  cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
  data$REM.NREM <- NA
  for (k in 1:(length(cycs))){
    if (k<length(NREMPs)){
      if (data$CycleStart[cycs[k]] == "NREMP"){ 
        data$REM.NREM[c((cycs[k]):((cycs[k+1])-1))] <- 0
      }else if (data$CycleStart[cycs[k]] == "REMP"){
        data$REM.NREM[c((cycs[k]):((cycs[k+1])-1))] <- 1
      }
    }else{
      if (data$CycleStart[cycs[k]] == "NREMP"){ 
        data$REM.NREM[c((cycs[k]):(nrow(data)))] <- 0
      }else if (data$CycleStart[cycs[k]] == "REMP"){
        data$REM.NREM[c((cycs[k]):(nrow(data)))] <- 1
      }
    }
  }
  return(data)
},
  
#' @description Merge information about the cycle number and about NREMP vs. REMP. Add percentiles of NREM & REM parts.
  
  addinfo2 <- function(data){
    # merge cycle marker & NREM/REM marker
    data$CycInfo <- paste(data$cycles, data$REM.NREM, sep = "")
    data$CycInfo[data$CycInfo == "NANA"] <- NA
    
    ## add percentiles of NREM & REM parts
    cycs <- na.omit(unique(data$CycInfo))
    data$perc <- NA
    for (k in 1:length(cycs)){
      length <- nrow(subset(data, data$CycInfo == cycs[k]))
      if(length >=10){ # only add percentiles if cycle is at least 10 epochs
        if ((length %% 10) == 0){
          seq <- rep(length/10,10)
          for (kk in 1:length(seq)){
            if (kk == 1){
              ind <- rep(kk, seq[kk])
            }else{
              ind <- c(ind, rep(kk, seq[kk]))
            }
          }
          data$perc[which(data$CycInfo == cycs[k])] <- ind
        }else{
          seq <- sample(c(rep(floor(length/10), (10-length %% 10)), rep(ceiling(length/10), length %% 10))) #construct shuffled sequence of floor/ceiling rounded percentiles that matches the length of the complete cycle
          for (kk in 1:length(seq)){
            if (kk == 1){
              ind <- rep(kk, seq[kk])
            }else{
              ind <- c(ind, rep(kk, seq[kk]))
            }
          }
          data$perc[which(data$CycInfo == cycs[k])] <- ind
        }
      }else{
        seq <- rep(1,length)
        data$perc[which(data$CycInfo == cycs[k])] <- seq
      }
    }
    
    return(data)
  },
  
#' @description Plots result of the Sleep Cycle detection & saves plot.
  
  plot_result <- function(data, filetype, name, svv){
    dfplot <- data
    dfplot$time <- seq(1,nrow(dfplot))
    # if (filetype == "vmrk"){
    #   dfplot$time2 <- (dfplot$Position/(1000/128))/(1000*60)
    # }
    dfplot$Description[dfplot$Description == 1] <- -1
    dfplot$Description[dfplot$Description == 2] <- -2
    dfplot$Description[dfplot$Description == 3] <- -3
    dfplot$Description[dfplot$Description == 5] <- 1
    dfplot$CycInfo[dfplot$CycInfo == 10] <- 1.5
    dfplot$CycInfo[dfplot$CycInfo == 11] <- 1.6
    dfplot$CycInfo[dfplot$CycInfo == 20] <- 1.8
    dfplot$CycInfo[dfplot$CycInfo == 21] <- 1.9
    dfplot$CycInfo[dfplot$CycInfo == 30] <- 1.5
    dfplot$CycInfo[dfplot$CycInfo == 31] <- 1.6
    dfplot$CycInfo[dfplot$CycInfo == 40] <- 1.8
    dfplot$CycInfo[dfplot$CycInfo == 41] <- 1.9
    dfplot$CycInfo[dfplot$CycInfo == 50] <- 1.5
    dfplot$CycInfo[dfplot$CycInfo == 51] <- 1.6
    dfplot$CycInfo[dfplot$CycInfo == 60] <- 1.8
    dfplot$CycInfo[dfplot$CycInfo == 61] <- 1.9
    dfplot$CycInfo[dfplot$CycInfo == 70] <- 1.5
    dfplot$CycInfo[dfplot$CycInfo == 71] <- 1.6
    dfplot$CycInfo <- as.numeric(dfplot$CycInfo)
    
    ggplot2::ggplot(dfplot, aes(x=time, y=Description, colour=Description)) + 
      theme_bw()+
      geom_point() +
      geom_line(aes(x=time, y=Description))+
      xlab("Time") +
      ylab("Sleep Stage")+
      # scale_y_continuous(limits = c(-3,5), labels = c("N3", "N2", "N1", "W", "REM", "", "", "", ""))+
      scale_y_continuous(limits = c(-3,2), breaks = c(-3, -2, -1, 0, 1, 2, 3, 4, 5), labels = c("N3", "N2", "N1", "W", "REM", "", "", "", ""))+
      scale_color_viridis(name = "Sleep Stage", option = "D")+
      geom_point(y = dfplot$CycInfo, size = 0.5, na.rm = T)
    
    savename <- paste(name, "plot.png", sep = "_")
    ggplot2::ggsave(file=paste(svv, savename, sep = "/"), width = 25, height = 15, units = "cm", dpi = 600) 
  }
)

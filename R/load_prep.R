#' @description Auxiliary function. Loads data (text files or Brain Vision Analyzer Marker files) containing the sleep staging results.

load_prep <- list(

load_data <- function(filetype, filename, treat_as_W, treat_as_N3, hd, sp){
  if (filetype == "vmrk"){
    header <- read.csv(filename, nrows = 1, header = F)
    data <- read.csv(filename, skip = 1) #each sleep stage refers to the 30s preceding the marker (irrespective of SR!)
    cycles <- data
    cycles[,1] <- "SleepCycle"
    cycles[,2] <- NA
    cycles$SleepStages <- data$Description
  }else if (filetype == "txt"){
    if (hd == "y"){ # does it have a header?
      data <- read.table(filename, header = T, sep = sp)
      for (z in 1:ncol(data)){
        if (length(unique(data[,z])) == 5){
          if (all(data[,2] %in% c(0,1,2,3,5))){
            colnames(data)[z] <- "Description"
            cycles <- data
            cycles$SleepStages <- data$Description
            colnames(cycles)[z] <- "SleepCycle"
            cycles$SleepCycle <- NA
            break
          }
        }else if (length(unique(data[,z])) == 6){
          if (all(data[,2] %in% na.omit(c(0,1,2,3,5,treat_as_W, treat_as_N3)))){
            colnames(data)[z] <- "Description"
            cycles <- data
            cycles$SleepStages <- data$Description
            colnames(cycles)[z] <- "SleepCycle"
            cycles$SleepCycle <- NA
            break
          }else{
            stop("Please check your file. The vector with the staging seems to contain other numbers than the sleep stages or the numbers you want to treat as a sleep stage.")
          }
        }else if (length(unique(data[,z])) == 7){
          if (all(data[,2] %in% na.omit(c(0,1,2,3,5,treat_as_W, treat_as_N3)))){
            colnames(data)[z] <- "Description"
            cycles <- data
            cycles$SleepStages <- data$Description
            colnames(cycles)[z] <- "SleepCycle"
            cycles$SleepCycle <- NA
            break
          }else{
            stop("Please check your file. The vector with the staging seems to contain other numbers than the sleep stages or the numbers you want to treat as a sleep stage.")
          }
        }
      }
    }else{
      data <- read.table(filename, header = F, sep = sp)
      for (z in 1:ncol(data)){
        if (length(unique(data[,z])) == 5){
          if (all(data[,2] %in% c(0,1,2,3,5))){
            colnames(data)[z] <- "Description"
            cycles <- data
            cycles$SleepStages <- data$Description
            colnames(cycles)[z] <- "SleepCycle"
            cycles$SleepCycle <- NA
            break
          }
        }else if (length(unique(data[,z])) == 6){
          if (all(data[,2] %in% na.omit(c(0,1,2,3,5,treat_as_W, treat_as_N3)))){
            colnames(data)[z] <- "Description"
            cycles <- data
            cycles$SleepStages <- data$Description
            colnames(cycles)[z] <- "SleepCycle"
            cycles$SleepCycle <- NA
            break
          }else{
            stop("Please check your file. The vector with the staging seems to contain other numbers than the sleep stages or the numbers you want to treat as a sleep stage.")
          }
        }else if (length(unique(data[,z])) == 7){
          if (all(data[,2] %in% na.omit(c(0,1,2,3,5,treat_as_W, treat_as_N3)))){
            colnames(data)[z] <- "Description"
            cycles <- data
            cycles$SleepStages <- data$Description
            colnames(cycles)[z] <- "SleepCycle"
            cycles$SleepCycle <- NA
            break
          }else{
            stop("Please check your file. The vector with the staging seems to contain other numbers than the sleep stages or the numbers you want to treat as a sleep stage.")
          }
        }
      }
    }
  }
  
  return(list(data, cycles))
},

#' @description Auxiliary function. Recodes markers to be treated as W or N3. Additionally, stages are recoded/combined for further processing.

prep_data <- function(data, treat_as_W, treat_as_N3){
  ##-- prep data for further processing
  # Recode markers to be treated as W
  if (!is.na(treat_as_W)){
    data$Description[data$Description == treat_as_W] <- 0
  }
  
  # Recode markers to be treated as N3
  if (!is.na(treat_as_N3)){
    data$Description[data$Description == treat_as_N3] <- 3
  }
  
  # Recode/combine stages
  data$Descr2 <- NA
  data$Descr2[data$Description == 1 | data$Description == 2 | data$Description == 0 | data$Description == 5] <- "RWN12"
  data$Descr2[data$Description == 3] <- "N3"
  data$Descr3 <- NA
  data$Descr3[data$Description == 1 | data$Description == 2 | data$Description == 3] <- "NREM"
  data$Descr3[data$Description == 0] <- "W"
  data$Descr3[data$Description == 5] <- "REM"
  
  return(data)
},

#' @description Auxiliary function. Finds the beginning of the first NREM period (>= 15min) at the beginning of the night and marks further potential (!) NREM periods.

find_NREMPs <- function(NREMWs, data){
  ## Find the first NREMP at the beginning of the night
  # check if the sequence of NREWM is continuous and the period is >=15min AND beginning is not wake
  NREMWs_start <- NA
  for (k in 1:(length(NREMWs)-29)){
    if ((all(seq(NREMWs[k],length.out = 30) == NREMWs[seq(k,k+29)])) & (data$Descr3[NREMWs[k]] != "W")){ 
      NREMWs_start <- c(NREMWs_start, NREMWs[k])
    }else{
      next
    }
  }
  NREMWs_start <- NREMWs_start[-c(1)] #first was NA, remove
  
  # find discontinuities in the sequence (= potential beginnings of new NREM period further into the night)
  NREMWs_start2 <- NREMWs_start[1] #NREMWs_start[1] = start of the first NREMP
  for (k in 1:(length(NREMWs_start)-1)){
    if ((NREMWs_start[k+1]-NREMWs_start[k])>1){
      NREMWs_start2 <- c(NREMWs_start2, NREMWs_start[k+1]) #if there is a discontinuity in the sequence, mark the beginning of a new NREM period
    }
  }
  return(NREMWs_start2)
},

#' @description Auxiliary function. Finds the beginning of the first REM period (no duration criterion) at the beginning of the night and marks further potential (!) REM periods.
find_REMPs <- function(REMs, REMP_length, data){
  ## Find REM episodes (first can be <5min, others have to be at least 5min)
  REMs <- which(data$Descr3 == "REM") #which 30s epochs are NREM
  REMs_start <- REMs[1] #set first REM epoch as beginning of first REMP as there's no duration criterion for first REMP
  for (k in 1:(length(REMs)-(REMP_length-1))){
    if (all(seq(REMs[k],length.out = REMP_length) == REMs[seq(k,k+(REMP_length-1))])){ # check if the sequence of min. 10 REM epochs is continuous
      REMs_start <- c(REMs_start, REMs[k])
    }
  }
  REMs_start <- unique(REMs_start)
  
  REMs_start2 <- REMs_start[1]  #REMs_start[1] = start of the first REMP (no duration criterion)
  for (k in 1:(length(REMs_start)-1)){
    if ((REMs_start[k+1]-REMs_start[k])>1){
      REMs_start2 <- c(REMs_start2, REMs_start[k+1]) #if there is an discontinuity in the sequence, mark the beginning of a new NREM sequence
    }
  }
  return(REMs_start2)
},

#' @description Auxiliary function. Deletes repetitions, i.e. if several NREMPs or REMPs come in a row
delete_reps <- function(data){
  ## remove several NREMPs or REMPs in a row
  rm <- NA
  cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
  for (k in 2:length(cycs)){
    if(data$CycleStart[cycs[k]] == data$CycleStart[cycs[k-1]])
      rm <- c(rm, cycs[k])
  }
  rm <- unique(rm)
  rm <- rm[c(-1)]
  return(rm)
}
)

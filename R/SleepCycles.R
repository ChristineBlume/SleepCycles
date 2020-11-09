#' @title Sleep Cycle Detection  
#'
#' @description Sleep cycles are largely detected according to the originally proposed criteria by Feinberg & Floyd (1979). 
#' NREM periods are periods starting with N1 with a minimal duration of 15min (can include W, up to <5min REM). 
#' REM following a NREM period automatically starts a potential REM period, however any REMP must be at least
#' 5min (except the first REMP). If a NREMP exceeds 120min in duration (excl. wake), it can be split into 2 parts. 
#' The new cycle then starts with the first N3 episode following a phase (>12min) with any other stage than N3, that is
#' a lightening of sleep (cf. Rudzik et al., 2020; Jenni et al., 2004; Kurth et al., 2010). The code makes suggestions where
#' to split. However, the code also offers the possibility to provide a numeric value for an epoch at which to split.
#' 
#' The function requires any sleep staging file with a column, in which the sleep stages are coded 
#' in the usual 0,1,2,3,5 (i.e., W, N1, N2, N3, REM) pattern. The user can define other integers to be handled as W or N3
#' (i.e. in the case stagings were done according to the old R&K criteria including S3 and S4). Further columns are not an issue.
#' Staging must be in 30s epochs. Besides text files, it can also handle marker files for the Brain Vision Analyzer (filetype = "txt" (default) or "vmrk").
#'
#' @details Besides sleep cycles (NREM-REM), the result also splits the NREM and REM parts of each cycle in percentiles. In case the 
#' length of a period is not divisible by 10 (i.e., 203 epochs), we added one epoch to percentiles in a randomized
#' fashion to reach the correct length of a period (i.e., 7 percentiles comprised 20 epochs, 3 comprised 21).
#' 
#' The code offers to choose whether incomplete cycles should be removed at the end of the night (rm_incompletecycs, default = F). Incomplete cycles are defined by cycles that are followed
#' by <5min NREM or W (e.g. because a participant is woken up).
#'
#' Although this is not encouraged, for some participants it may be necessary to decrease the minimum duration of REM from 5min to 4 or 4.5min
#' as otherwise a seemingly 'clear' REM period is skipped. While the default length of REMPs is 10 segments, it can be decreased.
#'
#' The user can either process all files in a given directory (default) or specific files by specifying a vector of files.
#'
#' By default, the function produces and saves a plot for visual inspection of the results.
#'  
#'  @param p character vector indicating the directory containing the sleep staging files
#'  @param files numeric vector indicating which files in 'p' to process. Default: NA
#'  @param filetype character indicating file type of the files containing the sleep staging results. Can be "txt" (default) or "vmrk" (i.e., marker files for Brain Vision Analyzer Software).
#'  @param treat_as_W numeric vector indicating which values should be treated as 'wake'. Default: NA
#'  @param treat_as_N3 numeric vector indicating which values should be treated as 'N3'. Default: NA
#'  @param rm_incompletecycs logical: should incomplete cycles at the end of the night be removed? Default: F.
#'  @param plot logical: should a plot for the result of the detection procedure be generated and saved? Default: T.
#'  @param REMP_length numeric value specifying the minimum duration of a REM period. Default is 10 segments (i.e. 5 minutes). Decreasing the min. length is not encouraged and should only be done following careful consideration.
#'
#'  @return Saves results of the detection in a results folder in 'p'. The resulting textfile contains the sleepstages in a column named 'SleepStages', the sleep cycles in 
#'  a column 'SleepCycles' (numeric value indicating the cycle number), information on whether it is a NREM or REM period (numeric value in column 'N_REM', 0 = NREM, 1 = REM), and an indicator of the percentiles
#'  of the (N)REM period of the cycle (numeric value in 'percentile' column; 1 = first percentile, 2 = second percentile, etc.). In case a (N)REM period is less than 10 epochs long,
#'  no percentiles are calculated (all epochs are coded as '1' in the 'percentile' column).
#'  
#'  @author Christine Blume, \email{christine.blume@@sbg.ac.at}
#'  @references{
#'  \insertRef{@article{doi:10.1111/j.1469-8986.1979.tb02991.x, author = {Feinberg, I. and Floyd, T. C.}, title = {Systematic Trends Across the Night in Human Sleep Cycles}, 
#'  journal = {Psychophysiology}, volume = {16}, number = {3}, pages = {283-291}, doi = {10.1111/j.1469-8986.1979.tb02991.x}, year = {1979}}}{SleepCycles}
#'  \insertRef{@article{doi:10.1093/sleep/zsz324, author = {Rudzik, Franziska and Thiesse, Laurie and Pieren, Reto and Héritier, Harris and Eze, Ikenna C and Foraster, Maria and Vienneau, Danielle and Brink, Mark and Wunderli, Jean Marc and Probst-Hensch, Nicole and Röösli, Martin and Fulda, Stephany and Cajochen, Christian},
#'  title = "{Ultradian modulation of cortical arousals during sleep: effects of age and exposure to nighttime transportation noise}",
#'  journal = {Sleep}, volume = {43}, number = {7}, year = {2020}, month = {02},
#'  abstract = "{The present study aimed at assessing the temporal non-rapid eye movement (NREM) EEG arousal distribution within and across sleep cycles and its modifications with aging and nighttime transportation noise exposure, factors that typically increase the incidence of EEG arousals.Twenty-six young (19-33 years, 12 women) and 16 older (52-70 years, 8 women) healthy volunteers underwent a 6-day polysomnographic laboratory study. Participants spent two noise-free nights and four transportation noise exposure nights, two with continuous and two characterized by eventful noise (average sound levels of 45 dB, maximum sound levels between 50 and 62 dB for eventful noise). Generalized mixed models were used to model the time course of EEG arousal rates during NREM sleep and included cycle, age, and noise as independent variables.Arousal rate variation within NREM sleep cycles was best described by a u-shaped course with variations across cycles. Older participants had higher overall arousal rates than the younger individuals with differences for the first and the fourth cycle depending on the age group. During eventful noise nights, overall arousal rates were increased compared to noise-free nights. Additional analyses suggested that the arousal rate time course was partially mediated by slow wave sleep (SWS).The characteristic u-shaped arousal rate time course indicates phases of reduced physiological sleep stability both at the beginning and end of NREM cycles. Small effects on the overall arousal rate by eventful noise exposure suggest a preserved physiological within- and across-cycle arousal evolution with noise exposure, while aging affected the shape depending on the cycle.}",
#'  issn = {0161-8105}, doi = {10.1093/sleep/zsz324}, url = {https://doi.org/10.1093/sleep/zsz324}, note = {zsz324}, eprint = {https://academic.oup.com/sleep/article-pdf/43/7/zsz324/33460937/zsz324.pdf},
#'  }}{SleepCycles}
#'  \insertRef{@article{doi:10.1093/sleep/27.4.774, title={Spectral analysis of the sleep electroencephalogram during adolescence},
#'  author={Jenni, Oskar G and Carskadon, Mary A},
#'  journal={Sleep},
#'  volume={27}, number={4}, pages={774--783}, year={2004}, publisher={Oxford University Press}}}{SleepCycles}
#'  \insertRef{@article{@article{doi:10.1523/JNEUROSCI.2532-10.2010 ,
#'  title={Mapping of cortical activity in the first two decades of life: a high-density sleep electroencephalogram study},
#'  author={Kurth, Salom{\'e} and Ringli, Maya and Geiger, Anja and LeBourgeois, Monique and Jenni, Oskar G and Huber, Reto},
#'  journal={Journal of Neuroscience},
#'  volume={30}, number={40}, pages={13211--13219}, year={2010},publisher= {Soc Neuroscience}}}}
#'  }
#'  
#'  @importFrom ggplot2 reshape2 plyr stringr viridis Rdpack reprompt
#'  
#'  @examples 
#'  data(sleepstages)
#'  newdir <- file.path(tempdir(),"SleepCycles_exmpl")
#'  dir.create(newdir, showWarnings = F)
#'  write.table(sleepstages, file = "sleepstages.txt", row.names=FALSE, col.names = FALSE)
#'  SleepCycles(newdir)
#'  
#'  @export
SleepCycles <- function(p, files = NA, filetype = "txt", treat_as_W = NA, treat_as_N3 = NA, rm_incompletecycs = F, plot = T, REMP_length = 10){
  
  # # --- set a few things
  setwd(p)

  # check if there are result files of this function in the directory as they will mess with the code
  # stop code execution if they are found
  x <- list.files(p, pattern = glob2rx("*SCycles.txt"))
  if (length(x)>0){
    stop("Please remove files from previous Sleep Cycle detections from the folder.")
  }
  
  #----- list all files in directory
  if (filetype == "vmrk"){
    d <- list.files(p, pattern = "*.vmrk")
  }else if (filetype == "txt"){
    d <- list.files(p, pattern = "*.txt")
    hd <- readline("Do your files have a header with row names (y/n)? ") #check if first line contains column namens
  }
  
  #----- has a vector for a subset of files to be processed been specified?
  if (!all(is.na(files))){
    d <- d[files]
  }
  
  #----- prepare results folder, create new directory
  sv <- paste("SleepCycles", Sys.Date(), sep = "_")
  dir.create(file.path(paste(p, sv, sep = "/")), showWarnings = FALSE)
  
  #--------------------------------------------------------
  #----- loop through files to determine sleep cycles
  #--------------------------------------------------------
  
  for (i in 1:length(d)){
    print(i) #tell user which file is processed
    filename <- d[i]
    
    ## load data
    D <- load_data(filetype, treat_as_W, treat_as_N3)
    data <- D[[1]]
    cycles <- D[[2]]
    rm(D)
    
    ##-- prep data for further processing
    data <- prep_data(data, treat_as_W, treat_as_N3)
    
    # Find NREM periods: start with N1 and can then also include W. >=15min
    NREMWs <- which(data$Descr3 == "NREM"| data$Descr3 == "W") #which 30s epochs are NREM or wake
    NREMs <- which(data$Descr3 == "NREM")
    NREMWs <- subset(NREMWs, NREMWs >= NREMs[1]) # exclude W at the beginning of the night before the first NREM epoch
    
    ## Loop through NREMWs 
    # check if the sequence of NREWM is continuous and the period is >=15min AND beginning is not wake -> first NREMP
    # Further: find discontinuities in the sequence (= potential beginnings of new NREM periods during the remaining night)
    NREMWs_start2 <- find_NREMPs(NREMWs)
    data$CycleStart <- NA
    data$CycleStart[NREMWs_start2] <- "NREMP" #marks all potential NREM period beginnings
    
    ## Find REM episodes (first can be <5min, others have to be at least 5min)
    REMs_start2 <- find_REMPs(REMs)
    data$CycleStart[REMs_start2] <- "REMP"
    
    ## remove several NREMPs or REMPs in a row
    rm <- delete_reps(data)
    data$CycleStart[c(rm)] <- NA
    
    ## is any NREM part (excl. wake) of a NREMP longer than 120min?
    toolong <- is.toolong(data)
    ## now split NREMPs that are too long
    if (length(toolong) > 0){
      data <- toolong_split(data, toolong)
    }
    
    ## now check again if there are still NREMPs > 120min
    ## is any NREM part (excl. wake) of a NREMP longer than 120min?
    rm(toolong)
    toolong <- is.toolong(data)
    
    ## now split NREMPs that are too long
    if (length(toolong) > 0){
      message("~ Still detected a NREMP > 120min. Let's go through the splitting process again. ~")
      data <- toolong_split(data, toolong)
    }

    # -----------------------------------------------------------------------------
    # now finish and add cycle markers and percentiles
    
    # add cycle markers to file (only for NREM cycles) & add NREM vs. REM part info
    data <- addinfo1(data)
    
    # remove incomplete NREM-REM cycle at the end of the night (i.e., cycles followed by <5min NREM or W)
    if (rm_incompletecycs == T){
      data  <- rm.incompletecycs(data)
    
    #remove NREM/W following last REMP (in case no new NREMP begins) or REM/W following last NREMP  (in case no new REMP begins)
    }else if (rm_incompletecycs == F){
      data <- clean_endofnight(data)
    }
    
    ## merge cycle marker & NREM/REM marker & add percentiles of NREM & REM parts
    data <- addinfo2(data)
    
    ## prep new marker file with cycle info
    if (filetype == "vmrk"){
      cycles <- cycles[,-c(4,5)]
    }
    cycles$Description <- data$cycle_perc
    cycles$SleepCycle <- data$cycles
    cycles$N_REM <- data$REM.NREM
    cycles$percentile <- data$perc
    
    ## save new file
    svv <- paste(p, sv, sep = "/")
    name <- unlist(stringr::str_split(filename, pattern = "_"))
    savename <- paste0(c(name[1:(length(name)-1)]), sep = "_", collapse = "")
    savename <- paste(savename, "SCycles.txt", sep = "")
    write.table(cycles, file = paste(svv, savename, sep = "/"), row.names = F)
    
    ## plot results if desired
    if (plot == T){
      plot_result(data, filetype)
    }
  }
}

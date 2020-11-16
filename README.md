The SleepCycles package
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Package and main function

The SleepCycles package and its main function `SleepCycles()` is
designed to detect sleep cycles and their respective NREM and REM parts
(called (N)REM periods) from data that has been sleep staged according
to AASM criteria. Additionally, each (N)REM part is split into
percentiles. The function results in a plot visualising the results and
creates a text file so the results can be used for further processing.

### Detection of sleep cycles

Sleep cycles are largely detected according to the originally proposed
criteria by Feinberg & Floyd (1979). NREM periods are periods starting
with N1 (or W following a REM period) with a minimal duration of 15min
(can include W, up to \<5min REM, except for the first REMP, for which
there is no minimum duration criterion). REM following a NREM period
always represents a potential REM period, however any REMP must be at
least 5min long (except the first REMP, for which no minimum duration
criterion is applied). If a NREMP exceeds 120min in duration (excl.
wake), it can be split into 2 parts. The new cycle starts with the first
N3 episode following a phase (\>12min) with any other stage than N3,
that is a lightening of sleep (cf. Rudzik et al., 2020; Jenni et al.,
2004; Kurth et al., 2010). The function makes suggestions where
splitting could be done according to these criteria and visualises the
potential splitting points on top of a hypnogram. The user can then
interactively choose where to split the NREMP. However, the code also
offers the possibility to provide a numeric value for an epoch at which
to split or you can also decide to not split at all. A combination of a
NREMP and the following REMP represents one sleep cycle, except for the
case when a NREMP is split. In this case, the first of the two resulting
NREMPs represents a sleep cycle (without REM).

### Requirements

The function requires any sleep staging results file with a column, in
which the sleep stages are coded in the usual numeric 0,1,2,3,5 (i.e.,
W, N1, N2, N3, REM) pattern (i.e., a numeric vector). The user can
define other integers to be handled as W or N3 (i.e. in the case
stagings were done according to the Rechtschaffen & Kales criteria
including S3 and S4). The presence of further columns, e.g. a ‘time’
column, is not an issue. Staging must be in 30s epochs. Besides text
files, the `SleepCycles()` function can also handle marker files for the
Brain Vision Analyzer. The input file type can be indicated with the
`filetype` argument (`filetype = "txt"` (default) or `filetype =
"vmrk"`).

#### Details

Besides sleep cycles (NREM-REM), the result also splits the NREM and REM
parts of each cycle in percentiles. In case the length of a period is
not divisible by 10 (e.g., 203 epochs), one epoch is added to
percentiles in a randomized fashion to reach the correct length of a
period (here: 7 percentiles of 20 epochs, 3 of 21 epochs).

The code offers to choose whether incomplete periods should be removed
at the end of the night (argument `rm_incomplete_period`, default =
FALSE). Incomplete periods are defined by periods that are followed by
\<5min NREM or W (e.g. because a participant is woken up).

Although this is not encouraged, for some participants it may be
necessary to decrease the minimum duration of REM from 5min to 4 or
4.5min as otherwise a seemingly ‘clear’ REM period is skipped. While the
default length of REMPs is 10 segments, it can be decreased.

The user can either process all files in a given directory (default) or
specific files by specifying a vector of files (argument `files`).

By default, the function produces and saves a plot for visual inspection
of the results (argument `plot`, default = TRUE).

#### Arguments in SleepCycle function

`p` character vector indicating the directory containing the sleep
staging files  
`files` numeric vector indicating which files in ‘p’ to process.
Default: NA  
`filetype` character indicating file type of the files containing the
sleep staging results. Can be “txt” (default) or “vmrk” (i.e., marker
files for Brain Vision Analyzer Software).  
`treat_as_W` numeric vector indicating which values should be treated as
‘wake’. Default: NA  
`treat_as_N3` numeric vector indicating which values should be treated
as ‘N3’. Default: NA  
`rm_incomplete_period` logical: should incomplete periods at the end of
the night be removed? Default: F.  
`plot` logical: should a plot for the result of the detection procedure
be generated and saved? Default: T.  
`REMP_length` numeric value specifying the minimum duration of a REM
period. Default is 10 segments (i.e. 5 minutes). Decreasing the min.
length is not encouraged and should only be done following careful
consideration

### Worked example

First, we install and load the package if we haven’t done so. The
package is currently still hosted on GitHub, wherefore installation
requires the `devtools` package to be installed first. Note that
`install.github` will automatically check for changes.

``` r
## First, we save your current workspace
save.image(file=paste(tempdir(), "currsession.RData", sep = "/"))
## make sure you start with a clean session, loaded packages might cause problems.
rm(list = ls(all = TRUE))
install.packages("devtools", repos = "http://cran.us.r-project.org")
library(devtools)
devtools::install_github("ChristineBlume/SleepCycles")
```

Then, we are ready to use the package on our data set. Note that in the
`sleepstages2` data set, the first NREM period exceeds 120 minutes.
Thus, the code attempts to split this NREM period.

The text file has a header, thus, when asked whether it has a header
file, type `y`. Columns are separated by comma, thus type `,` when
prompted. Of course, if we had several of these files in our directory,
they would all have to have the same pattern.

##### Create directory & save data file

First, we load the `sleepstages2` file that comes with the package and
create a directory, where we save it.

``` r
library(SleepCycles)
data(sleepstages2)

## save current working directory so we can reset this later.
olddir <- getwd()

## create a new directory in the temporary directory (don't worry, it will automatically be deleted  
## when you restart your computer)
newdir <- file.path(tempdir(),"SleepCycles_exmpl2")
dir.create(newdir, showWarnings = FALSE)

## write the sleepstages2 file to this new directory
write.table(sleepstages2, file = paste(newdir, "sleepstages2.txt", sep = "/"),
row.names=FALSE, col.names = TRUE, quote = FALSE, sep = ",")
```

##### Run the detection

Then, we apply the actual `SleepCycles` function. The file contains
column names in a header and columns are separated with a comma. When we
are prompted, we have to decide where we want to split the data, either
at the first or the second suggested location. I would suggest selecting
the fist, so when prompted, we simply type `1`.

``` r
SleepCycles::SleepCycles(newdir, filetype = "txt")

## We again load the workspace image from before the code above was executed
save.image(file=paste(tempdir(), "currsession.RData", sep = "/"))

## we set the directory back to the one we were using before as we were just working in the  
## temp directory.
setwd(olddir)
```

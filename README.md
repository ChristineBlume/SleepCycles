The function requires any sleep staging file with a column named "Description", in which the sleep stages are coded 
in the usual 0,1,2,3,5 (i.e., W, N1, N2, N3, REM) pattern. The user can define other integers to be handled as W or N3
(i.e. in the case stagings were done according to the old R&K criteria including S3 and S4).
Staging must be in 30s epochs. Besides text files, it 
can also handle marker files for the Brain Vision Analyzer (filetype = "txt" (default), "vmrk", or "vpd").

Sleep cycles are largely defined according to the originally proposed criteria by Feinberg & Floyd (1979). 
NREM periods are periods starting with N1 with a minimal duration of 15min (can include W, up to <5min REM). 
REM following a NREM period automatically starts a potential REM period, however any REMP must be at least
5min (except the first REMP). If a NREMP exceeds 120min in duration (excl. wake), it can be split into 2 parts. 
The new cycle then starts with the first N3 episode following a phase (>12min) with any other stage than N3, that is
a lightening of sleep (cf. Rudzik et al., 2020; Jenni et al., 2004; Kurth et al., 2010). The code makes suggestions where
to split. However, the code also offers the possibility to choose an different epoch to split.

Besides sleep cycles (NREM-REM), the result also splits the NREM and REM parts of each cycle in percentiles. In case the 
length of a period is not divisible by 10 (i.e., 203 epochs), we added one epoch to percentiles in a randomized
fashion to reach the correct length of a period (i.e., 7 percentiles comprised 20 epochs, 3 comprised 21).

The code offers to choose whether incomplete cycles should be removed at the end of the night (rm_incompletecycs, default = F). 

Athough this is not encouraged, for some participants it may be necessary to decrease the minimum duration of REM from 5min to 4 or 4.5min
as otherwise a seemingly 'clear' REM period is skipped. While the default length of REMPs is 10 segments, it can be decreased.

The user can either process all files in a given directory (default) or specific files by specifying a vector of files.

By default, the function produces and saves a plot for visual inspection of the results.







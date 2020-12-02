## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs or NOTES. 

## Checks using R-hub

There was 1 NOTE:

* Possibly mis-spelled words in DESCRIPTION:
    Blume (10:178)
    Cajochen (10:186)
    Feinberg (10:97)

  This is a spurious NOTE.
  
*  Found the following (possibly) invalid URLs:
  URL: arXiv:10.31219/osf.io/r2q8v
    From: man/SleepCycles.Rd
    Message: Invalid URI scheme
      
  This is a doi entered in the format <arXiv:> as instructed.
  
*  The Description field contains the following (possibly) invalid arXiv id:
    10.31219/osf.io/r2q8v
    
  This is a doi entered in the format <arXiv:> as instructed.

* This is a new release.

## Downstream dependencies
I have also run R CMD check on downstream dependencies.

## Comments resulting from first submission to CRAN (v 1.0.0)
All but one comments and suggestions have been fixed. Thanks for the checks.
Dontrun cannot be replaced by donttest as this example requires interaction between the user and the example. The missing input will result in an error.
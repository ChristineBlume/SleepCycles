## Test environments
* local R installation, R 4.0.4
* ubuntu 16.04 (on travis-ci), R 4.0.4
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs or NOTES. 

## Checks using R-hub

There were no ERRORs or WARNINGs or NOTES. 

* This is a new release.

## Downstream dependencies

I have also run R CMD check on downstream dependencies.

## Comments resulting from previous submission to CRAN (v 1.0.1)

There was 1 NOTE:

* Possibly mis-spelled words in DESCRIPTION:
    Blume (10:178)
    Cajochen (10:186)
    Feinberg (10:97)

  This was a spurious NOTE.

## Comments resulting from first submission to CRAN (v 1.0.0)

All but one comments and suggestions have been fixed. Thanks for the checks.
Dontrun cannot be replaced by donttest as this example requires interaction between the user and the example. The missing input will result in an error.

## Test environments
* local R installation, R 4.1.0
* Ubuntu Linux 20.04.1 LTS, R-release, GCC, R 4.0.4
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs or NOTES.

## Checks using R-hub
There were no ERRORs or WARNINGs or NOTES.

## Downstream dependencies
I have also run R CMD check on downstream dependencies.

## Comments resulting from previous submission to CRAN (v 1.1.3)

There was an issue with vignette building, which sometimes resulted in a WARNING.

## Comments resulting from previous submission to CRAN (v 1.1.2)

There were no ERRORs or WARNINGs or NOTES. 

## Comments resulting from previous submission to CRAN (v 1.1.1)

There were no ERRORs or WARNINGs or NOTES. 

## Comments resulting from previous submission to CRAN (v 1.1.0)

There were no ERRORs or WARNINGs or NOTES. 

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

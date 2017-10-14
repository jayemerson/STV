## Test environments
* local OS X install, R 3.1.1
* travis-ci
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.  However, I was unable to get
the check completed on the win-builder because of an error
related to 'testthat' needing package 'crayon'.

## Downstream dependencies
None.  This is a new package that is unlikely to ever have
dependencies.
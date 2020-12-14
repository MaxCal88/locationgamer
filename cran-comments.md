## Test environments
* local OS X install, R 3.6.1
* ubuntu 16.04 (on travis-ci), R 3.6.1
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

The note reads:

Possibly mis-spelled words in DESCRIPTION:
    Equilibria (3:22)
    Hotelling (7:73)
  
  New submission
  
  Found the following (possibly) invalid DOIs:
    DOI: 10.2307/2224214
      From: DESCRIPTION
      Status: Forbidden
      Message: 403
      
I checked the DOI on DOI.org and it references the intended paper of Hotelling "Stability in Competition".

* This is a resubmission.
* This is a new release.

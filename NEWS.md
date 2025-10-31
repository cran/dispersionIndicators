# dispersionIndicators 0.1.0

* Initial package version.

# dispersionIndicators 0.1.1
* Fixed some issues pointed out by CRAN team members.
  * Writing functions in examples/vignettes/tests to tmp files
  * More explicit copyrights
  * Adding references in the DESCRIPTION file

# dispersionIndicators 0.1.2
* Fixed some issues pointed out by CRAN team members.
  * Make tests pass for mac OS (issue with generated files checksum)

# dispersionIndicators 0.1.3
* Fixed some issues encountered on CRAN checking pipelines.
  * Replace test of outfile files for convex functions from pdf files to png
    files (obtained from the pdf files). This avoids issues of unstabilities of
    pdf content (due to e.g. software versions).

# dispersionIndicators 0.1.4
* Fixed some issues encountered on CRAN checking pipelines.
  * The test on output files for convex functions was disabled for unstable version of R under windows OS, due to 
    too much instabilities observed on CRAN pipelines.
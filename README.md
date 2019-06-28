# RxpsG
Processing tool for X-ray Photoelectron Spectroscopy Data

RxpsG installation instructions
1) Install R and Rstudio on your computer
2) Run Rstudio and open the Tools menu -> Global Options
3) For Windows Systems set the R version path (es. C:\Program Files\R\R-3.6.0)
4) Install the devtools package from CRAN
5) Load the devtools package using the command: library(devtools)
6) Use install_github("GSperanza/RxpsG")

Manual Installation:
1) dowload the RxpsG_xx.xx.tar.gz package
2) Run RStudio
2) install all the RxpsG dependencies using:
   install.packages(pkgs=c("baseline", "digest", "FME", "gWidgets2 (>= 1.0-7)", "gWidgets2tcltk",
                    "latticeExtra", "memoise", "minpack.lm", "NORMT3", "signal", "sm", "SparseM",
                    "tkrplot", "wavelets" )
3) Under RStudio open the Tools menu -> Install Packages -> Package Archive File ( *.tar.gz )
4) install RxpsG browsing the downloaded Rxpsg.xx.xx.tar.gz file 

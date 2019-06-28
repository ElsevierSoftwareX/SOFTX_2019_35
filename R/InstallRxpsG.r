# INstallation macro: installs libraries and RxpsG software from correct URL
# G. Speranza 25-11-2018
#
#

InstRxpsG <- function(){



    cat("\n Before installing RxpsG package be sure that:")
    cat("\n => Connection to internet OK")
    cat("\n => RxpsG package dowloaded from FBK URL\n")
    cat("\n press enter to continue,  x to exit")
    answ<-readline(prompt="") #input of RStudio path
    if (answ=="x" || answ=="X"){ return(cat("\n==> Installation stopped!")) }

cat("\n ==> Installing non-standard libraries: it can take a few seconds...\n\n")

#list of packages to be installed
    packages<-c("baseline", 
                "digest",
                "coda", "deSolve", "minqa", "Rcpp", "rootSolve", "FME",
                "lattice", 
                "RColorBrewer", "latticeExtra",
                "MASS",
                "memoise", 
                "minpack.lm",
                "NORMT3",
                "signal",
                "sm",
                "SparseM",
                "wavelets", 
                "tkrplot",
                "methods", "gWidgets2", "gWidgets2tcltk")
    SysInfo<-Sys.info()
    sys<-SysInfo[[1]]
    usr<-SysInfo[[7]]
    Lpth<-.libPaths()
    LL <- length(Lpth)
    for (ii in 1:LL){
        pkgName<-paste(Lpth[ii],"/boot", sep="")   #check existence default library
        if (file.exists(pkgName)){
           break
        } else {
           cat("\n Please check R installation: default library not found!")
           return()
        }
    }
    Lpth <- Lpth[ii]
    LL<-length(packages)
    for (ii in 1:LL){
        pkgName<-paste(Lpth,"/",packages[ii], sep="")
#check if packages are already installed then update
        if (file.exists(pkgName)){
           cat("\n ==> Updating package: ", packages[ii])
           update.packages(lib.loc = Lpth,
                 oldPkgs = packages[ii],
                 repos = "https://cloud.r-project.org",  #getOption("repos")[[1]],
                 checkBuilt = TRUE,
                 ask = FALSE,
                 type = getOption("pkgType"))
        } else {
#install the packages from R CRAN URL
           cat("\n ==> Installing package: ", packages[ii])
           install.packages(pkgs=packages[ii], lib=Lpth,
                 repos = "https://cloud.r-project.org",  
#                 dependencies = FALSE,
#                 type = getOption("pkgType"),
#                 configure.args = getOption("configure.args"),
#                 configure.vars = getOption("configure.vars"),
#                 clean = TRUE,
                 verbose = TRUE
           )      
        }
    }

    cat("\n\n ==> Select the RxpsG Tar.Gz package")
    TarGzPack<-file.choose(default = "", caption = "Select RxpsG package:", multi = FALSE, filters = "tar.gz")
    TarGzPack<-gsub("\\\\", "/", TarGzPack) # change \\ in / in the pathname

#RxpsG installation: the Tar.Gz file contains non standard folders requiring untar command.
    untar(tarfile=TarGzPack, exdir=.libPaths(), verbose=TRUE)
    cat("\n ==> RxpsG package installed")

#--- Only for windows systems
    if (SysInfo[[1]]=="windows" || SysInfo[[1]]=="Windows"){
       cat("\n ==> Windows operating system detected: configuring")
       RxpsGpth<-find.package("RxpsG")

# Modify autoexec file for Icon_desktop_shortcut
       SourceFile<-paste(RxpsGpth,"/RxpsGProfile/RxpsG.bat", sep="")  #location of the batch file in the RxpsG library
       cat("\n ==> Install batch file")
       if (file.exists(SourceFile) ){ #if batch file exists remove to install a new one
          file.remove(SourceFile)
       }

       SourceDir<-"c:/Program Files/RStudio"  #default locaton of RStudio
       if(file.exists("c:/Program Files/RStudio/bin/rstudio.exe") == FALSE){
          cat("\n WARNING: non-conventional RStudio installation. Please select the RStudio folder location")
          SourceDir<-choose.dir(default = "", caption = "Select RStudio Directory")
          SourceDir<-gsub("\\\\", "/", SourceDir) # change \\ in / in the pathname
       }
       SourceDir<-paste(SourceDir, "/bin/", sep="")
       file.create(SourceFile) #create the batch file in the Rdata/Profile folder
       fp<-file(SourceFile, open="w") #open the batch file
       cat("path ", SourceDir, "\n", "rstudio.exe", sep="", file=fp)  #write the batch commands
       close(fp)

       cat("\n ==> Create desktop shortcut")
       usr<-SysInfo[[7]]
       SourceFile<-paste(RxpsGpth,"/RxpsGProfile/RxpsG_Shortcut.lnk", sep="")
       DestFile<-paste("c:/Users/",usr,"/Desktop/RxpsG_Shortcut.lnk", sep="")
       file.copy(SourceFile,DestFile)
       cat("\n ==> Shortcut to RxpsG saved on Desktop\n\n")
       cat("\n ==> RxpsG installation done!")

    }
}




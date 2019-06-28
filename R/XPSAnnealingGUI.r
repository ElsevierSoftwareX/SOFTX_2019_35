#Annealing GUI:

#'Displays the trend of the ratio of core line intensities or Fit component intensities
#'as a function of the annealing temperature associated to a given XPSSample. Annealing temperature
#'is manually provided to the GUI.
#'A fit using an exponential decay function is then used to fit the data
#'
#'
#'@examples
#'
#'\dontrun{
#'	DepthPro()
#'}
#'
#'@export
#'

AnnPro<-function(){


   CheckCL <- function(){
       CLtestList <- CommonCL        #duplicate the CommonCL list
       N.XS <- length(CLlist)        #N. XPSSpectra selected
       for (ii in 1:N.XS){
           N.CL <- length(CommonCL)      #N. Coreline names
           for (jj in N.CL:1){
              CL <- unlist(strsplit(CommonCL[jj], "\\.")) #extract pattern to compare skipping the CoreLine index
              CL <- CL[2]
              xx <- grep(CL, CLlist[[ii]])#is CL string present in CLlist?
              if (length(xx)==0) {      #pattern CL not present in CLlist
                 CommonCL <<- CommonCL[-jj]  #drop the elements of CommonCL not present in CommonCL
              }
           }
       }
   }

   MakeBaseLine <- function(SampName, Object) {
        BasLinType <- NULL
        BgDeg <- NULL
        Symbol <- Object@Symbol

        Selectwin <- gwindow("DEFINE BASELINE", visible=FALSE, parent=window)
        SelectGrp <- ggroup(horizontal=FALSE, container=Selectwin)
        txt <- paste("NO Baseline found on ", Symbol, " of ", SampName, sep="")
        message <- glabel(txt, container=SelectGrp)
        font(message) <- list(weight="bold")
        gseparator(horizontal=TRUE, container=SelectGrp)
		  bg <- gradio(items=c("linear","Shirley", "polynomial", "spline"),  horizontal=TRUE, handler=function(h, ...){
                              BasLinType <- svalue(bg)
                              if (BasLinType=="polynomial"){
                                  enabled(PolyDeg) <- TRUE
                              } else {
                                  enabled(PolyDeg) <- FALSE
                              }
                      }, container=SelectGrp)
        PolyDeg <- gedit(initial.msg = "Polynom degree:", container=SelectGrp)
        enabled(PolyDeg) <- FALSE
		  gbutton("OK", handler=function(...){
		                        BasLinType <<- svalue(bg)
		                        BgDeg <<- as.numeric(svalue(PolyDeg))
                              dispose(Selectwin)
                      }, container=SelectGrp)
        visible(Selectwin) <- TRUE
        Selectwin$set_modal(TRUE)
        plot(Object)

        if (BasLinType == "linear" || BasLinType == "Shirley") {
           gmessage(msg="==> Set the Baseline Limits", title="HELP INFO", icon="info")
           plot(Object)
           pos<-locator(n=2, type="p", col="red", lwd=2)
           Object@Boundaries$x<-pos$x
           Object@Boundaries$y<-pos$y
           Object <- XPSsetRegionToFit(Object)
           Object <- XPSbaseline(Object, BasLinType, BgDeg, Wgt, splinePoints )
           Object <- XPSsetRSF(Object)
        } else if (BasLinType == "polynomial") {
           gmessage(msg="==> Set the Baseline Limits", title="HELP INFO", icon="info")
           plot(Object)
           pos<-locator(n=2, type="p", col="red", lwd=2)
           Object@Boundaries$x<-pos$x
           Object@Boundaries$y<-pos$y
           Object <- XPSbaseline(Object, BasLinType, BgDeg, Wgt, splinePoints )
        } else if (BasLinType == "spline") {
            splinePoints<-list(x=NULL, y=NULL)
            txt<-"==> LEFT click to set spline points; RIGHT to exit"
            gmessage(msg=txt, title="HELP INFO", icon="info")
            plot(Object)
            pos<-c(1,1) # only to enter in  the loop
            while (length(pos) > 0) {  #pos != NULL => mouse right button not pressed
                  pos<-locator(n=1, type="p", col=3, cex=1.5, lwd=2, pch=1)
                  if (length(pos) > 0) {
                      splinePoints$x <- c(splinePoints$x, pos$x)  # $x and $y must be separate to add new coord to splinePoints
                      splinePoints$y <- c(splinePoints$y, pos$y)
                  }
            }
            # Now make BaseLine
            decr<-FALSE #Kinetic energy set
            if (Object@Flags[1] == TRUE) {
               idx<-order(splinePoints$x, decreasing=decr)  #idx is the vector of permutations for ascending order
               splinePoints$x<-splinePoints$x[idx] #splinePoints$x[idx] = SplinePoints selected in ascending order
               splinePoints$y<-splinePoints$y[idx] #splinePoints$x[idx] = SplinPoints selected in ascending order
            }
            LL<-length(splinePoints$x)

            Object@Boundaries$x<-c(splinePoints$x[1],splinePoints$x[LL]) #set the boundaries of the baseline
            Object@Boundaries$y<-c(splinePoints$y[1],splinePoints$y[LL])
            Object <- XPSsetRegionToFit(Object)
            Object <- XPSbaseline(Object, BasLinType, BgDeg, Wgt, splinePoints )
            Object <- XPSsetRSF(Object)
        }
        Object@Baseline$baseline <- new("baseline",
              baseline = Object@Baseline$y,
              corrected = Object@RegionToFit$y-Object@Baseline$y,
              spectra = Object@RegionToFit$y,
              call = ""
            )
        Object@Baseline$type = BasLinType
        plot(Object)
        return(Object)
   }


   FitData <- function(){
            model <- as.numeric(svalue(DP.Model, index=TRUE))

# FitResiduals() at beginning are evaluated using the Start parameters. Then modFit() generates new parameters
# and calls the FitResiduals() with this new Parms. FitResiduals(Parms) calls the FitFunction() which now is
# evaluated using the new parameters FitParms. Then residuals estimated using the new Parms are evaluated.
# Following the Start format, also the new parameters are generated using the same names of variables.

            FitFunction <- function(Start, FitExpr){
                       if (model == 1) {
                          d <- Start["d"]
                          bkg <- Start["bkg"]
                       } else if (model == 2) {
                          d <- Start["d"]
                          A <- Start["A"]
                          Rgh <- Start["Rgh"]
                          blr <- Start["blr"]
                       }
                       FitCurve <- eval(parse(text=FitExpr))
                       return(FitCurve)
            }

            FitResiduals <- function(Parms) {
                       residuals <- DataToFit$y-FitFunction(Parms, FitExpr)
                       return(residuals)
            }

            FitMtd <- svalue(FitMethod, index=TRUE)
            ptol <- maxiter <- nprint <- NULL
            if (FitMtd ==  1){
               FitMtd <-"Marq"
               ctrl<-list(ptol= 1e-6, maxiter=1000, nprint=1)   #ctrl for Marquartd optimization
            } else if (FitMtd == 2) {
               FitMtd <-"Newton"
               ctrl<-list(ptol= 1e-6, maxiter=1000, nprint=1)   #ctrl for Newton
            } else if (FitMtd == 3){
               FitMtd <-"Port"
               ctrl<-list(rel.tol=1e-6, eval.max=200, iter.max=1000, trace=1)  # ctrl for Port
            } else if (FitMtd == 4) {
               FitMtd <-"CG"
               ctrl<-list(reltol=1e-6, iter.max=1000, trace=1) # ctrl for Nelder-Mead CG BFGS L-BFGS-B SANN
            } else if (FitMtd == 5) {
               FitMtd <-"SANN"
               ctrl<-list(reltol=1e-6, iter.max=1000, trace=1) # ctrl for Nelder-Mead CG BFGS L-BFGS-B SANN
            } else if (FitMtd == 6) {
               FitMtd <-"Pseudo"
               ctrl<-list(varleft=1e-6, numiter=1000, verbose=TRUE) # ctrl per Pseudo
            }
            FitExpr <- NULL

#--- Due to limited experimental data ModFit is applied for fitting
#    To avoid defining an XPSSample and a Coreline with ARXPS data stored,
#    Modfit is integrated in this GUI and called directly

               FitCurve <<- NULL
               bkg <- NULL
               Start <- c(d=0.1, bkg=0)  # c collects the set of parameters with their names
               Lower <- c(0, 0)
               Upper <- c(10, DataToFit$y[1])
               IniParms <<- data.frame(
                           row.names = c("d", "bkg"),
                           start = Start,
                           min = Lower,
                           max = Upper)
#               DataToFit <- list(x=c(90,74.14,68.6,60.2,49,40.1,31.5,25.2,20,18.7,17.4,17,16.8)*pi/180, #example of ARXPSData
#                                 y=c(2.65,2.62,2.62,2.6,2.6,2.55,2.5,2.4,2.28,2.2,2,1.58,1.1))
               AnnT <- DataToFit$x

#$$$$               FitExpr <- "bkg + R.Rsf * exp(-d/(La*sin(Tilt))) / (1-exp(-d/(Lb*sin(Tilt)) ) )"
               FitEstimation <<- modFit(f = FitResiduals, p = Start, lower=Lower, upper=Upper, method=FitMtd, control=ctrl)
               FitParms <<- FitEstimation$par
               d <- unlist(FitParms["d"])
               bkg <- unlist(FitParms["bkg"])

               cat("\n ----Best Fit Param----\n")
               print(FitParms)
               cat("\n ----------------------\n")

#$$$$               FitCurve <<- bkg + R.Rsf * exp(-d/(La*sin(Tilt))) / (1-exp(-d/(Lb*sin(Tilt)) ))
               Tilt <- Tilt*180/pi
               x <- cbind(Tilt, Tilt)
               y <- cbind(DataToFit$y, FitCurve)
               matplot(x, y, type="b", pch=16, cex=2, col=c("blue", "red"), xlab="Annealing T [deg.]", ylab="Fit [a.u.]")



            return(FitCurve)
   }

   SaveResults <- function(h,...){
         SaveWin<-gwindow("SAVE DEPTH PROFILE RESULTS", visible=FALSE)
         SaveGroup <- ggroup(label="", horizontal=FALSE, container=SaveWin)

         SaveFrame <-gframe(text=" OUTPUT DATAFILE ", spacing=5, container=SaveGroup)
         SaveObj1 <- glabel(SelectedFName, container=SaveFrame)
         SaveObj2 <- gedit("", initial.msg="Output File Name: ", container=SaveFrame)
         SaveObj3 <- gbutton(" OK " , handler=function(h, ...){
                           DestFile <- svalue(SaveObj2)
                           DestFile <- unlist(strsplit(DestFile, "\\."))
                           DestFile <- paste(DestFile[1], ".RData", sep="")  #Force the extension to ".RData"
                           dispose(SaveWin)

                           FNameOut <- new("XPSSample")
                           FNameList <- ""
                           FileNames <- NULL
                           CLnames <- NULL
                           kk <- 1
                           for (ii in 1:N.XS){  #Exstract the Selected Coreline from the list of selected XPSSamples
                               for (jj in 1:N.CL){
                                   FName <- get(SelectedFName[ii], envir=.GlobalEnv)
                                   FNameOut[[kk]] <- FName[[SelectedCL[jj]]] #load the selected coreline in a temporary XPSSample
                                   CLnames <- c(CLnames, SelectedCL[jj])
                                   kk <- kk+1
                               }
                               FileNames <- c(FileNames, FName@Filename) #for each XPSSample obtain the data FileName
                               FNameList <- paste(FNameList, SelectedFName[ii], sep=", ")
                           }
                           FNameOut[[kk]] <- new("XPSCoreLine") #create a new coreline structure to save ARXPS data (data to fit, best fit, thickness...

                           CLnames <- c(CLnames, "I1/I2")
                           if (N.CL == 1){      #initialize CLcomment when only 1 coreline selected: DepthProfile performed on CL fit components
                              CLcomment <- paste("coreline ", SelectedCL[1], "fit components: ", svalue(ComponentCK[[1]]), svalue(ComponentCK[[2]]), sep="")
                           } else if (N.CL == 2) {
                              CLcomment <- paste("corelines: ", SelectedCL[1], ", ", SelectedCL[2], sep="")  #initialize CLname when only 2 corelines selected
                           }
                           DPModel <- svalue(DP.Model)
                           FNameOut@Sample <- FName@Sample
                           FNameOut@Comments <- c(paste("==>ARXPS: depth profile analysis on files: ", FNameList, sep=""),
                                                     paste("==>Analysis performed on ", CLcomment, sep=""),
                                                     paste("==>",DPModel, " model used to fit the ratio of their spectral intensities", sep=""))
                           FNameOut@User <- FName@User
                           FNameOut@Filename <- FileNames
                           FNameOut@names <- CLnames
                           #Define Data Structure for Depth Profile fitting
                           DPModel <- "Classic Depth Profile Model"  #by default Classic method set as Model name
                           if (DPModel[1] == "Roughness") { newDPModel <- "Classic Depth Profile Model" }   #Roughness modified method set name DpthProfileRoughness
                           if (DPModel[1] == "Max") { DPModel <- "Maximum Entropy Depth Profile Model" } #Max Entropy method  set name DpthProfileEntropy

                           FNameOut[[kk]]@.Data[[1]] <- AnnT      #abscissa are the annealing temp
                           FNameOut[[kk]]@.Data[[2]] <- Ratio     #ordinate are the ratios
                           FNameOut[[kk]]@.Data[[3]] <- rep(1, length(AnnT))  #analyzer transfer function here unitary for all spectral data
                           FNameOut[[kk]]@RegionToFit$x <- AnnT      #abscissa are the annealing T
                           FNameOut[[kk]]@RegionToFit$y <- Ratio     #ordinate are the ratios
                           FNameOut[[kk]]@Baseline$x <- AnnT
                           FNameOut[[kk]]@Baseline$y <- Bkg <- rep(0, N.XS) 
                           FNameOut[[kk]]@Baseline$baseline <- new("baseline",
                                          baseline = matrix(data=AnnT, nrow=1),   #matrix() required by class("baseline") see baseline() function of R
                                          corrected = matrix(data=Ratio-min(Ratio),nrow=1),
                                          spectra = matrix(data=Ratio, nrow=1),
                                          call = match.call()
                                        )
                           FNameOut[[kk]]@Baseline$type = "Linear"
                           FNameOut[[kk]]@Fit$y <- FitCurve
                           FNameOut[[kk]]@Fit$fit <- FitEstimation
                           FNameOut[[kk]]@Boundaries <- list(x=range(AnnT), y= range(Ratio))
                           FNameOut[[kk]]@units <- c("Annealing Temperature (deg. C)", paste(SelectedSpect[2], SelectedSpect[1], sep="/")) #adopt the units and flags of the last loaded FName[[ coreline1 ]]
                           FNameOut[[kk]]@Flags <- c(FALSE, FALSE, FALSE, FALSE) #FName[[SelectedCL[1]]]@Flags
                           FNameOut[[kk]]@Info <- paste("ARXPS analysis using ", DPModel, " on ", paste(FileNames, collapse=" "), " XPSSamples", sep="")
                           FNameOut[[kk]]@Symbol <- "I1/I2"

                           #Add Fit Function to experim. data
                           FNameOut[[kk]] <- XPSaddComponent(FNameOut[[kk]], type = "Generic")
                           names(FNameOut[[kk]]@Components) <- "DP"
                           FNameOut[[kk]]@Components[[1]]@funcName <- "Depth_Profile"
                           FNameOut[[kk]]@Components[[1]]@description <- DPModel
                           FNameOut[[kk]]@Components[[1]]@label = "DP"

                           #Setting Fit Parameters
                           FNameOut[[kk]]@Components[[1]]@param <- IniParms
                           FNameOut[[kk]]@Components[[1]]@param$Fit <- FitParms
                           FNameOut[[kk]]@Components[[1]]@ycoor <- FitCurve
                           assign(DestFile, FNameOut, envir=.GlobalEnv)
#print(str(FNameOut[[kk]]))
                           plot(FNameOut)
            }, container=SaveFrame)
            visible(SaveWin) <- TRUE
      XPSSaveRetrieveBkp("save")
   }


#-------variables---
#---load list of file ID and correspondent FileNames
      rm(list = ls(envir = MyEnv, all.names = TRUE), envir = MyEnv)
      FNameList<-XPSFNameList()
      if (length(FNameList) == 0) { return() }
      SampID <- ""
      SpectList <- ""
      SourceFileList <- NULL
      SelectedFName <- NULL
      SpectDataX <- list()
      SpectDataY <- list()
      CLlist <- list()
      ComponentCK <- list() #list used to make widgets for selection of coreLine fit components
      CommonCL <- NULL #the list of Corelines of the first XPSSample used as reference
      SelectedCL <- NULL
      SelectedSpect <- NULL
      N.XS <- NULL     #number of selected XPSSamples
      N.CL <- NULL     #number of selected common lines
#      N.FN <- length(FNameList)

      AnnT <- NULL
      angles <- list()
      infoLab <- list()
      AreaComp1 <- NULL
      AreaComp2 <- NULL
      Ratio <- NULL
      PosMax1 <- NULL
      PosMax2 <- NULL
      rho <- NULL
      XrayE <- NULL
      BE <- NULL
      La <- NULL    #attenuation length element1    cannot use the name L1 problems in XPSFitLM
      Lb <- NULL    #attenuation length element2    cannot use the name L2 problems in XPSFitLM
      R.Rsf <- NULL
      FitEstimation <- NULL
      FitCurve <- NULL
      IniParms <- data.frame()
      FitParms <- NULL
      IniData <- FALSE


#####----main---
      AddWin <- gwindow("ARXPS: DEPTH PROFILE ANALYSIS", visible=FALSE)
      size(AddWin) <- c(600, 400)
      Addgroup <- ggroup(horizontal=FALSE, container=AddWin)
      NoteBK<-gnotebook(expand=TRUE, container = Addgroup)

      SelGroup <- ggroup(label="CORELINE SELECTION", horizontal=TRUE, spacing=5, container=NoteBK)

      layoutS1 <- glayout(homogeneous=FALSE, spacing=5, container=SelGroup)

      layoutS1[1,1] <-  AddFrame1 <- gframe("SELECT THE SOURCE XPS-SAMPLES", spacing=5, horizontal=TRUE, container=layoutS1)
      SourceFiles <- gcheckboxgroup(FNameList, selected=-1, horizontal=FALSE, handler=function(h,...){
                                   CLlist<<-NULL  #reset the CoreLine list to be checked
                                   txt<-NULL
                                   SelectedFName <<- svalue(SourceFiles)
                                   N.XS <<- length(SelectedFName)
                                   for (ii in 1:N.XS){
                                       CLlist[[ii]] <<- XPSSpectList(SelectedFName[ii])
                                       names(CLlist)[[ii]] <<- as.character(ii)
                                       txt <- paste(txt, paste(CLlist[[ii]], collapse="  "), "\n")  #transform vector of strings CLlist[] in one string and add carriage return
                                       svalue(CoreLineList) <- txt
                                   }
                         }, container = AddFrame1)

      layoutS1[1,2] <-  AddFrame2 <- gframe("CORE LINES LIST", spacing=5, horizontal=FALSE, container=layoutS1)
      CoreLineList <- glabel(text="", container=AddFrame2)  #just to not collapse AddFrame2

      layoutS1[2,1] <- Compare <-gbutton("COMPARE CORE LINE NAMES", handler=function(h, ...){
                                   CommonCL <<- XPSSpectList(SelectedFName[1])
                                   CheckCL()   #check for corelines common to selected XPSSamples
                                   delete(AddFrame3,CommonCoreLines)
                                   CommonCoreLines<<-gcheckboxgroup(CommonCL, selected=-1, horizontal=TRUE, handler=function(h,...){
                                                                          enabled(Compare) <- FALSE
                                                                          enabled(SelectCL) <- TRUE
                                                                   }, container=AddFrame3)
                         }, container=layoutS1)


      layoutS1[3,1] <-  AddFrame3 <- gframe("COMMON CORE LINES", spacing=5,  container=layoutS1)
      CommonCoreLines <- glabel("  ", container=AddFrame3)  #just to not collapse AddFrame3

      layoutS1[3,2] <- SelectCL <-gbutton("SELECT CORELINES", handler=function(h,...){
                               SlctdCL <- svalue(CommonCoreLines)
                               N.CL <<- length(SlctdCL)
                               if (N.CL > 2) {
                                  gmessage(msg="More than two core lines selected! Please check carefully.", title="WRONG CORE LINE SELECTION", icon="error")
                                  return()
                               } else if (N.CL == 1) {
                                  CoreLine <- unlist(strsplit(SlctdCL, "\\."))   #drop "NUMBER." at beginning of coreLine name
                                  CoreLine <- CoreLine[2]
                                  SelectedCL <<- CoreLine                  #save selected core lines
                                  txt <- paste("Only ", CoreLine, " selected! Do you want to analyze fitting components?", sep="")
                                  answ <- gconfirm(msg=txt, title="ANALYSIS ON FIT COMPONENTS", icon="warning")
                                  if (answ == "FALSE") {
                                      gmessage(msg="Please select the second core line and proceed", title="CORE LINE SELECTION", icon="warning")
                                      return()
                                  }
                                  enabled(SelectCL) <- FALSE
                                  tmp <- new("XPSSample",
                                             Project = " ",
                                             Comments = " ",
                                             User=Sys.getenv('USER'),
                                             Filename=" " )

                                  for(ii in 1:N.XS){ #Now load the selected coreline from XPSSamples at different Annealing Temperatures
                                     FName <- get(SelectedFName[ii], envir=.GlobalEnv)
                                     txt <- paste(SelectedFName[ii], " Fit Comp.: ", sep="")
                                     layoutFC[ii,1] <- glabel(txt, spacing=5, container=layoutFC)
                                     tmp[[ii]] <- FName[[CoreLine]] #load the selected coreline in a temporary XPSSample
                                     CompNames <- names(tmp[[ii]]@Components)  #List of names of the fitting components
                                     layoutFC[ii,2] <- ComponentCK[[ii]] <<- gcheckboxgroup(CompNames, horizontal = TRUE, checked=FALSE, container = layoutFC) #CoreLineComp ia an array
                                  }
                                  E.Units <- "Kinetic Energy [eV]"
                                  BE <<- FALSE #Kinetic energy scale for the XPSSamples
                                  if (FName[[CoreLine]]@Flags[1] == TRUE) {
                                      BE <<- TRUE        #BE, KE found for the last XPSSample holds also for the other
                                      E.Units <- "Binding [eV]"
                                  }
                                  plot(tmp)
                                  enabled(SelectFC) <- TRUE
                                  for (ii in 1:N.XS){
                                      AnnTlayout[ii, 1] <<- glabel(SelectedFName[ii], container=AnnTlayout)
                                      AnnTlayout[ii, 2] <<- angles[[ii]] <<- gedit("", initial.msg="Ann. Temp.", container=AnnTlayout)
#                                     AnnTlayout[ii, 3] <<- infoLab[[ii]] <<- glabel(" " , container=AnnTlayout)
                                  }
                               } else if (N.CL == 2) {
                                  RSF <- NULL
                                  #Store spectral data and info in the SpectData list
                                  for (ii in 1:N.CL){   #N.CL must be == 2
                                      CoreLine <- SlctdCL[ii]
                                      CoreLine <- unlist(strsplit(CoreLine, "\\."))#skip the CoreLine index
                                      CoreLine <- CoreLine[2]
                                      SelectedCL[ii] <<- CoreLine                  #save selected core lines name for labeling axis and messages
                                      RSF[ii] <- paste("RSF_", CoreLine, sep="")
                                      for (jj in 1:N.XS){                          #N.XS = number of selected XPS Samples to analyze
                                          FName<-get(SelectedFName[jj], envir=.GlobalEnv)
                                          SpectDataX[[CoreLine]][[jj]] <<- FName[[CoreLine]]@RegionToFit$x  #abscissa of selected spectrum
                                          if ( ! hasBaseline(FName[[CoreLine]]) ){ #Has a Baseline the selected coreline in the list of XPSSample?
                                             FName[[CoreLine]] <- MakeBaseLine(SelectedFName[jj], FName[[CoreLine]])         #if not, define the Baseline
                                             assign(SelectedFName[jj], FName, envir=.GlobalEnv) #save defined baseline in the .GlobalEnv
                                          }
                                          SpectDataY[[CoreLine]][[jj]] <<- FName[[CoreLine]]@RegionToFit$y-FName[[CoreLine]]@Baseline$y  #spectrum without baseline
                                      }
                                  }
                                  BE <<- FALSE #Kinetic energy scale for the XPSSamples
                                  if (FName[[CoreLine]]@Flags[1] == TRUE) {BE <<- TRUE} #BE, KE found for the last XPSSample holds also for the other
                                  for (ii in 1:N.CL){   #N.CL must be == 2
                                       SpectDataY[[RSF[ii] ]][1] <<- FName[[SelectedCL[ii]]]@RSF  #Relative Sensitivity Factor of the selected Coreline
                                  }
                                  #Structure of SpectDataY is
                                  # SpectDataY[[1]] = list of Corelines type 1 from XPSSamples at differet annealing Temperatures
                                  # SpectDataY[[2]] = list of Corelines type 2 from XPSSamples at differet annealing Temperatures 
                                  # SpectDataY[[3]] = list of RSF corresponding to Corelines type 1 from XPSSamples at differet annealing Temperatures
                                  # SpectDataY[[4]] = list of RSF corresponding to Corelines type 2 from XPSSamples at differet annealing Temperatures

                                  #Now is possible to construct the widget to associate the annealing Temperatures
                                  for (ii in 1:N.XS){
                                      AnnTlayout[ii, 1] <<- glabel(SelectedFName[ii], container=AnnTlayout)
                                      AnnTlayout[ii, 2] <<- angles[[ii]] <<- gedit("", initial.msg="Ann. Temp.", container=AnnTlayout)
#                                      AnnTlayout[ii, 3] <<- infoLab[[ii]] <<- glabel(" " , container=AnnTlayout)
                                  }
                                  enabled(SelectCL) <- FALSE
                                  enabled(FilmDens) <- TRUE
                                  enabled(Xenergy) <- TRUE
                                  enabled(Thickness) <- TRUE
                                  svalue(NoteBK) <- 2
                               }
                         }, container=layoutS1)

      layoutS1[4,1] <-  AddFrame4 <- gframe("FIT COMPONENTS", horizontal = FALSE, spacing=5,  container=layoutS1)
      layoutFC <- glayout(homogeneous=FALSE, spacing=5, container=AddFrame4)
      glabel("   ", container= AddFrame4)  #white spaces just to define the frame
      layoutS1[4,2] <- SelectFC <-gbutton("SELECT FIT COMPONENTS", handler=function(h,...){
                               #Store spectral data and info in the SpectData list
                               for(ii in 1:N.XS){ #Now load the selected coreline from XPSSamples at different annealing Temperatures
                                  CmpFit <- svalue(ComponentCK[[ii]], index=TRUE)
                                  if (length(CmpFit) != 2) {
                                      gmessage(msg = "Two component for each XPSSample has to be selected. Please control!", title="FIT COMPONENT SELECTION", icon = "warning")
                                      return()
                                  }
                                  FName <- get(SelectedFName[ii], envir=.GlobalEnv)
                                  SpectDataX[["C1"]][[ii]] <<- FName[[SelectedCL]]@RegionToFit$x  #abscissa of selected spectrum
                                  SpectDataY[["C1"]][[ii]] <<- FName[[SelectedCL]]@Components[[CmpFit[1]]]@ycoor  #spectrum without baseline
                                  SpectDataX[["C2"]][[ii]] <<- FName[[SelectedCL]]@RegionToFit$x  #abscissa of selected spectrum
                                  SpectDataY[["C2"]][[ii]] <<- FName[[SelectedCL]]@Components[[CmpFit[2]]]@ycoor  #spectrum without baseline
                                  MM <- max(SpectDataY[["C1"]][[ii]])
                                  MM <- findYIndex(SpectDataY[["C1"]][[ii]], MM, 0.001) #index corresponding to the max
                                  MM <- MM[1]
                                  SpectDataX[["PosMaxC1"]][[ii]] <<- SpectDataX[["C1"]][[ii]][MM] #position of the component1 max
                                  MM <- max(SpectDataY[["C2"]][[ii]])
                                  MM <- findYIndex(SpectDataY[["C2"]][[ii]], MM, 0.001) #index corresponding to the max
                                  MM <- MM[1]
                                  SpectDataX[["PosMaxC2"]][[ii]] <<- SpectDataX[["C1"]][[ii]][MM] #position of the component1 max
                               }
                               SpectDataY[["RSF_C1"]][1] <<- FName[[SelectedCL]]@RSF  #Relative Sensitivity Factor of the selected Coreline
                               SpectDataY[["RSF_C2"]][1] <<- FName[[SelectedCL]]@RSF  #Relative Sensitivity Factor of the selected Coreline
                               if (SpectDataX[["PosMaxC1"]][[1]] < SpectDataX[["PosMaxC2"]][[1]]) {
                                  SelectedCL[3] <<- "Low Energy Fit Comp."      #SelectedCL[3], SelectedCL[4] labels for axis and messages
                                  SelectedCL[4] <<- "High Energy Fit Comp."     #SelectedCL[1], SelectedCL[2] reserved for CoreLine Names
                               } else {
                                  SelectedCL[3] <<- "High Energy Fit Comp."
                                  SelectedCL[4] <<- "Low Energy Fit Comp."
                               }

                               for (ii in 1:N.XS){
                                   AnnTlayout[ii, 1] <<- glabel(SelectedFName[ii], container=AnnTlayout)
                                   AnnTlayout[ii, 2] <<- angles[[ii]] <<- gedit("", initial.msg="Ann. Temp.", container=AnnTlayout)
                               }
                               enabled(SelectFC) <- FALSE
                               enabled(FilmDens) <- TRUE
                               enabled(Xenergy) <- TRUE
                               enabled(Thickness) <- TRUE
                               svalue(NoteBK) <- 2
                         }, container=layoutS1)
      enabled(SelectFC) <- FALSE

#---Thickness Estimation

      EstimGroup <- ggroup(label="THICKNESS ESTIMATION", horizontal=TRUE, spacing=5, container=NoteBK)

      AnnTGroup <- ggroup(horizontal=FALSE, spacing=5, container=EstimGroup)
      message <- glabel("Set Annealing Temperatures of XPSSamples (deg. C)", container=AnnTGroup)
#      font(message) <- list(weight="bold")
      gseparator(horizontal=TRUE, container=AnnTGroup)
      AnnTlayout <- glayout(homogeneous=FALSE, spacing=3, container=AnnTGroup)
      gseparator(horizontal=FALSE, container=AnnTGroup)
      Infolayout <- glayout(homogeneous=FALSE, spacing=3, container=AnnTGroup)
      Infolayout[1, 1] <- message1 <- glabel("   Film density in Kg/m^3   ", container=Infolayout)
      Infolayout[1, 2] <- message2 <- glabel("X-ray Excitation Energy (eV)", container=Infolayout)
      Infolayout[2, 1] <- FilmDens <- gedit("", initial.msg="Coating Density", container=Infolayout)
      Infolayout[2, 2] <- Xenergy <- gedit("1486.6", initial.msg="", container=Infolayout)
      enabled(FilmDens) <- FALSE
      enabled(Xenergy) <- FALSE                    

      gseparator(horizontal=FALSE, container=AnnTGroup)
      glabel("Thickness estimation methods", spacig=3, container=AnnTGroup)
      DP.Model <- gradio(c("Da Implementare 1", "Da Implementare 2"), spacig=3, selected=1, horizontal=TRUE, handler = function(h,...){
                               enabled(Thickness)<-TRUE
                         }, container=AnnTGroup)
      glabel("Residual minimization methods", spacig=3, container=AnnTGroup)
      FitMethod <- gradio(c("Marquardt", "Newton", "Port", "Conj.Gradient", "SANN", "Pseudo"), selected=1, spacig=3, horizontal=TRUE, container=AnnTGroup)

      Thickness <- gbutton("START ESTIMATION", handler=function(...){
                               if (IniData ==FALSE){ #initializes data for thickness estimation
                                  if (length(rho)==0 || length(XrayE)==0 || is.na(sum(AnnT))){  #if annealing Temp., rho, XrayE not already initialized
                                     rho <<- as.numeric(svalue(FilmDens))
#                                     XrayE <<- as.numeric(svalue(Xenergy))
                                     for (ii in 1:N.XS){
                                        AnnT[ii] <<- as.numeric(svalue(angles[[ii]]))
                                     }
#                                     if (length(rho)==0 || length(XrayE)==0 || is.na(sum(AnnT))){  #if AnnT contains NA (i.e. lacking angle) sum(AnnT)==NA
                                     if (length(rho)==0 || is.na(sum(AnnT))){  #if AnnT contains NA (i.e. lacking T) sum(AnnT)==NA
                                        gmessage(msg="INFORMATION LACKING! Please control and fill in all the items", title = "WARNING", icon="warning")
                                        return()
                                     }
                                  }
                                  idx <- order(AnnT, decreasing=TRUE)
                                  AnnT <<- AnnT[idx] #annealing T ordered in decreasing order
                                  #order spectral data correspondently
                                  SpectDataX[[1]] <<- SpectDataX[[1]][idx] #correspondent SpectralData applying the permutations done for Annealing Temp vector
                                  SpectDataX[[2]] <<- SpectDataX[[2]][idx]
                                  SpectDataY[[1]] <<- SpectDataY[[1]][idx]
                                  SpectDataY[[2]] <<- SpectDataY[[2]][idx]
                                  SelectedFName <<- SelectedFName[idx] #also the list of XPSSamples is ordered with correspondence to the Annealing T
                                  for (ii in 1:N.XS){
                                      AreaComp1[ii] <- sum(SpectDataY[[1]][[ii]]) # SpectData[jj, ii]: jj runs on N.Corelines = 2, ii runs on N XPSSample = N.XS
                                      AreaComp2[ii] <- sum(SpectDataY[[2]][[ii]]) # This is the integral of selected Coreline 2 of the XPSSample ii
                                      Ratio[ii] <<- AreaComp1[ii]/AreaComp2[ii]
                                      MM <- max(SpectDataY[[1]][[ii]])       #max of selected corelines1 or fit component1
                                      MM <- findYIndex(SpectDataY[[1]][[ii]], MM, 0.001)
                                      MM <- MM[1] #index relative to the max CL1 can be a vector depending spectral noise and if precision 0.001 is enough

                                      PosMax1[ii] <<- SpectDataX[[1]][[ii]][MM]  #energy correspondent to the max CL1 or position fit component 1
                                      MM <- max(SpectDataY[[2]][[ii]])       #max of selected corelines2 or fit component 2
                                      MM <- findYIndex(SpectDataY[[2]][[ii]], MM, 0.001)
                                      MM <- MM[1]

                                      PosMax2[ii] <<- SpectDataX[[2]][[ii]][MM]  #energy correspondent to the max CL2 or position fit component 2
                                  }
                                  PosMax1 <<- round(mean(PosMax1), 2)    # PosMax1 is the average position of the Coreline1 (element1) acquired at different Annealing T
                                  PosMax2 <<- round(mean(PosMax2), 2)    # PosMax2 is the average position of the Coreline2 (element2) acquired at different Annealing T
                                  RSF1 <- SpectDataY[[3]]         # Sensitivity Factor of element 1
                                  RSF2 <- SpectDataY[[4]]         # Sensitivity Factor of element 2

                                  # the estimation of the film thickness requires the intensities from surface and bulk elements
                                  # measured under identical conditions i.e. the ratio of intensities from samples composed by only
                                  # by element 1 or by element 2= R.Rsf  is represended by the ratio of the element RSF
                                  R.Rsf <<- RSF1/RSF2

                                  SelectedSpect <<- SelectedCL
                                  if (N.CL ==1) {
                                      SelectedSpect[1] <<- paste(SelectedCL[1], SelectedCL[3], sep=".")
                                      SelectedSpect[2] <<- paste(SelectedCL[1], SelectedCL[4], sep=".")
                                  }

                                  par(mfrow=c(1,1)) #set single panel plot
                                  plot(AnnT, Ratio, type="b", pch=16, cex=2, col="blue", main="INTENSITY RATIO vs. ANNEALING T", xlab="Annealing Temperature [deg. C]", ylab=paste(SelectedSpect[1], "/", SelectedSpect[2]))
                                  txt <- paste("Look at the plot and confirm if: \n", txt, sep="")
                                  answ <- gconfirm(msg=txt, title="CONFIRM ELEMENT POSITION", icon="warning")
                                  E.Units <- "Kinetic Energy [eV]"
                                  if (BE == TRUE) {              #binding energy set
                                      PosMax1 <- XrayE-PosMax1   #now posmax  is in kinetic energy
                                      PosMax2 <- XrayE-PosMax2   #now posmax  is in kinetic energy
                                      E.Units <- "Binding [eV]"
                                      enabled(SaveAndExit) <- TRUE # Saving data blocked: ctrls on Dest file needed
                                  }

                                  #call fitting routine
                                  DataToFit <<- data.frame(x=AnnT, y=Ratio)
                                  Fit <<- FitData()
                                  IniData <<- TRUE
                               } else {
                                  Fit <<- FitData()   # This allows repeating the fit selecting other fitting models
                               }  # end of  if(IniData==FALSE)
                         }, container=AnnTGroup)
      enabled(Thickness) <- FALSE

#--- COMMON BUTTONS


      gbutton("RESET",  handler=function(h, ...) {
                               svalue(SourceFiles) <- NULL
                               svalue(CoreLineList) <- NULL
                               svalue(CommonCoreLines) <- NULL
                               svalue(FilmDens) <- NULL
                               svalue(Xenergy) <- NULL
                               for (ii in 1:N.XS){
                                   AnnTlayout[ii, 1] <<- glabel(" ", container=AnnTlayout)
                                   AnnTlayout[ii, 2] <<- angles[[ii]] <<- gedit("", initial.msg="Temp. deg.", container=AnnTlayout)
                               }

                               FNameList<-XPSFNameList()
                               if (length(FNameList) == 0) { return() }
                               SampID <- ""
                               SpectList <<- ""
                               SourceFileList <<- NULL
                               SelectedFName <<- NULL
                               SpectDataX <<- NULL
                               SpectDataY <<- NULL
                               CLlist <<- list()
                               ComponentCK <<- list()
                               CommonCL <<- XPSSpectList(FNameList[1]) #the list of Corelines of the first XPSSample used as reference
                               SelectedCL <<- NULL
                               SelectedSpect <<- NULL
                               N.XS <<- NULL     #number of selected XPSSamples
                               N.CL <<- NULL     #number of selected common lines

                               AnnT <<- NULL
                               angles <<- list()
                               infoLab <<- list()
                               AreaComp1 <<- NULL
                               AreaComp2 <<- NULL
                               Ratio <<- NULL
                               PosMax1 <<- NULL
                               PosMax2 <<- NULL
                               rho <<- NULL
                               XrayE <<- NULL
                               BE <<- NULL
                               La <<- NULL    #attenuation length element1    cannot use the name L1 problems in XPSFitLM
                               Lb <<- NULL    #attenuation length element2    cannot use the name L2 problems in XPSFitLM
                               R.Rsf <<- NULL
                               FitEstimation <<- NULL
                               FitCurve <<- NULL
                               IniParms <<- data.frame()
                               FitParms <<- NULL
                               IniData <<- FALSE
                               IniData <<- FALSE
                               svalue(NoteBK) <- 1 #go to the first page
                         },container=Addgroup)


      SaveAndExit<-gbutton("SAVE DEPTH PROFILE RESULTS", handler=function(h, ...){
                                  SaveResults()
                                  XPSSaveRetrieveBkp("save")
                         }, container=Addgroup)
      enabled(SaveAndExit) <- FALSE # Saving data blocked: ctrls on Dest file needed

      gbutton("EXIT",  handler=function(h, ...) {
                                  dispose(AddWin)
                                  XPSSaveRetrieveBkp("save")
                         },container=Addgroup)


#DestFName XPSSample modified with baselines if not present

      enabled(SelectCL) <- FALSE

      svalue(NoteBK) <- 1
      visible(AddWin) <- TRUE

}



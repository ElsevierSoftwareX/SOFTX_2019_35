#Macro to change Baseline extremes

#'Modifies the BaseLine level and limits of a given Coreline
#'
#'Function to modify the BaseLine ends and level for a given CoreLine.
#'No parameters are passed to this function.
#'
#'
#'@examples
#'
#'\dontrun{
#'	XPSMoveBaseLine()
#'}
#'
#'@export
#'


XPSMoveBaseLine<- function(){

   ReDraw<-function(){
       Xrange=range(FName[[indx]]@.Data[1])
       if (FName[[indx]]@Flags[1]) {   #reverse if BE scale
           Xrange <- rev(Xrange)
       }
#--- Here the coreline and Baseline+Fit has to be displayed separately
       SampData<-as.matrix(FName[[indx]]@.Data) #create spectrum data matrix for plot
       plot(x=SampData[[1]], y=SampData[[2]], xlim=Xrange, type="l", lty="solid", lwd=1, col="black")
       SampData<-as(FName[[indx]], "matrix") #create Baseline+Fit data matrix for plot
       NC<-ncol(SampData)
       if (NC > 2) { #there is a Baseline
          BaseLine<-SampData[,3]
          matlines(x=SampData[,1], y=BaseLine, xlim=Xrange, type="l", lty="solid", lwd=1, col="sienna")
       }
       if (NC > 3){ #there is a fit
          FitComp<-SampData[,4:NC-1]  #Only components and fit
          SpectFit<-SampData[,NC]  #fit
          matlines(x=SampData[,1], y=FitComp, xlim=Xrange, type="l", lty="solid", lwd=1, col="blue")
          matlines(x=SampData[,1], y=SpectFit, xlim=Xrange, type="l", lty="solid", lwd=1, col="red")
       }
   }



   updateObj <- function(h,...){
      SelectedFName<-svalue(SourceFile)
      FName<<-get(SelectedFName,envir=.GlobalEnv)
      SpectList<<-XPSSpectList(SelectedFName)
      delete(MBLFrame2,SourceCoreline)
      SourceCoreline <<- gcombobox(SpectList, selected=-1, handler=function(h, ...){
                                  SourceFile<-svalue(SourceFile)
                                  SourceCoreline<-svalue(SourceCoreline)
                                  SourceCoreline<-unlist(strsplit(SourceCoreline, "\\."))   #extract the spectrum idx
                                  indx <<- as.numeric(SourceCoreline[1])
                                  BLtype<-FName[[indx]]@Baseline$type
                                  FNameBkp<<-FName #save for reset plot
                                  ReDraw()
                             }, editable=FALSE, container=MBLFrame2)
      add(MBLFrame2,SourceCoreline)
      plot(FName)
      enabled(SourceCoreline) <- TRUE #enable the selection of the coreline
   }


   do.baseline <- function(){
      deg<-NULL
      splinePoints<-NULL
      BLtype<-FName[[indx]]@Baseline$type
      tmp<-NULL
      if ( indx != 0 && hasBoundaries(FName[[indx]]) ) {
          FName[[indx]] <<- XPSsetRegionToFit(FName[[indx]])
          FName[[indx]] <<- XPSbaseline(FName[[indx]], BLtype, deg, splinePoints )
          LL<-length(FName[[indx]]@Components)
          if (LL > 0) {
             for(ii in 1:LL){
                FName[[indx]]@Components[[ii]]<<-Ycomponent(FName[[indx]]@Components[[ii]], x=FName[[indx]]@RegionToFit$x, y=FName[[indx]]@Baseline$y) #calcola la Y eed aggiunge la baseline
             }
# update fit$y with sum of components
             tmp <- sapply(FName[[indx]]@Components, function(z) matrix(data=z@ycoor))
             FName[[indx]]@Fit$y <<- ( colSums(t(tmp)) - length(FName[[indx]]@Components)*(FName[[indx]]@Baseline$y))
          }
          plot(FName[[indx]])
      }
   }


   MakeBaseLine <- function(){
        BLinfo <- FName[[indx]]@Baseline$type
        BasLinType <- BLinfo[1]
        splinePoints <- NULL
        deg <- NULL
        Wgt <- NULL

#        BasLinType<-tolower(BasLinType)
        if (BasLinType == "shirley"){BasLinType<-"Shirley"}       #different names for old/new RXPSG packages
        if (BasLinType == "2P.shirley"){BasLinType<-"2P.Shirley"} #transform to default BaseLineNames of new RXPSG.
        if (BasLinType == "3P.Shirley"){BasLinType<-"3P.Shirley"}
        if (BasLinType == "LP.shirley"){BasLinType<-"LP.Shirley"}
        if (BasLinType == "2P.tougaard"){BasLinType<-"2P.Tougaard"}
        if (BasLinType == "3P.tougaard"){BasLinType<-"3P.Tougaard"}
        if (BasLinType == "4P.tougaard"){BasLinType<-"4P.Tougaard"}

        if (BasLinType == "linear" || BasLinType == "Shirley" || BasLinType == "2P.Shirley" || BasLinType == "2P.Tougaard" || BasLinType == "3P.Tougaard") {
           FName[[indx]] <<- XPSsetRegionToFit(FName[[indx]])
           FName[[indx]] <<- XPSbaseline(FName[[indx]], BLinfo[1], deg, Wgt, splinePoints )
           FName[[indx]] <<- XPSsetRSF(FName[[indx]])
        } else if (BasLinType == "polynomial") {
           deg <- as.numeric(BLinfo[2])
           FName[[indx]] <<- XPSbaseline(FName[[indx]], BLinfo[1], deg, Wgt, splinePoints )
        } else if (BasLinType == "spline") {
            splinePoints<-list(x=NULL, y=NULL)
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
            if (FName[[indx]]@Flags[1] == TRUE) { decr<-TRUE }
            idx<-order(splinePoints$x, decreasing=decr)
            splinePoints$x<-splinePoints$x[idx] #splinePoints$x in ascending order
            splinePoints$y<-splinePoints$y[idx] #following same order select the correspondent splinePoints$y
            LL<-length(splinePoints$x)
            FName[[indx]]@Boundaries$x<<-c(splinePoints$x[1],splinePoints$x[LL]) #set the boundaries of the baseline
            FName[[indx]]@Boundaries$y<<-c(splinePoints$y[1],splinePoints$y[LL])
            FName[[indx]] <<- XPSsetRegionToFit(FName[[indx]])
            FName[[indx]] <<- XPSbaseline(FName[[indx]], BasLinType, deg, Wgt, splinePoints )
            FName[[indx]] <<- XPSsetRSF(FName[[indx]])
        } else if (BasLinType == "3P.Shirley" || BasLinType == "LP.Shirley" || BasLinType == "4P.Tougaard") {
            Wgt <- as.numeric(BLinfo[2])
            FName[[indx]] <<- XPSsetRegionToFit(FName[[indx]])
            FName[[indx]] <<- XPSbaseline(FName[[indx]], BasLinType, deg, Wgt, splinePoints )
            FName[[indx]] <<- XPSsetRSF(FName[[indx]])
        }
        LL<-length(FName[[indx]]@Components)
        if (LL > 0) {
           for(ii in 1:LL){
              FName[[indx]]@Components[[ii]]<<-Ycomponent(FName[[indx]]@Components[[ii]], x=FName[[indx]]@RegionToFit$x, y=FName[[indx]]@Baseline$y) #calcola la Y eed aggiunge la baseline
           }
# update fit$y with sum of components
           tmp <- sapply(FName[[indx]]@Components, function(z) matrix(data=z@ycoor))
           FName[[indx]]@Fit$y <<- ( colSums(t(tmp)) - length(FName[[indx]]@Components)*(FName[[indx]]@Baseline$y))
        }
   }




#--- Variables ---

   rm(list = ls(envir = MyEnv, all.names = TRUE), envir = MyEnv)
   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }

   FName<-NULL
   FNameBkp<-NULL
   FNameList<-XPSFNameList()
   SpectList<-""
   index<-NULL
   plot.new() #reset the graphic window

#--- GUI ---

   MBLwin<-gwindow("MOVE BASELINE", visible=FALSE)
   MBLgroup <- ggroup(label="", horizontal=FALSE, container=MBLwin)

   glabel("Select the XPSSample", container=MBLgroup)
   MBLFrame1<-gframe("Select the XPSSample data file", horizontal=TRUE,container=MBLgroup)
   SourceFile<-gcombobox(FNameList, selected=-1, editable=FALSE, expand=FALSE, handler=updateObj, container = MBLFrame1)

   MBLFrame2 <-gframe(text="Select CoreLine to Process", spacing=5, container=MBLgroup)
   SourceCoreline <- gcombobox(SpectList, selected=-1, editable=FALSE, container=MBLFrame2)

   MBLFrame3 <-gframe(text="Move Baseline", horizontal=FALSE, spacing=5, container=MBLgroup)
   MBLbutton <-gbutton(text="Set Baseline Boundaries", horizontal=FALSE, spacing=5, handler=function(h,...){
                    BLinfo <- FName[[indx]]@Baseline$type
                    if (BLinfo[1] == "spline") {
                       txt<-"Spline background: \n ==> LEFT click to set spline points; RIGHT to exit"
                       gmessage(msg=txt, title="HELP INFO", icon="info")
                    } else {
                       txt <- paste(BLinfo[1], " background found!\n  ==> Set the Baseline Limits")
                       gmessage(msg=txt, title="HELP INFO", icon="info")
                       pos<-locator(n=2, type="p", col="red", lwd=2)
                       FName[[indx]]@Boundaries<<-pos
                    }
#                    do.baseline()
                    MakeBaseLine()
                    #if change the Baseline extension the fit components and the fit have to be recomputed
                    LL=length(FName[[indx]]@Components)
                    tmp<-NULL
                    if (LL > 0) {
                       for(ii in 1:LL) {
                          FName[[indx]]@Components[[ii]] <- Ycomponent(FName[[indx]]@Components[[ii]], x=FName[[indx]]@RegionToFit$x, y=FName[[indx]]@Baseline$y) #computes the Y and add the Baseline
                       }
                       tmp <- sapply(FName[[indx]]@Components, function(z) matrix(data=z@ycoor))
                       FName[[indx]]@Fit$y <- (colSums(t(tmp)) - length(FName[[indx]]@Components)*(FName[[indx]]@Baseline$y))
                    }
                    plot(FName[[indx]])
                 }, container=MBLFrame3)
#   MBLlabel<-glabel(text="Boundaries selection with Left mouse button ", container=MBLFrame3)


   gbutton("      RESET      ", handler=function(h,...){
                    LL <- length(FName[[indx]]@Components)
                    FName[[indx]] <<- FNameBkp[[indx]]
                    ReDraw()
                 }, container = MBLgroup)

   gbutton("       SAVE      ", handler=function(h,...){
    	              assign(activeFName, FName, envir=.GlobalEnv)
                    XPSSaveRetrieveBkp("save")
                 }, container = MBLgroup)


   gbutton("   Save & EXIT   ", handler=function(h,...){
                    dispose(MBLwin)
     	              assign(activeFName, FName, envir=.GlobalEnv)
                    XPSSaveRetrieveBkp("save")
                 }, container = MBLgroup)

   visible(MBLwin) <- TRUE

}

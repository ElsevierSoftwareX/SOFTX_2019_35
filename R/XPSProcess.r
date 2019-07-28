###########################################
## XPS processing with gWidgets2 and tcltk
###########################################

#'GUI to analyze XPS-Corelines
#'
#'Interactive GUI to add BaseLines and Fitting components to a CoreLine
#'
#'@param Object XPSSample object
#'@param Object_name XPSSample name as character
#'@param TKwin Graphical windows dimensions
#'@return Return the processed \code{object}.
#'
#'@export
#'


XPSprocess <- function(Object, Object_name = as.character(substitute(Object)), TKwin=winsize) {

  my.coords <- function(plot, x, y, parplt, parusr)
  {
    xClick <- as.numeric(x)     #x, y, position in pixels from left top corner
    yClick <- as.numeric(y)

    width  <- as.numeric(tclvalue(tkwinfo("reqwidth",plot)))    #window dimension in pixels
    height <- as.numeric(tclvalue(tkwinfo("reqheight",plot)))

    #in draw.plot(): parplt <- par('plt')
    xMin <- parplt[1] * width   #renormalize pixels for plotting area dimension (parplt ranges in 0:1)
    xMax <- parplt[2] * width
    yMin <- parplt[3] * height
    yMax <- parplt[4] * height
    #in draw.plot(): parusr <- par('usr')
    rangeX <- parusr[2] - parusr[1]  #X, Y ranges of the plotted spectrum
    rangeY <- parusr[4] - parusr[3]

    yClick <- height - yClick #+ 2*Bordy    #2*Bordy one for the upper border one for the lower

    xPlotCoord <- parusr[1]+(xClick-xMin)*rangeX/(xMax-xMin) #now convert renormalized pixels in the spectrum scale
    yPlotCoord <- parusr[3]+(yClick-yMin)*rangeY/(yMax-yMin) 

    c(xPlotCoord, yPlotCoord, width, height, xClick, yClick)
  }


  mouseLFT <- function(x, y) {
     tabMain <- svalue(nbMain)
     tabComp <- svalue(nbComponents)
     if (coreline == 0) { return() }
     if ( SetZoom == TRUE ) {   #when Set Zoom set only mouseRGT button has to be used
        return()
     }
     if (svalue(BaselineType1) == -1 && svalue(BaselineType2) == -1 && svalue(BaselineType3) == -1) {    # no Baseline selected
         gmessage("Select Baseline please...", title="WARNINR", icon="warning")
         return()
     }
     if (tabMain == 1 && length(Object[[coreline]]@Components) > 0) {
         gmessage("Fit present: \nchange notebook page, Baseline operations not allowed", title="WARNINR", icon="warning")
         return()
     }
     if ( coreline != 0) { #coreline != "All Spectra"
	     coords  <<-  my.coords(img, x, y, parplt, parusr)
		  yy <- coords[2]
		  xx <- coords[1]

        if (Object[[coreline]]@Flags[1]) { #Binding energy set
           if (xx > parusr[1] || xx < parusr[2]  || yy < parusr[3]  || yy > parusr[4] ){ return() } #if you click ouside XY box of the plot do noting
        } else {
           Xlimits<<-sort(c(point.coords$x[1], point.coords$x[2]), decreasing=FALSE) #pos$x in increasing order
           if (xx < parusr[1]  || xx > parusr[2] || yy < parusr[3]  || yy > parusr[4]){ return() }
        }

        if (! is.null(point.coords$x[1]) && tabMain==1 ) { #initially point.coords contains the spectrum boundaries
	        d.pts <- (point.coords$x - xx)^2  #distance between spectrum edge and marker position
	        point.index <<- min(which(d.pts == min(d.pts)))  #which is the edge nearest to the marker?
    	  } else { 
           point.index <<- 1
        }
		  point.coords$x[point.index] <<- xx   # set the marker position or modify the position for the baseline (
		  point.coords$y[point.index] <<- yy

        if (tabMain == 1 && SetZoom == FALSE) {    # Baseline notebook page
           Object[[coreline]]@Boundaries <<- point.coords
           if (Object[[coreline]]@Flags[1]) { #Binding energy set
               Xlimits<<-sort(c(point.coords$x[1], point.coords$x[2]), decreasing=TRUE) #pos$x in decreasing order
           } else {
               Xlimits<<-sort(c(point.coords$x[1], point.coords$x[2]), decreasing=FALSE) #pos$x in increasing order
           }
#           Ylimits <<- range(Object[[coreline]]@.Data[2])
           splinePoints <<- point.coords
           do.baseline(deg, Wgt, splinePoints)
        }
        if (tabMain == 1 && SetZoom == TRUE) {    # Baseline notebook page
           Object[[coreline]]@Boundaries <<- Corners
           if (Object[[coreline]]@Flags[1]) { #Binding energy scale
               Xlimits <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE)  # X zoom limits
               Ylimits <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE) # Y zoom limits
           } else {                           #Kinetic energy scale
               Xlimits <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE)
               Ylimits <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
           }

           point.coords$x <<- Xlimits   #Baseline edges == X zoom limits
           idx <- findXIndex(Object[[coreline]]@.Data[1], Xlimits[1])
           point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][idx]
           idx <<- findXIndex(Object[[coreline]]@.Data[1], Xlimits[2])
           point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][idx]
        }
        if (tabMain == 2 && tabComp == 2) { # Components/Move Notebook page
           do.move()
        }
     }
     Marker$Points<<-point.coords
     replot()
  }


  dragmousemove <- function(x, y) {
     if ( SetZoom == TRUE ) {   #when Set Zoom set only mouseRGT button has to be used
        return()
     }
     if ( coreline != 0) {   #coreline == "All spectra"
       coords <<- my.coords(img, x, y, parplt, parusr)
       point.coords$x[point.index] <<- coords[1]
       point.coords$y[point.index] <<- coords[2]
       Marker$Points<<-point.coords
       replot()
     }
  }


  mouseRGT <- function(x, y) {
     coords  <<-  my.coords(img, x, y, parplt, parusr)
     yy <- coords[2]
     xx <- coords[1]
     if (Object[[coreline]]@Flags[1]) { #Binding energy set
        if (xx > parusr[1] || xx < parusr[2]  || yy < parusr[3]  || yy > parusr[4] ){ return() } #if you click ouside XY box of the plot do noting
     } else {
        Xlimits<<-sort(c(point.coords$x[1], point.coords$x[2]), decreasing=FALSE) #pos$x in increasing order
        if (xx < parusr[1]  || xx > parusr[2] || yy < parusr[3]  || yy > parusr[4]){ return() }
     }

     if (BType=="spline") {
       splinePoints$x <<- c(splinePoints$x, coords[1])
       splinePoints$y <<- c(splinePoints$y, coords[2])
       Marker$Points<<-splinePoints
     } else if ( SetZoom == TRUE ) {   #RGT click to define zoom area
        point.coords$x[point.index] <<- coords[1]   #add component 3 to abscissa
        point.coords$y[point.index] <<- coords[2]   #add component 3 to ordinate
  	     xlim <- sort(Xlimits,decreasing=FALSE)
  	     DY <- (Ylimits[2]-Ylimits[1])/30
        if (xx < xlim[1]  || xx > xlim[2] || yy < (Ylimits[1]-DY)  || yy > (Ylimits[2]+DY) ){ return() } #control riht click inside the XY range
        if (point.index==1) {
           Corners$x<<-c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
           Corners$y<<-c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
           point.index<<-3    #to add comp. 3 to points.coord and save the new marker position
  	     } else if (point.index==3) {
           D<-NULL
           Dmin<-((point.coords$x[3]-Corners$x[1])^2 + (point.coords$y[3]-Corners$y[1])^2)^0.5   #inizialization values
           for (ii in 1:4) {
               D[ii]<-((point.coords$x[3]-Corners$x[ii])^2 + (point.coords$y[3]-Corners$y[ii])^2)^0.5  #dist P0 P1
               if(D[ii] <= Dmin){
                   Dmin<-D[ii]
                   idx=ii
               }
           }
           if (idx==1){  #adjust corner position following the new marker position
               Corners$x[1]<<-Corners$x[2]<<-point.coords$x[3]
               Corners$y[1]<<-Corners$y[3]<<-point.coords$y[3]
           } else if (idx==2){
               Corners$x[1]<<-Corners$x[2]<<-point.coords$x[3]
               Corners$y[2]<<-Corners$y[4]<<-point.coords$y[3]
           } else if (idx==3){
               Corners$x[3]<<-Corners$x[4]<<-point.coords$x[3]
               Corners$y[1]<<-Corners$y[3]<<-point.coords$y[3]
           } else if (idx==4){
               Corners$x[3]<<-Corners$x[4]<<-point.coords$x[3]
               Corners$y[2]<<-Corners$y[4]<<-point.coords$y[3]
           }
           if (Object[[coreline]]@Flags[1]) { #Binding energy scale
               point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE) # pos$x in decreasing order: Corners$x[1]==Corners$x[2]
               point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
           } else {                           #kinetic energy scale
               point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE) # pos$x in increasing order
               point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
           }
        }
        Marker$Points<<-Corners
     }
     replot()
  }


  draw.plot <- function(...) {
     if ( coreline == 0) {     # coreline == "All spectra"
        plot(Object)
     } else {
        tabMain <- svalue(nbMain)
   	  tabComp <- svalue(nbComponents)
        if (tabMain == 1) {   #Baseline pgMain
           plot(Object[[coreline]], xlim=Xlimits, ylim=Ylimits)
	        if (SetZoom == TRUE) {  #in Zoom Mode  BType is set to ""
              points(Marker$Points, col=Marker$col, cex=Marker$cex, lwd=Marker$lwd, pch=Marker$pch) #draw zoom corners
              rect(point.coords$x[1], point.coords$y[1], point.coords$x[2], point.coords$y[2])      #draw zoom frame area
           } else {
              points(Marker$Points, col=Marker$col, cex=Marker$cex, lwd=Marker$lwd, pch=Marker$pch)
           }
        } else if (tabMain == 2 && (tabComp == 1)) { #Components-Add/Delete Tab
           if (svalue(plotFit) == "residual" && hasFit(Object[[coreline]])) {
              XPSresidualPlot(Object[[coreline]])
           } else {
		        plot(Object[[coreline]])
		        points(point.coords, col=2, cex=1.2, lwd=2, pch=3)  #draw component position
	        }
        } else if ((tabMain == 2) && (tabComp == 2) ){ #Components-Move Tab
           MaxCompCoords <- getMaxOfComponents(Object[[coreline]])
           point.coords$x<<-MaxCompCoords[[1]][compIndx]
           point.coords$y<<-MaxCompCoords[[2]][compIndx]
           if (svalue(plotFit) == "residual" && hasFit(Object[[coreline]])) {
			     XPSresidualPlot(Object[[coreline]])
           } else {
	           plot(Object[[coreline]])
           }
	        points(point.coords, col=2, cex=1, lwd=2, pch=1)
        }

        parusr <<- par('usr')
        parplt <<- par('plt')
        if (is.null(point.coords$x)) { point.coords$x <- NA }
        if (is.null(point.coords$y)) { point.coords$y <- NA }
        svalue(sb) <- sprintf(paste("x =",round(point.coords$x[1],2), " y =",round(point.coords$y[2],2), sep=" "))
     }
  }


  replot <- function(...) {
    tkrreplot(img)
  }


  set.coreline <- function(h, ...) {
    coreline<<-svalue(Core.Lines)
    coreline<<-unlist(strsplit(coreline, "\\."))   #"number." and "CL name" are separated
    coreline<<-as.integer(coreline[1])   # Coreline index
    if ( coreline == 0) {    #coreline == "All spectra"
		 enabled(T1Frame1) <- FALSE
		 enabled(T2group1) <- FALSE
    } else {
       enabled(T1Frame1) <- TRUE
       enabled(MZbutton)<-FALSE
       enabled(ZObutton)<-FALSE

       if (length(Object[[coreline]]@Baseline) > 0 ) enabled(T2group1) <- TRUE
       # Zoom disabled
       SetZoom <<- FALSE
       # if boundaries already defined
       if ( hasBaseline(Object[[coreline]]) ) {
          LL<-length(Object[[coreline]]@RegionToFit$x)
          point.coords$x <<- c(Object[[coreline]]@RegionToFit$x[1], Object[[coreline]]@RegionToFit$x[LL])
          point.coords$y <<- c(Object[[coreline]]@RegionToFit$y[1], Object[[coreline]]@RegionToFit$y[LL])
          Xlimits<<-range(Object[[coreline]]@RegionToFit$x)
          Ylimits<<-range(Object[[coreline]]@RegionToFit$y)
       } else if ( hasBoundaries(Object[[coreline]]) ) {
          LL<-length(Object[[coreline]]@.Data[[1]])
          point.coords$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
          point.coords$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
          Xlimits<<-range(Object[[coreline]]@.Data[1])
          Ylimits<<-range(Object[[coreline]]@.Data[2])
       } else {
          reset.baseline()   #defines the spectral limits and marker positions
       }
       if ( hasBaseline(Object[[coreline]]) ) {
          BasLinType<-Object[[coreline]]@Baseline$type
          BasLinType<-tolower(BasLinType)
          if (length(grep(BasLinType,c("linear", "polynomial", "spline"))) > 0){
              svalue(BaselineType1) <- BasLinType
          } else if(length(grep(BasLinType,c("shirley", "2p.shirley", "3p.shirley", "lp.shirley")))>0) {
              svalue(BaselineType2) <- BasLinType
          } else if(length(grep(BasLinType,c("2p.tougaard", "3p.tougaard", "4p.tougaard"))) > 0) {
              svalue(BaselineType3) <- BasLinType
          }
          Xlimits<<-range(Object[[coreline]]@RegionToFit$x)
          Ylimits<<-range(Object[[coreline]]@RegionToFit$y)
          svalue(nbMain) <- 1
       }
       if ( hasComponents(Object[[coreline]]) ) {
          enabled(T2group1) <- TRUE
      	 svalue(nbMain) <- 2
  		    svalue(nbComponents) <- 1
          CompNames<<-names(Object[[coreline]]@Components)
          delete(T2Frame3, FitComp)
          FitComp<<-gcombobox(CompNames, selected=-1, handler=function(h, ...){
                               compIndx<-svalue(FitComp)
                               compIndx<-unlist(strsplit(compIndx, split="C"))   #index of the chosen component
                               compIndx<<-as.integer(compIndx[2])
                               replot()
                            }, container = T2Frame3)
          add(T2Frame3, FitComp)
       }
       Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
       Object[[coreline]]@Boundaries <<- point.coords #boundaries correspond to coreline limits or RegToFit limits
    }
    svalue(nbMain) <- 1 #when a coreline is selected Baseline Notebook page is set
    replot()
  }


  do.baseline <- function(deg, Wgt, splinePoints, ...){
     if ( BType=="") {
        gmessage("Please select the BaseLine type!", icon="warning")
        return()
     }
     if ( coreline != 0 && hasBoundaries(Object[[coreline]]) ) {
        Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])
        if (BType == "spline" && length(splinePoints)==0) { return()} #splinePoints not defined skip XPSBaseline()
        Object[[coreline]] <<- XPSbaseline(Object[[coreline]], BType, deg, Wgt, splinePoints )
        Object[[coreline]] <<- XPSsetRSF(Object[[coreline]], Object[[coreline]]@RSF)
        enabled(T2group1) <- TRUE
        replot()
     }
     svalue(nbComponents) <- 1 #quando seleziono una coreline mi metto sulla pagina Add/Delete Components
  }


  reset.baseline <- function(h, ...) {
    if ( coreline != 0 ) {   #coreline != "All spectra"
       if ( FreezeLimits == FALSE ) {  #ZOOM not activated
          if (Object[[coreline]]@Flags[1] == TRUE){  #Binding Energy scale
             point.coords$x <<- sort(range(Object[[coreline]]@.Data[1]),decreasing=TRUE)
          } else {                                   #Kinetic energy scale
             point.coords$x <<- sort(range(Object[[coreline]]@.Data[1]),decreasing=FALSE)
          }
          idx1 <- findXIndex(Object[[coreline]]@.Data[[1]], point.coords$x[1])
          point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][idx1]
          idx2 <- findXIndex(Object[[coreline]]@.Data[[1]], point.coords$x[2])
          point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][idx2]
          Xlimits<<-point.coords$x
          Ylimits<<-c(min(Object[[coreline]]@.Data[[2]][idx1:idx2]), max(Object[[coreline]]@.Data[[2]][idx1:idx2]))
	       Object[[coreline]] <<- XPSremove(Object[[coreline]], "all")
          splinePoints<<-NULL
       } else {  #a zoom is present: we preserve Xlimits and Ylimits and point.coords values
          Object[[coreline]] <<- XPSremove(Object[[coreline]], "all")
          splinePoints<<-NULL
       }
	    enabled(T2group1) <- FALSE
	 }
	 Object[[coreline]]@Boundaries <<- point.coords
  }


  update.outputArea <- function(...) {
  	  if ( coreline != 0 ) {
  		  if (svalue(disp_area)) { XPScalc(Object[[coreline]]) }
  		  if ( hasFit(Object[[coreline]]) && svalue(disp_quant)) {
		     XPSquantify(Object)
	     }
  	  }
  }


  add.component <- function(h, ...) {
    if ( coreline != 0 && hasBaseline(Object[[coreline]]) ) {
   	 if (! is.null(point.coords$x[1]) ) {
      		Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = svalue(fit.type),
                                             peakPosition = list(x = point.coords$x[point.index], y = point.coords$y[point.index]) )
## to update fit remove Component@Fit and make the sum of Component@ycoor including the newone
	         tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))  #create a matrix formed by ycoor of all the fit Components
	         CompNames<<-names(Object[[coreline]]@Components)
	         delete(T2Frame3, FitComp)
            FitComp<<-gcombobox(CompNames, selected=-1, handler=function(h, ...){
                                  compIndx<-svalue(FitComp)
                                  compIndx<-unlist(strsplit(compIndx, split="C"))   #indice della componente scelta class numeric
                                  compIndx<<-as.integer(compIndx[2])
                                  replot()
                               }, container = T2Frame3)
            add(T2Frame3, FitComp)
	         Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y))
      		point.coords <<- list(x=NULL,y=NULL)
      		replot()
      	}
      }
  }


  del.component <- function(h, ...) {
     if (gconfirm(msg="All Constraints will be lost! Proceed anyway?", title="DELETE", icon="warning")) {
	     LL<-length(Object[[coreline]]@Components)
	     for (ii in 1:LL) { #remove all CONSTRAINTS
            Object[[coreline]]<<-XPSconstrain(Object[[coreline]],ii,action="remove",variable=NULL,value=NULL,expr=NULL)
        }
        if ( coreline != 0 && hasComponents(Object[[coreline]]) ) {
		     delWin <- gwindow("DELETE FIT COMPONENT", parent = window, visible = FALSE)
		     size(delWin) <-c(250, 150)
		     delGroup <- ggroup(horizontal=FALSE, container=delWin)
		     glabel("Select the fit component to delete", container=delGroup)
		     gseparator(container=delGroup)
			  compId <- gcombobox(c(CompNames,"All"), selected=1, container = delGroup)
		     buttonGroup <- ggroup(horizontal=TRUE, container=delGroup)
		     gbutton("OK", handler=function(...){
                  if (svalue(compId) != "All"){
                     indx<-as.numeric(svalue(compId, index=TRUE))
		     		      Object[[coreline]] <<- XPSremove(Object[[coreline]], what="components", number=indx )
		     		      CompNames<<-names(slot(Object[[coreline]],"Components"))
		     		      if (length(Object[[coreline]]@Components) > 0 ) {
                        #plot update:
	                      tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))
	                      Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y))
                     }
		            } else {
		    		      Object[[coreline]] <<- XPSremove(Object[[coreline]], "components")
                     CompNames<<-""
                  }
                  delete(T2Frame3,FitComp)
                  FitComp<<-gcombobox(CompNames, selected=-1, handler=function(h, ...){  #Update component selection in MOVE COMP panel
                                      compIndx<-svalue(FitComp)
                                      compIndx<-unlist(strsplit(compIndx, split="C"))    #index of the selected component
                                      compIndx<<-as.integer(compIndx[2])
                                      replot()
                                   }, container = T2Frame3)

 			         svalue(plotFit) <<- "normal"
		            point.coords <<- list(x=NULL,y=NULL)
			         replot()
		            dispose(delWin)
		         }, container=buttonGroup)
		    gbutton("Cancel", handler=function(...) dispose(delWin), container=buttonGroup)
		    visible(delWin) <- TRUE
       }
     }
  }


  do.move <- function(...) {
    compIndx<-svalue(FitComp)
    compIndx<-unlist(strsplit(compIndx, split="C"))   #index selected component
    compIndx<-as.integer(compIndx[2])
  	 if ( coreline != 0 ) {
  		 Xx <- point.coords$x[point.index]
  		 Yy <- point.coords$y[point.index]
		 varmu <- sort(getParam(Object[[coreline]]@Components[[compIndx]],variable="mu"))
		 minmu <- varmu$start-varmu$min
		 maxmu <- varmu$max-varmu$start
		 newmu <- c(Xx, Xx-minmu, Xx+maxmu)
		 newy <- Yy - Object[[coreline]]@Baseline$y[max(which(Object[[coreline]]@Baseline$x>Xx))+1]

		 Object[[coreline]]@Components[[compIndx]] <<- setParam(Object[[coreline]]@Components[[compIndx]], parameter=NULL, variable="mu", value=newmu)
		 Object[[coreline]]@Components[[compIndx]] <<- setParam(Object[[coreline]]@Components[[compIndx]], parameter="start", variable="h", value=newy)
		 Object[[coreline]]@Components[[compIndx]] <<- Ycomponent(Object[[coreline]]@Components[[compIndx]], x=Object[[coreline]]@RegionToFit$x, y=Object[[coreline]]@Baseline$y)

		 tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))
		 Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y) )
		 Object[[coreline]] <<- sortComponents(Object[[coreline]]) #order components in decreasing order
		 update.outputArea()
  	 }
  }


  do.before.close <- function(...) {
    activeSpecIndx<-coreline
	 assign(Object_name, Object, envir = .GlobalEnv)
	 assign("activeSpectIndx", activeSpecIndx, envir = .GlobalEnv)
	 assign("activeSpectName", coreline, envir = .GlobalEnv)
    dispose(MainWindow)
    plot(Object[[activeSpecIndx]])
    XPSSaveRetrieveBkp("save")
  }




#---  VARIABILI  -----------------------------
  point.coords <- list(x=NULL, y=NULL)
  compIndx <- 1
  parusr <- NULL # conversion units
  parplt <- NULL # conversion units
  Marker <- list(Points=list(x=NULL, y=NULL), col=2, cex=2, lwd=1.5, pch=10)
  coords <- NULL # for printing mouse coordinates on the plot
  Xlimits <- NULL
  Ylimits <- NULL
  FNameList <- XPSFNameList() #list of XPSSamples
  SpectList <- XPSSpectList(Object_name) #list of XPSSample Corelines
  coreline <- 0
  deg <- 1 #by default the baseline polynom degree = 1
  Wgt <- 0.3 #LinearPolynomial weigth in LP.Shirley
  BType<-"linear" #defaul BKground
  splinePoints<-list(x=NULL, y=NULL)
  SetZoom <- FALSE
  FreezeLimits <- FALSE
  Corners <- point.coords
  point.index<-1
  ZoomPoints<-list(x=NULL, y=NULL)
  FitFunct<-c("Gauss", "Lorentz", "Voigt", "Sech2", "GaussLorentzProd", "GaussLorentzSum",
              "AsymmGauss", "AsymmVoigt", "AsymmGaussLorentz", "AsymmGaussVoigt",
              "AsymmGaussLorentzProd", "DoniachSunjic", "DoniachSunjicTail",
              "DoniachSunjicGauss", "DoniachSunjicGaussTail", "SimplfiedDoniachSunjic", "ExpDecay",
              "PowerDecay", "Sigmoid")
  CompNames<-"   "


#----- PROCESS GUI -----------------------------
  ## Start
  MainWindow <- gwindow("XPS processing GUI", toolkit = "tcltk", visible = FALSE)
  MainGroup <- ggroup(container = MainWindow, horizontal = TRUE)

  ## XPS Sample & Core lines
  Pgroup1 <- ggroup(horizontal = FALSE, spacing = 5, expand = TRUE, container = MainGroup)
  ## Interactive graph window
  GraphGroup <- ggroup(container = MainGroup)

  Pframe1 <- gframe(text = " XPS Sample and Core line Selection ",horizontal = TRUE, container = Pgroup1)
  XPS.Sample <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h,...){
                                 activeFName<-svalue(XPS.Sample)
                                 Object<<-get(activeFName, envir=.GlobalEnv)
                                 SpectList<<-XPSSpectList(activeFName)
                                 delete(Pframe1, Core.Lines)
                                 Core.Lines <<- gcombobox(c("0.All spectra", SpectList), selected=1, expand = FALSE, handler = set.coreline, container = Pframe1)
                                 coreline <<- 0
                                 replot()
                       }, container = Pframe1)
  svalue(XPS.Sample)<-activeFName

  Core.Lines <- gcombobox(c("0.All spectra", SpectList), selected=1, expand = FALSE, handler = set.coreline, container = Pframe1)


#----- Notebook -----------------------------
  nbMain <- gnotebook(expand = FALSE, container = Pgroup1)
  size(nbMain) <- c(360,365)  #this are the minimal dimensions of the notebook to visualize all the widgets


#----- TAB1: Baseline -----
  T1group1 <- ggroup(container = nbMain, label = "Baseline", spacing=1, horizontal=FALSE)

  T1Frame1 <- gframe(text = " Process ", spacing=1, horizontal=FALSE, container = T1group1)
  T1Frame2 <- gframe(text = " Help ", spacing=1, container = T1Frame1)

  glabel("Set the Baseline Edges with the cursor", spacing=1, container = T1Frame2)
  T1Frame3 <- gframe(text = " Baseline Types ", horizontal=FALSE, spacing=1, container = T1Frame1)

  BaselineType1 <- gradio(items=c("linear", "polynomial", "spline"), selected=1, spacing=1, horizontal = TRUE, handler = function(h, ...){
                                   svalue(BaselineType2, index=TRUE)<- -1  #CLEAR BaselineType2
                                   svalue(BaselineType3, index=TRUE)<- -1  #CLEAR BaselineType3
                                   tcl("update", "idletasks")
                                   BType <<- svalue(BaselineType1)
                                   if (BType == "linear") {
                                      #fast generation of background do not need background reset
                                      delete(T1Frame3, T1group2)       #delete content of Frame Settings
                                      T1group2<<-ggroup(horizontal=FALSE, spacing=1, container = T1Frame3)
                                      glabel("  ", container=T1group2) #add two empty row
                                      glabel("  ", container=T1group2)
                                      add(T1Frame3, T1group2)
                                      Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                      do.baseline(deg, Wgt, splinePoints)
                                  }
                                   if (BType == "polynomial") {
                                      splinePoints<<-list(x=NULL, y=NULL)
                                      reset.baseline()  #reset previous baseline
                                      Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                      replot()
                                      delete(T1Frame3, T1group2)
                                      T1group2<<-ggroup(horizontal=FALSE, spacing=1, container = T1Frame3)
                                      glabel("Polynom degree", container=T1group2)
                                      Pgroup<<-ggroup(horizontal=TRUE, spacing=1, container = T1group2)
                                      polydeg<-gcombobox(c("1", "2", "3", "4", "5", "6"), width=20, selected=1, container= Pgroup)
                                      gbutton("Make BaseLine", handler = function(h, ...) {
                                                           deg<<-as.numeric(svalue(polydeg))
                                                           do.baseline(deg, Wgt, splinePoints)
                                                           }, container= Pgroup)
                                      add(T1Frame3, T1group2)
                                      do.baseline(deg, Wgt, splinePoints)
                                   }
                                   if (BType == "spline") {
                                      Marker <<- list(Points=point.coords, col=3, cex=1, lwd=2, pch=1)
                                      reset.baseline()  #reset previous baseline
                                      replot()
                                      delete(T1Frame3, T1group2)
                                      T1group2<<-ggroup(horizontal=FALSE, spacing=1, container = T1Frame3)
                                      glabel("Select spline points DX mouse button", container=T1group2)
                                      splineButton<-gbutton("Make Spline Baseline", handler = function(h, ...) {
                                                              if (length(splinePoints$x) <= 2) {
                                                                gmessage(msg="Select the spline points with the Right mouse button. Then press SET BASELINE button", title="WARNING!", icon="warning")
                                                                svalue(BaselineType1)<-"linear" #plot Linear baseline until splinePoints are selected
                                                                splinePoints <<- point.coords
                                                              } else {
                                                                decr<-FALSE #Kinetic energy set
                                                                if (Object[[coreline]]@Flags[1] == TRUE) { decr<-TRUE }
                                                                idx<-order(splinePoints$x, decreasing=decr)
                                                                splinePoints$x<<-splinePoints$x[idx] #splinePoints$x in ascending order
                                                                splinePoints$y<<-splinePoints$y[idx] #following same order select the correspondent splinePoints$y
                                                                LL<-length(splinePoints$x)
                                                                Object[[coreline]]@Boundaries$x<<-c(splinePoints$x[1],splinePoints$x[LL]) #set the boundaries of the baseline
                                                                Object[[coreline]]@Boundaries$y<<-c(splinePoints$y[1],splinePoints$y[LL])
                                                                Marker <<- list(Points=splinePoints, col=3, cex=1, lwd=2, pch=1)
                                                                do.baseline(deg, Wgt, splinePoints)
                                                              }
  	                                                        },container = T1group2)
                                      add(T1Frame3, T1group2)
                                      do.baseline(deg, Wgt, splinePoints)
                                   }
                   },container = T1Frame3)

  BaselineType2 <- gradio(items=c("Shirley", "2P.Shirley", "3P.Shirley", "LP.Shirley"), selected=-1, spacing=1, horizontal = TRUE, handler = function(h, ...){
                                   svalue(BaselineType1, index=TRUE)<- -1  #CLEAR BaselineType1
                                   svalue(BaselineType3, index=TRUE)<- -1  #CLEAR BaselineType3
                                   tcl("update", "idletasks")
                                   BType <<- svalue(BaselineType2)
                                   if (BType=="Shirley" || BType=="2P.Shirley") {
                                      #fast generation of background do not need background reset
                                      splinePoints<<-list(x=NULL, y=NULL)  #reset preivous baseline
                                      reset.baseline()
                                      Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                      replot()
                                      delete(T1Frame3, T1group2)
                                      T1group2<<-ggroup(horizontal=FALSE, spacing=1, container = T1Frame3)
                                      glabel("   ", spacing=1, container=T1group2) #add space with empty text rows
                                      glabel("   ", spacing=1, container=T1group2)
                                      add(T1Frame3, T1group2)
                                      do.baseline(deg, Wgt, splinePoints)
                                   }
                                   if (BType=="3P.Shirley") {
                                      splinePoints<<-list(x=NULL, y=NULL)  #reset preivous baseline
                                      reset.baseline()
                                      Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                      replot()
                                      delete(T1Frame3, T1group2)
                                      T1group2 <<- ggroup(horizontal=FALSE, spacing=1, container = T1Frame3)
                                      glabel("Distortion Parameter Ds", spacing=1, container=T1group2)
                                      STgroup <<- ggroup(horizontal=TRUE, spacing=1, container = T1group2)
                                      weight<-gedit("0.3", width=20, spacing=1, container= STgroup)
                                      gbutton("Make BaseLine", handler = function(h, ...) {
                                                           slot(Object[[coreline]],"Boundaries") <<- point.coords
                                                           Wgt<<-as.numeric(svalue(weight))
                                                           do.baseline(deg, Wgt, splinePoints)
                                                           }, container= STgroup)
                                      add(T1Frame3, T1group2)
                                   }
                                   if (BType=="LP.Shirley") {
                                      splinePoints<<-list(x=NULL, y=NULL)  #reset preivous baseline
                                      reset.baseline()
                                      Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                      replot()
                                      delete(T1Frame3, T1group2)  #canccello il contenuto Frame Settings
                                      T1group2 <<- ggroup(horizontal=FALSE, spacing=1, container = T1Frame3)
                                      glabel("B coeff.", spacing=1, container=T1group2)
                                      STgroup <<- ggroup(horizontal=TRUE, spacing=1, container = T1group2)
                                      weight<-gedit("0.005", width=20, spacing=1, container= STgroup)
                                      gbutton("Make BaseLine", handler = function(h, ...) {
                                                           Object[[coreline]]@Boundaries <<- point.coords
                                                           Wgt<<-as.numeric(svalue(weight))
                                                           do.baseline(deg, Wgt, splinePoints)
                                                           }, container= STgroup)
                                      add(T1Frame3, T1group2)
                                   }
                   },container = T1Frame3)

  BaselineType3 <- gradio(items=c("2P.Tougaard", "3P.Tougaard", "4P.Tougaard"), selected=-1, spacing=1, horizontal = TRUE, handler = function(h, ...){
                                   svalue(BaselineType1, index=TRUE)<- -1  #CLEAR BaselineType1
                                   svalue(BaselineType2, index=TRUE)<- -1  #CLEAR BaselineType2
                                   tcl("update", "idletasks")
                                   BType <<- svalue(BaselineType3)
                                   if (BType=="2P.Tougaard") {
                                      #fast generation of background do not need background reset
                                      delete(T1Frame3, T1group2)  #cancello il contenuto Frame Settings
                                      T1group2<<-ggroup(horizontal=FALSE, spacing=1, container = T1Frame3)
                                      glabel("   ", spacing=1, container=T1group2) #aggiungo spazio con una riga vuota
                                      glabel("   ", spacing=1, container=T1group2) #aggiungo spazio con una riga vuota
                                      add(T1Frame3, T1group2)
                                      reset.baseline()
                                      Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                      do.baseline(deg, Wgt, splinePoints)
                                   }
                                   if (BType=="3P.Tougaard") {
                                      #fast generation of background do not need background reset
                                      delete(T1Frame3, T1group2)  #to reset the widget T1group2
                                      T1group2<<-ggroup(horizontal=FALSE, spacing=1, container = T1Frame3)
                                      glabel("   ", spacing=1, container=T1group2) #to add  space in the widget
                                      glabel("   ", spacing=1, container=T1group2) #to add  space in the widget
                                      add(T1Frame3, T1group2) #add the new T1group2 widget
                                      reset.baseline()
                                      Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                      do.baseline(deg, Wgt, splinePoints)
                                   }
                                   if (BType=="4P.Tougaard") {
                                      splinePoints<<-list(x=NULL, y=NULL)  #reset preivous baseline
                                      reset.baseline()
                                      Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                      replot()
                                      delete(T1Frame3, T1group2)  #to reset the widget T1group2
                                      T1group2<<-ggroup(horizontal=FALSE, spacing=1, container = T1Frame3)#update T1group2
                                      glabel("C coeff.", spacing=1, container=T1group2)
                                      STgroup<<-ggroup(horizontal=TRUE, spacing=1, container = T1group2)
                                      weight<-gedit("0.5", width=20, spacing=1, container= STgroup)
                                      gbutton("Make BaseLine", handler = function(h, ...) {
                                                           Object[[coreline]]@Boundaries <<- point.coords
                                                           Wgt<<-as.numeric(svalue(weight))
                                                           do.baseline(deg, Wgt, splinePoints)
                                                           }, container= STgroup)
                                      add(T1Frame3, T1group2) #add the new T1group2 widget
                                   }
                   },container = T1Frame3)

  T1group2<-ggroup(horizontal=FALSE, spacing=1, container = T1Frame3)
  l1<-glabel("   ", spacing=1, container=T1group2)  #to add  space in the window frame
  l2<-glabel("   ", spacing=1, container=T1group2)  #to add  space in the window frame

  ActionFrame <- gframe(text = " Reset ", spacing=1, container = T1Frame1)
  gbutton("Reset Baseline", handler = function(h, ...) {
                   splinePoints<<-list(x=NULL, y=NULL)
		             svalue(BaselineType1, index=TRUE) <- -1
	            	 svalue(BaselineType2, index=TRUE) <- -1
            		 svalue(BaselineType3, index=TRUE) <- -1
                   reset.baseline()
                   Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                   replot()
                }, container = ActionFrame)

  T1Frame4 <- gframe(text = " Plot ", spacing=1, horizontal=TRUE, container = T1Frame1)

  ZRbutton<-gbutton("SET ZOOM REGION", handler = function(h, ...){
                   point.coords<<-NULL   #point.coords contain the X, Y data ranges
                   row1<-" => Right Clicks to define the 2 ZOOM REGION CORNERS (opposite in diagonal)"
                   row2<-"\n => Right click near corners to adjust Zoom Region Dimensions"
                   row3<-"\n => When Zoom Region OK, press MAKE ZOOM"
                   msg<-paste(row1, row2, row3, sep="")
                   gmessage( msg, icon="warning")
                   svalue(BaselineType1)<- -1
                   svalue(BaselineType2)<- -1
                   svalue(BaselineType3)<- -1
                   BType<<-""  #otherwise conflict between mouseRGT-selected points for zooming and for splinePoints
                   point.index <<- 3
                   reset.baseline()
                   point.coords$x[1]<<-min(Object[[coreline]]@.Data[[1]])
                   point.coords$x[2]<<-max(Object[[coreline]]@.Data[[1]])
                   point.coords$y[1]<<-min(Object[[coreline]]@.Data[[2]])
                   point.coords$y[2]<<-max(Object[[coreline]]@.Data[[2]])
                   if (Object[[coreline]]@Flags[1]) { #Binding energy set
                       point.coords$x<<-sort(c(point.coords$x[1], point.coords$x[2]), decreasing=TRUE) #pos$x in decreasing order
                   }
                   Corners$x<<-c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
                   Corners$y<<-c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
                   Marker <<- list(Points=Corners, col=3, cex=1.2, lwd=2.5, pch=3)
                   SetZoom <<- TRUE    #definition of zoom area disabled
                   FreezeLimits <<- TRUE  #reset spectrum range disabled
                   enabled(MZbutton)<-TRUE
                   enabled(ZObutton)<-TRUE
                   tkconfigure(img, cursor = "tcross")
                   replot()
                }, container = T1Frame4)

  MZbutton<-gbutton("MAKE ZOOM", handler = function(h, ...){
                   if (Object[[coreline]]@Flags[1]) { #Binding energy set
                       point.coords$x<<-sort(c(point.coords$x[1], point.coords$x[2]), decreasing=TRUE) #pos$x in decreasing order
                   } else {
                       point.coords$x<<-sort(c(point.coords$x[1], point.coords$x[2]), decreasing=FALSE) #pos$x in increasing order
                   }
                   point.coords$y<<-sort(c(point.coords$y[1], point.coords$y[2]), decreasing=FALSE) #pos$x in increasing order
                   Xlimits <<- point.coords$x  #Baseline edges == X zoom limits
                   Ylimits <<- point.coords$y
                   idx <- findXIndex(Object[[coreline]]@.Data[[1]], Xlimits[1])
                   point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][idx]
                   idx <- findXIndex(Object[[coreline]]@.Data[[1]], Xlimits[2])
                   point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][idx]
                   Object[[coreline]]@Boundaries <<- point.coords
                   BType<<-""  #otherwise conflict between mouseRGT-selected points for zooming and for splinePoints
                   svalue(BaselineType1)<- -1
                   svalue(BaselineType2)<- -1
                   svalue(BaselineType3)<- -1
                   point.index <<- 1
                   Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                   SetZoom <<- FALSE  #definition of zoom area disabled
                   FreezeLimits <<- TRUE #reset spectrum range disabled
                   tkconfigure(img, cursor = "crosshair")
                   replot()
                   enabled(ZObutton)<-TRUE
                }, container = T1Frame4)


  ZObutton<-gbutton("ZOOM OUT", handler = function(h, ...) {
                   Xlimits<<-range(Object[[coreline]]@.Data[1])  #Set plot limits to the whole coreline extension
                   Ylimits<<-range(Object[[coreline]]@.Data[2])
                   Object[[coreline]]@Boundaries$x <<- Xlimits
                   Object[[coreline]]@Boundaries$y <<- Ylimits
                   if ( hasBaseline(Object[[coreline]]) ) {
                       Xlimits<<-range(Object[[coreline]]@RegionToFit$x) #if Baseline present limit the
                       Ylimits<<-range(Object[[coreline]]@RegionToFit$y) #plot limits to the RegionToFit
                       LL<-length(Object[[coreline]]@RegionToFit$x)
                       point.coords$x[1] <<- Object[[coreline]]@RegionToFit$x[1]
                       point.coords$x[2] <<- Object[[coreline]]@RegionToFit$x[LL]
                       point.coords$y[1] <<- Object[[coreline]]@RegionToFit$y[1]
                       point.coords$y[2] <<- Object[[coreline]]@RegionToFit$y[LL]
                   } else {
                       LL<-length(Object[[coreline]]@.Data[[1]])
                       point.coords$x[1] <<- Object[[coreline]]@.Data[[1]][1]
                       point.coords$x[2] <<- Object[[coreline]]@.Data[[1]][LL]
                       point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][1]
                       point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][LL]
                       point.index<<-1
                   }
                   if (Object[[coreline]]@Flags[1]) { #Binding energy set
                       Xlimits<<-sort(Xlimits, decreasing=TRUE)  #pos$x in decreasing order
                   } else {
                       Xlimits<<-sort(Xlimits, decreasing=FALSE) #pos$x in increasing order
                   }
                   Ylimits<<-sort(Ylimits, decreasing=FALSE) #pos$ in increasing order
                   SetZoom <<- FALSE  #definition of zoom area disabled
                   FreezeLimits <<- FALSE #reset spectrum range enabled
                   Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                   replot()
                   enabled(MZbutton)<-FALSE
                   enabled(ZObutton)<-FALSE
                }, container = T1Frame4)







#----- TAB2: Components -----
  T2group1 <- ggroup(container = nbMain, spacing=1, horizontal=FALSE, label = "Components")

  nbComponents <- gnotebook(container = T2group1, expand = FALSE)
  T2group2  <- ggroup(container = nbComponents, spacing=1, horizontal=FALSE, label = " Add/Delete ")
  T2group3  <- ggroup(container = nbComponents, spacing=1, horizontal=FALSE, label = " Move ")


#----- Add/Delete component subtab
  T2Frame1 <- gframe(text = " Component LineShape ", spacing=1, container = T2group2)
  fit.type <- gcombobox(FitFunct, selected = 1, container = T2Frame1, handler = function(h, ...){ svalue(sb) <- sprintf("Selected component type %s", svalue(h$obj)) } )
  T2Frame2 <- gframe(text = " Action ", container = T2group2, horizontal=FALSE)
  add_btn <- gbutton("Add", container = T2Frame2, handler = add.component)
  del_btn <- gbutton("Delete", container = T2Frame2, expand=FALSE, handler = del.component )


#----- Move component subtab
  T2Frame3 <- gframe(text = " Select Component ", spacing=1, container = T2group3)
  FitComp<-gcombobox(CompNames, selected=-1, handler=function(h, ...){
                               compIndx<-svalue(FitComp)
                               compIndx<-unlist(strsplit(compIndx, split="C"))   #index of the selected component (numeric)
                               compIndx<<-as.integer(compIndx[2])
                               replot()
                           }, container = T2Frame3)

  T2Frame4 <- gframe(text = " Display ", container = T2group3, horizontal=TRUE)
  disp_area <- gcheckbox("Area table", checked=FALSE, container=T2Frame4, handler = update.outputArea )
  disp_quant <- gcheckbox("Quantification table", checked=FALSE, container=T2Frame4, handler = update.outputArea )

  ## plot type : Residual or simple
  T2Frame5 <- gframe(text = " Plot ", spacing=1, container = T2group1)
  plotFit <- gradio(items=c("normal", "residual"), selected=1, spacing=1, container = T2Frame5, expand = TRUE, horizontal = TRUE, handler = replot)



#----- SAVE&CLOSE button -----
  gseparator(container = Pgroup1) # separator
#  gimage(filename="atom.gif", dirname = dirname(system.file("Users/canteri/Downloads/atom.gif")), container = Pgroup1)
#  addSpring(Pgroup1)
  gseparator(container = Pgroup1)

  gbutton("RESET", container = Pgroup1, expand = FALSE, handler = function(h, ...){
		            svalue(BaselineType1, index=TRUE) <- -1
	               svalue(BaselineType2, index=TRUE) <- -1
            	   svalue(BaselineType3, index=TRUE) <- -1
                  BType<<-"linear"  #by default initial Baseline is linear
                  delete(T1Frame3, T1group2)  #cancello il contenuto Frame Settings
                  T1group2<<-ggroup(horizontal=FALSE, spacing=7, container = T1Frame3)
                  glabel("  ", container=T1group2) #add two empty row
                  glabel("  ", container=T1group2)
                  add(T1Frame3, T1group2)
                  Object<<-get(activeFName, envir = .GlobalEnv)
                  if (length(Object[[coreline]]@Components) > 0) {
                      Xlimits <<- range(Object[[coreline]]@RegionToFit[1])
                      Ylimits <<- range(Object[[coreline]]@RegionToFit[2])
                  } else if (length(Object[[coreline]]@Components) == 0) {
                      Xlimits <<- range(Object[[coreline]]@.Data[1])
                      Ylimits <<- range(Object[[coreline]]@.Data[2])
                      reset.baseline()
                      Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                      splinePoints<<-point.coords
                      svalue(nbMain)<<-1
                  }
                  enabled(MZbutton)<-FALSE
                  enabled(ZObutton)<-FALSE
                  replot()
              })

  gbutton("SAVE", container = Pgroup1, expand = FALSE, handler = function(h, ...){
                  coreline <- svalue(Core.Lines)
                  coreline<-unlist(strsplit(coreline, "\\."))
                  activeSpecIndx<-as.integer(coreline[1])
	               assign(Object_name, Object, envir = .GlobalEnv)
	               assign("activeSpectIndx", activeSpecIndx, envir = .GlobalEnv)
	               assign("activeSpectName", coreline, envir = .GlobalEnv)
	               XPSSaveRetrieveBkp("save")
              })

  gbutton("SAVE & EXIT", container = Pgroup1, expand = FALSE, handler = do.before.close  )

  gbutton("CLOSE", container = Pgroup1, expand = FALSE, handler=function(h,...){dispose(Pgroup1)}  )


#----- plot area -----

  HS <- VS <- 1.3+TKwin-1 # HS, VS are the horizontal and vertical scale factor for the TK graphic window
  #HS and VS may assume values typically in the range 1 - 2 with changes of 0.1
  #==>To have a correct corrispondence between mouse position and mouse coordinates transforming
  #positions mapped in tkrplot() in user coordinates (see my.coords() function) the vertical dimension of the
  #TK window must ALWAYS exceed that of the notebook i.e. no borders around the TK window must be present. 
  #VS = 1.3 is the minimum scale factor value for the TK window to achieve this condition.
  #The TKwin parameter set in Preferences() ranges form 1 to 2.5 and allows increase HS and VS scale factors.
  #For TKwin = 1 HS and VS assume the minimum value = 1.3 . When TKwin is changed in Preferences(), TKwin-1 is 
  #the way to obtain changes of 0.1 to apply to HS and VS to increase/decrease the TK window by a reasonable amounts.
  
  img <- tkrplot(getToolkitWidget(GraphGroup), fun = draw.plot, hscale=HS, vscale=VS)
  add(GraphGroup, img)
  ## interactivity
  tkbind(img, "<Button-1>", mouseLFT)
  tkbind(img, "<Button-3>", mouseRGT)
  tkbind(img, "<B1-Motion>", dragmousemove)
  tkconfigure(img, cursor = "crosshair")

  sb <- gstatusbar("status", container = MainWindow)


#----- Change tab handler -----
  addHandlerChanged(nbMain, handler=function(h,...) {
       nbPage<-svalue(nbMain, index=TRUE)
       coreline <- svalue(Core.Lines)
       coreline <- unlist(strsplit(coreline, "\\."))   #split string in correspondence of the point: coreline[1]=index, coreline[2]=spect name
       coreline<-as.numeric(coreline[1])               #this is the coreline index
  	    if ( nbPage > 1 ) { point.coords <<- list(x = NA, y = NA) }
 	    svalue(plotFit) <- "normal"
 	    svalue(sb) <- sprintf("On page %s", h$page.no)
       if (coreline>0 && length(Object[[coreline]]@RegionToFit)>0 ) {
          enabled(T2group1) <- TRUE 
       } #baseline already defined enable component selection
  	  }
  )


  enabled(T1Frame1) <- FALSE
  enabled(T2group1) <- FALSE
  enabled(MZbutton)<-FALSE
  enabled(ZObutton)<-FALSE

  visible(MainWindow) <- TRUE
  svalue(nbComponents) <- 2
  svalue(nbComponents) <- 1
  svalue(nbMain) <- 2
  svalue(nbMain) <- 1
  tcl("update", "idletasks")
  MainWindow$set_modal(TRUE)
}

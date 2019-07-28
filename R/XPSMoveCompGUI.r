#   Manual Move Components with tcltk and rpanel
#   FUNCTION CALL:  XPSMoveCompGUI(c2.pxt,CoreLine=2,hscale=1.8)

#'Function to modify component position and intensity in a fit
#'
#'Provides a userfriendly interface change position and intensity of each
#'individual fitting component of a selected XPSCoreline. Changes are saved in the
#'.GlobalEnv main software memory
#'
#'
#'@param XPSSample XPSSample object
#'@param Indx  index identifyinf the CoreLine to work on
#'@param hscale  dimension of the plotting window
#'@examples
#'
#'\dontrun{
#'	XPSMoveComp(SampData, 2, "medium")
#'}
#'
#'@export
#'


XPSMoveComp <- function(XPSSample, Indx, hscale) {

LoadCoreLine<-function(){
    XPSSample <<- get(ActiveFName, envir=.GlobalEnv)     #load the XPSSample data from main memory
    Indx <<- get("activeSpectIndx", envir=.GlobalEnv)    #get active Spectrum index
    Object<<-XPSSample[[Indx]]
    Xlimits<<-range(Object@RegionToFit$x)
    Ylimits<<-range(Object@RegionToFit$y)
    ComponentList<<-names(slot(Object,"Components"))
    if (length(ComponentList)==0) {
        gmessage("ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
        return()
    }
    delete(MCFrame, FitComp) # selecting a new core line needs a new gradio running on the coreline fit-components

    if (length(ComponentList)>1){    #gradio works with at least 2 items Less than 2 items gcheckbox will be used
       FitComp <<- gradio(ComponentList, selected=1, handler = function(h,...){   #gradio handler has to be redefined
                   refresh<<-TRUE
                   replot()   #replot spectrum without marker
                   Component<-svalue(FitComp)
                   Component<-as.numeric(unlist(strsplit(Component, split="C")))  #index of the selected component
                   Component<-Component[2]
                   xx<-Object@Components[[Component]]@param[2,1] #component position mu
                   Rng <- range(Object@RegionToFit[[1]])
                   if (xx < Rng[1]) {xx <- Rng[1]}
                   if (xx > Rng[2]) {xx <- Rng[2]}
                   yy<-Object@Components[[Component]]@param[1,1] #component height h
                   Estep<-abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                   Xindx<-which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
                   yy<-yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
                   coords[1] <<- xx
                   coords[2] <<- yy
                   refresh<<-FALSE #now plot also the component marker
                   replot()   #replot all
               },  container = MCFrame)
      } else {
         FitComp <<- gcheckboxgroup(ComponentList, checked=TRUE, handler = function(h,...){ #gradio handler has to be redefined
                   refresh<<-TRUE
                   replot()   #replot spectrum without marker
                   Component<-svalue(FitComp)
                   Component<-as.numeric(unlist(strsplit(Component, split="C")))
                   Component<-Component[2]
                   xx<-Object@Components[[Component]]@param[2,1] #component position mu
                   Rng <- range(Object@RegionToFit[[1]])
                   if (xx < Rng[1]) {xx <- Rng[1]}
                   if (xx > Rng[2]) {xx <- Rng[2]}
                   yy<-Object@Components[[Component]]@param[1,1] #component height h
                   Estep<-abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                   Xindx<-which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
                   yy<-yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
                   coords[1] <<- xx
                   coords[2] <<- yy
                   refresh<<-FALSE #now plot also the component marker
                   replot()   #plot all
              },  container = MCFrame)
     }
     xx<-Object@Components[[1]]@param[2,1] #component position mu
     yy<-Object@Components[[1]]@param[1,1] #component height h
     Estep<-abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
     Xindx<-which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
     yy<-yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
     coords[1] <<- xx
     coords[2] <<- yy
     refresh<<-FALSE #ora plotto anche il marker componente
     replot()   #riplotta spettro e marker componente selezionata
   }

  my.coord <- function(plot, xx, yy){     #giving cursor position in pixels here compute cursor coordinates
     coords<-NULL
     xClick<-as.numeric(unlist(strsplit(xx, split="x"))[1])
     yClick<-as.numeric(unlist(strsplit(yy, split="y"))[1])

     width <- as.numeric(tclvalue(tkwinfo("reqwidth",plot)))
     height <- as.numeric(tclvalue(tkwinfo("reqheight",plot)))
     yClick <- height - yClick  # window top left corner
     xMin <- parplt[1] * width
     xMax <- parplt[2] * width
     yMin <- parplt[3] * height
     yMax <- parplt[4] * height
     rangeX <- parusr[2] - parusr[1]
     rangeY <- parusr[4] - parusr[3]

     xx <- parusr[1]+(xClick-xMin)*rangeX/(xMax-xMin)
     yy <- parusr[3]+(yClick-yMin)*rangeY/(yMax-yMin) # orig

     Xlim1<-min(Object@RegionToFit[[1]])   #control over the possible X, Y range
     Xlim2<-max(Object@RegionToFit[[1]])
     Ylim1<-min(Object@RegionToFit[[2]])
     Ylim2<-max(Object@RegionToFit[[2]])
     if (xx < Xlim1 ) {xx <- Xlim1}
     if (xx > Xlim2 ) {xx <- Xlim2}
     if (yy < Ylim1 ) {yy <- Ylim1}
     if (yy > Ylim2 ) {yy <- Ylim2}

     Estep<-abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
     Xindx<-which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
     yy_BasLin<-yy-Object@Baseline$y[Xindx]  #spectral intensity at xx without Baseline
     coords<-c(xx, yy, yy_BasLin)
     return(coords)
  }

  LBmousedown <- function(xx, yy) {   #Left mouse button down
     coords  <<-  my.coord(img, xx, yy)
	  yy <- coords[2]
	  xx <- coords[1]
     if (Object@Flags[1]) { #Binding energy set
        if (xx > parusr[1] || xx < parusr[2] || yy < parusr[3] || yy > parusr[4]){ return() } #if you click ouside XY box of the plot do noting
     } else {
        Xlimits<<-sort(c(point.coords$x[1], point.coords$x[2]), decreasing=FALSE) #pos$x in increasing order
        if (xx < parusr[1] || xx > parusr[2] || yy < parusr[3] || yy > parusr[4]){ return() }
     }
     if ( SetZoom==FALSE ) { #left button works only when SET ZOOM REGION inactive
        do.move()
        XPSquantify(XPSSample)
        refresh<<-FALSE
     }
     replot()  }

  RBmousedown <- function(xx, yy) {   #Right mouse button down
     coords  <<-  my.coord(img, xx, yy)
	  yy <- coords[2]
	  xx <- coords[1]
     if (Object@Flags[1]) { #Binding energy set
        if (xx > parusr[1] || xx < parusr[2] || yy < parusr[3] || yy > parusr[4]){ return() } #if you click ouside XY box of the plot do noting
     } else {
        Xlimits<<-sort(c(point.coords$x[1], point.coords$x[2]), decreasing=FALSE) #pos$x in increasing order
        if (xx < parusr[1] || xx > parusr[2] || yy < parusr[3] || yy > parusr[4]){ return() }
     }
     if ( SetZoom==TRUE ) { #left button works only when SET ZOOM REGION button pressed
     	  point.coords$x[point.index] <<- coords[1]   #abscissa
     	  point.coords$y[point.index] <<- coords[2]   #ordinate
     	  if (point.index==1) {
     	     point.index<<-2    #to modify the second edge of the selected area
           Corners$x<<-c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
           Corners$y<<-c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
  	     }  else if (point.index==2) {
           Corners$x<<-c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
           Corners$y<<-c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
           point.index<<-3
  	     } else if (point.index==3) {
           D<-vector("numeric", 4)
           Dmin<-((point.coords$x[3]-Corners$x[1])^2 + (point.coords$y[3]-Corners$y[1])^2)^0.5  #valore di inizializzazione
           for (ii in 1:4) {
               D[ii]<-((point.coords$x[3]-Corners$x[ii])^2 + (point.coords$y[3]-Corners$y[ii])^2)^0.5  #dist P0 P1
               if(D[ii] <= Dmin){
                  Dmin<-D[ii]
                  idx=ii
               }
           }
           if (idx==1){
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
           if (Object@Flags[1]) { #Binding energy set
              point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE) #pos$x in decreasing order
              point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
           } else {
              point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE) #pos$x in increasing order
              point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
           }
        }
        replot()
      }
  }



  do.move <- function(...) {
     Component <- svalue(FitComp)
     if (length(Component)==0) {
         gmessage(msg="Select Component Please", title="WARNING", icon = "warning")
     } else {
        Component<-as.numeric(unlist(strsplit(Component, split="C")))   #index of the selected component
        Component<-Component[2]
        xx <- coords[1]
        yy <- coords[2]  #Component max value with baseline
        zz <- coords[3]  #Component max value without baseline
        FitFunct<-Object@Components[[Component]]@funcName
        newh<-GetHvalue(Object, Component, FitFunct, zz)  #Get value computes the Component value given the fit parameters and the Ymax value

        #range limits for mu
        varmu <- getParam(Object@Components[[Component]],variable="mu")
        minmu <- varmu$start-varmu$min
        maxmu <- varmu$max-varmu$start
        newmu <- c(xx, xx-minmu, xx+maxmu)
        #range limits for h
        varh <- getParam(Object@Components[[Component]],variable="h")
        minh <- varh$start-varh$min
        maxh <- varh$max-varh$start

        if (maxh > 0) {
            newh <- c(newh, 0, newh*5)    # No constraints on h
        }
        if (maxh==0){
            newh <- c(newh, newh, newh)    # h is fixed
        }
        if (maxh<0){
            newh <- c(newh, 0, newh*5)    # maxh cannot be <0: => force newH to correct values
        }
        if (varh$start <0) {
            newh <- c(0, 0, 1e5)   #set a positive value for an hypotheic fit
        }
        Object@Components[[Component]] <<- setParam(Object@Components[[Component]], parameter=NULL, variable="mu", value=newmu)
        Object@Components[[Component]] <<- setParam(Object@Components[[Component]], parameter=NULL, variable="h", value=newh)
        Object@Components[[Component]] <<- Ycomponent(Object@Components[[Component]], x=Object@RegionToFit$x, y=Object@Baseline$y) #calcola la Y eed aggiunge la baseline
# Fit computed addind fit components with the modified ones
        tmp <- sapply(Object@Components, function(z) matrix(data=z@ycoor))
        Object@Fit$y <<- ( colSums(t(tmp)) - length(Object@Components)*(Object@Baseline$y))

        Object <<- sortComponents(Object)
#if component order changed then rennumber them
        LL<-length(Object@Components) #N. fit components
        for (ii in 1:LL){
           if (xx == Object@Components[[ii]]@param["mu",1]) { #compare marker position with component positions
             indx<-ii
             break()
           }
        }
        svalue(FitComp)<-paste("C", indx, sep="")  #update component gradio
        XPSSample[[Indx]]<<-Object
     }
  }


  draw.plot <- function(...) {
     if (point.index==1 && refresh==FALSE) {  #point.index==1 when moving mcomponent
        plot(Object, xlim=Xlimits, ylim=Ylimits)
        points(x=coords[1], y=coords[2], col=2, cex=1.2, lwd=2, pch=1)  # if refresh==FALSE plot spectrum with component marker
     } else if (SetZoom == TRUE){   #set zoom area corners
	     if (point.index <= 2) {  #define zoom area corners
 	         plot(Object)
            points(point.coords, type="p", col=3, cex=1.2, lwd=2.5, pch=3)
  	     } else if (point.index>2){  #plot zoom area corners
 	         plot(Object, xlim=Xlimits, ylim=Ylimits)
            points(Corners, type="p", col=3, cex=1.2, lwd=2.5, pch=3)
            rect(point.coords$x[1], point.coords$y[1], point.coords$x[2], point.coords$y[2])
        }
     }  #else {
#        plot(Object, xlim=Xlimits, ylim=Ylimits)
#     }
     parusr <<- par('usr')
     parplt <<- par('plt')
     svalue(StatusBar) <- sprintf(paste("x =",round(coords[1],1), " y =",round(coords[2]), sep=" "))
  }

  reset.plot <- function(h, ...) {
       point.coords$x <<- range(Object@RegionToFit$x) #set original X range
       point.coords$y <<- range(Object@RegionToFit$y) #set original Y range
       Object@Boundaries <<- point.coords
       Xlimits<<-point.coords$x
       Ylimits<<-sort(point.coords$y, decreasing=FALSE)
       Corners<<- point.coords
       parusr <<- par("usr")
       parplt <<- par("plt")
       tkconfigure(img, cursor = "crosshair")
       replot()
  }


  replot <- function(...) { tkrreplot(img) }


  refresh.plot <- function(...) {
     plot(Object)
     tkrreplot(img)
  }

  do.before.close <- function(...) {
    dispose(MCWindow)
    plot(XPSSample[[Indx]])
  }


# --- Varables definition ---
     ActiveFName<-get("activeFName", envir=.GlobalEnv)   #XPS data name
     XPSSample<-get(ActiveFName, envir=.GlobalEnv)       #load XPSdata values from main memory
     SpectName <- get("activeSpectName", envir=.GlobalEnv)
     Object<-XPSSample[[Indx]]
     ComponentList<-names(slot(Object,"Components"))

     coords<-c(xx=NA, yy=NA, yy_BasLin=NA)
     CompCoords<-c(xx=NA, yy=NA, yy_BasLin=NA)
     point.coords <- list(x=NA, y=NA)
     vscale <- hscale
     xx<-NULL
     yy<-NULL
     Corners <- point.coords
     point.index <- 1

     refresh<-TRUE
     SetZoom <- FALSE
     NoFit<-FALSE
#Coreline boundaries
     if (length(ComponentList)==0) {
        gmessage("ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
        Xlimits<-range(Object@.Data[1])
        Ylimits<-range(Object@.Data[2])
        NoFit<-TRUE
     } else {
        LL<-length(Object@.Data[[1]])
        point.coords$x <- range(Object@RegionToFit$x) #set the X window extension == x range
        point.coords$y <- range(Object@RegionToFit$y) #set the Y window extension == y range
        Xlimits<-range(Object@RegionToFit$x)
        Ylimits<-range(Object@RegionToFit$y)
        Object@Boundaries$x <- Xlimits
        Object@Boundaries$y <- Ylimits
     }


#--- Window definition ---
     MCWindow <- gwindow("XPS MOVE COMPONENT", visible = FALSE)
     MCGroup <- ggroup(horizontal=TRUE, spacing = 5, container=MCWindow)

#--- Selection Group ---
     SelectGroup <- ggroup(horizontal=FALSE, spacing = 5, container=MCGroup)

     MCFrame <- gframe(text = " COMPONENTS ", container = SelectGroup)
     if (length(ComponentList)>1){    #gradio works with at least 2 items
         FitComp <<- gradio(ComponentList, selected=1, handler = function(h,...){
                               refresh<<-TRUE    #cancel previous selections
                               replot()   #plot spectrum without marker
                               Component<-svalue(FitComp)
                               Component<-as.numeric(unlist(strsplit(Component, split="C")))   #index selected component
                               Component<-Component[2]
                               xx<-Object@Components[[Component]]@param[2,1] #component position mu
                               Rng <- range(Object@RegionToFit[[1]])
                               if (xx < Rng[1]) {xx <- Rng[1]}
                               if (xx > Rng[2]) {xx <- Rng[2]}
                               yy<-Object@Components[[Component]]@param[1,1] #component height h
                               FuncName<-Object@Components[[Component]]@funcName
                               yy<-yy/GetHvalue(Object,Component, FuncName, 1)
                               Estep<-abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx<-which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
                               yy<-yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
                               coords[1] <<- xx
                               coords[2] <<- yy
                               refresh<<-FALSE #now plot also the component marker
                               replot()   #replot spectrum and marker
                           },  container = MCFrame)
      } else {
         FitComp <<- gcheckboxgroup(ComponentList, checked=TRUE, handler = function(h,...){
                               refresh<<-TRUE    #cancel previous component markers
                               replot()   #plot spectrum only
                               Component<-svalue(FitComp)
                               Component<-as.numeric(unlist(strsplit(Component, split="C")))   #index selected compoent
                               Component<-Component[2]
                               xx<-Object@Components[[Component]]@param[2,1] #component position mu
                               Rng <- range(Object@RegionToFit[[1]])
                               if (xx < Rng[1]) {xx <- Rng[1]}
                               if (xx > Rng[2]) {xx <- Rng[2]}
                               Estep<-abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx<-which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
                               yy<-yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                               coords[1] <<- xx
                               coords[2] <<- yy
                               refresh<<-FALSE #now plot spectrum + component marker
                               replot()
                           },  container = MCFrame)
     }

     CCLbutton<-gbutton(" Change Core Line ", handler=function(h,...){
                               CLmainwin<-gwindow("CORELINE SELECTION", parent=MCWindow, visible=FALSE)
                               CLgroup <- ggroup(label="", horizontal=FALSE, container=CLmainwin)

                               CLframe <-gframe(text="SELECT THE CORELINE", spacing=5, container=CLgroup)
                               SpectList<-XPSSpectList(ActiveFName)
                               CLobj <- gradio(SpectList, selected=Indx, editable=FALSE, handler=function(h,...){
                                    XPSComponent<-svalue(CLobj)
                                    XPSComponent<-unlist(strsplit(XPSComponent, "\\."))   #drop "NUMber." in component name
                                    Indx<-as.integer(XPSComponent[1])
                                    SpectName<-XPSComponent[2]
                                    assign("activeSpectName", SpectName,envir=.GlobalEnv) #set activeSpectName == last selected spectrum
                                    assign("activeSpectIndx", Indx,envir=.GlobalEnv) #set the activeIndex == last selected spectrum
                                    LoadCoreLine()
                                    XPSSample<-get(ActiveFName, envir=.GlobalEnv)
                                    XPSSample[[Indx]]
                                    plot(XPSSample[[Indx]])
                                    dispose(CLmainwin)
                               }, container=CLframe)
                               visible(CLmainwin)<-TRUE
                           }, container = SelectGroup)

     LBFitbutton<-gbutton(" FIT Lev.Marq. ", handler=function(h,...){
                               Component<-svalue(FitComp)
                               Object<<-XPSFitLM(Object)
                               xx<-Object@Components[[Component]]@param[2,1] #component position mu
                               yy<-Object@Components[[Component]]@param[1,1] #component height h
                               Estep<-abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx<-which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
                               yy<-yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                               coords[1] <<- xx #coords of marker of the first fit component
                               coords[2] <<- yy
                               Object <<- sortComponents(Object)
                               refresh<<-FALSE  #now plot also the component marker
                               replot(Object)
                           }, container = SelectGroup)

     MFFitbutton<-gbutton(" FIT Modfit   ", handler=function(h,...){
                               Component<-svalue(FitComp)
                               Object<<-XPSModFit(Object)
                               xx<-Object@Components[[Component]]@param[2,1] #component position mu
                               yy<-Object@Components[[Component]]@param[1,1] #component height h
                               Estep<-abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx<-which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]]<xx+Estep/2)
                               yy<-yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                               coords[1] <<- xx
                               coords[2] <<- yy
                               Object <<- sortComponents(Object)
                               refresh<<-FALSE #now plot also the component marker
                               replot(Object)
                           }, container = SelectGroup)

     ZRbutton<-gbutton("SET ZOOM REGION", handler = function(h, ...){
                               CompCoords<<-coords   #save the of position component_marker
                               point.coords<<-NULL   #point.coords contain the X, Y data ranges
                               enabled(CCLbutton)<-FALSE
                               enabled(LBFitbutton)<-FALSE
                               enabled(MFFitbutton)<-FALSE
                               enabled(RSTbutton)<-FALSE
                               row1<-" => Right Clicks to define the 2 ZOOM REGION CORNERS (opposite in diagonal)"
                               row2<-"\n => Right click near corners to adjust Zoom Region Dimensions"
                               row3<-"\n => When Zoom Region OK, press MAKE ZOOM"
                               msg<-paste(row1, row2, row3, sep="")
                               gmessage( msg, icon="warning")
                               SetZoom<<-TRUE
                               tkconfigure(img, cursor = "tcross")
                           }, container = SelectGroup)

     MZbutton<-gbutton("MAKE ZOOM", handler = function(h, ...){
                               if (Object@Flags[1]) { #Binding energy set
                                  point.coords$x<-sort(point.coords$x, decreasing=TRUE) #pos$x in decreasing order
                                  point.coords$x[1]<-point.coords$x[1]
                                  point.coords$x[2]<-point.coords$x[2]
                               } else {
                                  point.coords$x<-sort(point.coords$x, decreasing=FALSE) #pos$x in increasing order
                                  point.coords$x[1]<-point.coords$x[1]
                                  point.coords$x[2]<-point.coords$x[2]
                               }
                               Xlimits<<-point.coords$x
                               Ylimits<<-sort(point.coords$y, decreasing=FALSE)
 	                            slot(Object,"Boundaries") <<- point.coords
 	                            point.index<<-1
 	                            coords<<-CompCoords #restore of position component_marker
 	                            refresh<<-FALSE
                               SetZoom<<-FALSE
                               tkconfigure(img, cursor = "crosshair")
                               replot()
                               enabled(CCLbutton)<-TRUE
                               enabled(LBFitbutton)<-TRUE
                               enabled(MFFitbutton)<-TRUE
                               enabled(RSTbutton)<-TRUE
                          }, container = SelectGroup)

     RSTbutton<-gbutton("RESET PLOT", handler = function(h, ...) {
                               SetZoom<<-FALSE
                               refresh<<-FALSE
  	                            point.index<<-1
  	                            reset.plot()
                         }, container = SelectGroup)

     gbutton("    SAVE      ", handler=function(h,...){
#    With button SAVE the Component parameters are updated and are now available for FiTConstraints
                               Indx <- get("activeSpectIndx", envir=.GlobalEnv)
                               XPSSample[[Indx]]<<-Object
                               assign(ActiveFName, XPSSample, envir = .GlobalEnv)
                               plot(XPSSample[[Indx]])
                               XPSSaveRetrieveBkp("save")
                           }, container = SelectGroup)

     gbutton(" RE-LOAD DATA ", handler=function(h,...){
                               LoadCoreLine()
                           }, container = SelectGroup)

     gbutton("    EXIT      ", handler=function(h,...){
                               do.before.close()
                               XPSSaveRetrieveBkp("save")
                           }, container = SelectGroup)

     StatusBar <- gstatusbar("status", container = MCWindow)

     Plotgroup <- ggroup(horizontal=FALSE, spacing = 5, container=MCGroup)

#--- TKRPLOT interactivity------
     img <- tkrplot(getToolkitWidget(Plotgroup), fun = draw.plot, hscale=hscale, vscale=hscale)

     add(Plotgroup, img)
     tkbind(img, "<Button-1>", LBmousedown)   #left mouse button
     tkbind(img, "<Button-3>", RBmousedown)   #right mouse button
     tkconfigure(img, cursor = "crosshair")

     enabled(CCLbutton)<-TRUE
     enabled(LBFitbutton)<-TRUE
     enabled(MFFitbutton)<-TRUE
     enabled(RSTbutton)<-TRUE
     visible(MCWindow) <- TRUE

#--- Marker-----
     if (NoFit==FALSE){
        coords[1]<-Object@Components[[1]]@param[2,1] #component position mu
        coords[2]<-Object@Components[[1]]@param[1,1] #component1 height h
        FuncName<-Object@Components[[1]]@funcName
        coords[2]<-coords[2]/GetHvalue(Object,1, FuncName, 1)
        Estep<-abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
        Xindx<-which(Object@RegionToFit[[1]]>coords[1]-Estep/2 & Object@RegionToFit[[1]]<coords[1]+Estep/2) #indice del vettore X corrispondente alla posizione della componente
        coords[2]<-coords[2]+Object@Baseline$y[Xindx]
        refresh<-FALSE
        replot()    
        refresh<-TRUE
     }
}

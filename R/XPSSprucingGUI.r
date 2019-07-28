#-----------------------------------------
# XPS processing with gWidgets2 and tcltk
#-----------------------------------------
#'Sprucing XPSSample datas
#'
#'GUI to correct original XPS spectral data
#'
#'@param Object XPSSample object name
#'@param coreline numeric index identifying the current XPS CoreLine
#'@param plot_win Window size default=1.3
#'@return Returns the \code{Object} with a coreline added.
#'
#'@examples
#'
#'\dontrun{
#'	XPSextractGUI(SampData, Index, "medium")
#'}
#'
#'@export
#'

XPSSprucingGUI <- function(Object, coreline, plot_win = 1.3) {

    .my.coords <- function(plot, x, y, parusr, parplt) {

    xClick <- as.numeric(x)
    yClick <- as.numeric(y)
    width  <- as.numeric(tclvalue(tkwinfo("reqwidth",plot)))
    height <- as.numeric(tclvalue(tkwinfo("reqheight",plot)))

    xMin <- parplt[1] * width
    xMax <- parplt[2] * width
    yMin <- parplt[3] * height
    yMax <- parplt[4] * height

    rangeX <- parusr[2] - parusr[1]
    rangeY <- parusr[4] - parusr[3]

    yClick <- yClick+0.5 # orig
    yClick <- height - yClick  # orig

    xPlotCoord <- parusr[1]+(xClick-xMin)*rangeX/(xMax-xMin)
    yPlotCoord <- parusr[3]+(yClick-yMin)*rangeY/(yMax-yMin) # orig
    return(c(xPlotCoord, yPlotCoord))
  }


dragmouse <- function(x, y) {
      coords <<- .my.coords(img, x, y, parusr, parplt)
      point.coords$x[point.index] <<- coords[1]
      point.coords$y[point.index] <<- coords[2]
      replot()
}

mouseup <- function(x, y) {
		coords  <<- .my.coords(img, x, y, parusr, parplt)
     	point.coords$x[point.index] <<- coords[1]   #abscissa
     	point.coords$y[point.index] <<- coords[2]   #ordinate
     	if (point.index==1) {
     	   point.index<<-2    #to modify the second edge of the selected area
         Corners$x<<-c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
         Corners$y<<-c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
  	   } else if (point.index==2) {
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
         if (Object[[coreline]]@Flags[1]) { #Binding energy set
            point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
            point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
         } else {
            point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE) #ordina pos$x in ordine crescente
            point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
         }
      }
      replot()
}

undo.plot <- function(...){
      if (SelReg==1) {
         reset.boundaries()
     	   replot()
      } else if (SelReg>1) {
	      Object[[coreline]]@Boundaries$x <<- OldCoords$x
	      Ylimits <<- OldCoords$y
     	   replot()
  	   }
}

draw.plot <- function(...) {
      Xlimits <- Object[[coreline]]@Boundaries$x
	   if (point.index <= 2) {
	  	   plot(Object[[coreline]], xlim=Xlimits)
         points(point.coords, col="red", cex=1, lwd=1.5, pch=3)
 	   } else if (point.index>2){
	  	   plot(Object[[coreline]], xlim=Xlimits, ylim=Ylimits)
         points(Corners, type="p", col="red", cex=1, lwd=1.5, pch=3)
         rect(point.coords$x[1], point.coords$y[1], point.coords$x[2], point.coords$y[2])
	   }
	   svalue(statbar) <- sprintf(paste("x =",round(coords[1], 1), " y =",round(coords[2], 1), sep=" "))
      parusr <<- par("usr")
      parplt <<- par("plt")

}


replot <- function(...) { tkrreplot(img) }


reset.boundaries <- function(h, ...) {
	    Object[[coreline]] <<- XPSremove(Object[[coreline]], "all")
       LL<-length(Object[[coreline]]@.Data[[1]])
       point.coords$x[1] <<- Object[[coreline]]@.Data[[1]][1] #ascissa primo estremo 1 del survey
       point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][1] #ordinata primo estremo 1 del survey
       point.coords$x[2] <<- Object[[coreline]]@.Data[[1]][LL] #ascissa  secondo estremo del survey
       point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][LL] #ordinata secondo estremo del survey
       slot(Object[[coreline]],"Boundaries") <<- point.coords
       Ylimits<<-c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
       OldCoords <<- point.coords #for undo
       Corners <- point.coords
       point.index <<- 1
       parusr <<- par("usr")
       parplt <<- par("plt")
       replot()
  }

do.editRegion <- function(h, ...){
            idx1<-point.coords$x[1]
            idx2<-point.coords$x[2]
            newcoreline<-Object[[coreline]]
            idx1<-findXIndex(unlist(newcoreline@.Data[1]), point.coords$x[1]) #indice relativo al limite superiore (inferiore KE?) della RegionToFit
            idx2<-findXIndex(unlist(newcoreline@.Data[1]), point.coords$x[2]) #indice relativo al limite inferiore (superiore KE?) della RegionToFit
            tmp <- unlist(Object[[coreline]]@.Data[1]) #estraggo le ascisse regione selezionata
            newcoreline@.Data[[1]] <- tmp[idx1:idx2]     #aggiungo e ascisse regione selezionata
            tmp <- unlist(Object[[coreline]]@.Data[2]) #estraggo le ordinate regione selezionata
            newcoreline@.Data[[2]] <- tmp[idx1:idx2]     #aggiungo le ordinate regione selezionata
            DataTable<-as.data.frame(cbind(newcoreline@.Data[[1]], newcoreline@.Data[[2]]))
            names(DataTable)<-c("  X  ", "  Y  ")
            delete(gframe23, label23)
            XPSgdf<-gdf(items=DataTable, container=gframe23)
            add(gframe23, XPSgdf)
            size(XPSgdf)<-c(200,180)
            addHandlerChanged(XPSgdf, handler=function(h,...){ #addHandlerChanged scarica il dataframe modificato in NewFirParam che e' salvato al di fuori di saveFitParam attraverso la <<-
                     DataTable <<- h$obj[]
            })
            buttOK<-gbutton("OK", container=gframe23, handler=function(h, ...){
                     Object[[coreline]]@.Data[[1]][idx1:idx2]<<-DataTable[[1]]
                     Object[[coreline]]@.Data[[2]][idx1:idx2]<<-DataTable[[2]]
                     delete(gframe23, XPSgdf)
                     delete(gframe23, buttOK)
                     delete(gframe23, buttCanc)
                     label23<-glabel(" Data to correct:  ", container=gframe23)
                     replot(Object[[coreline]])
            })
		      buttCanc<-gbutton("Cancel", container=gframe23, handler=function(...) {
                     delete(gframe23, XPSgdf)
                     delete(gframe23, buttOK)
                     delete(gframe23, buttCanc)
                     reset.boundaries()
                     label23<-glabel(" Data to correct:  ", container=gframe23)
            })
  }

do.before.close <- function(...) {
	 assign(activeFName, Object, envir = .GlobalEnv)
    dispose(SPwin)
    plot(Object)
    XPSSaveRetrieveBkp("save")
  }

#====== VARIABLES DEFINITION=======

  point.coords <- list(x=NA,y=NA)
  point.index <- 1
  coords <- NA # for printing mouse coordinates on the plot
  xx<-NULL
  yy<-NULL
#Coreline boundaries
  LL<-length(Object[[coreline]]@.Data[[1]])
  point.coords$x[1] <- Object[[coreline]]@.Data[[1]][1] #ascissa primo estremo 1 del survey
  point.coords$y[1] <- Object[[coreline]]@.Data[[2]][1] #ordinata primo estremo 1 del survey
  point.coords$x[2] <- Object[[coreline]]@.Data[[1]][LL] #ascissa  secondo estremo del survey
  point.coords$y[2] <- Object[[coreline]]@.Data[[2]][LL] #ordinata secondo estremo del survey
  Ylimits<-c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
  OldCoords <- point.coords #for undo
  Corners <- point.coords
  SelReg <- 0

  Object[[coreline]]@Boundaries$x <- c(point.coords$x)
  Object[[coreline]]@Boundaries$y <- c(point.coords$y)

  parusr <- NA
  parplt <- NA

#====== Widget definition =======
  SPwin <- gwindow("XPS SPRUCING GUI", expand=TRUE, visible = FALSE)
#  size(SPwin) <- c(1000, 800)
  MainGroup <- ggroup(horizontal = TRUE, container = SPwin)
  SPlayout <- glayout(homogeneous=FALSE, spacing=3, container=MainGroup)
  ## Core lines
  SPlayout[1, 1] <- OptGroup <- ggroup(horizontal = FALSE, expand=TRUE, spacing = 5, container = SPlayout)

  gframe20 <- gframe(text = " Help ", container = OptGroup)
  HelpLab <- glabel("Set the region edges with the cursors and press Select", container=gframe20)
  font(HelpLab)<-list(family="sans",size=12)


  gframe22 <- gframe(text = " Select Data ", container = OptGroup, horizontal = FALSE)
  gbutton(" SELECT REGION ", container = gframe22, handler = function(h, ...){
              OldCoords <<- Object[[coreline]]@Boundaries
              SelReg <<- SelReg+1
              rngX<-range(point.coords$x)
              rngX<-(rngX[2]-rngX[1])/20
              rngY<-range(point.coords$y)
              rngY<-(rngY[2]-rngY[1])/20

              if (Object[[coreline]]@Flags[1]) { #Binding energy set
                 point.coords$x<-sort(point.coords$x, decreasing=TRUE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
                 point.coords$x[1]<-point.coords$x[1]+rngX/20
                 point.coords$x[2]<-point.coords$x[2]-rngX/20
              } else {
                 point.coords$x<-sort(point.coords$x, decreasing=FALSE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
                 point.coords$x[1]<-point.coords$x[1]-rngX/20
                 point.coords$x[2]<-point.coords$x[2]+rngX/20
              }
              point.coords$y<-sort(point.coords$y, decreasing=FALSE)
              Ylimits<<-c(point.coords$y[1]-rngY/10, point.coords$y[2]+rngY/10)
 	           slot(Object[[coreline]],"Boundaries") <<- point.coords
              replot()
         } )

  gbutton(" EDIT REGION ", handler = function(h, ...){
              do.editRegion()
  	      }, container = gframe22 )

  gbutton(" UNDO ", handler = function(h, ...) {
  	           undo.plot()
  	      }, container = gframe22 )

  gbutton("RESET BOUNDARIES", handler = function(h, ...) {
  	           reset.boundaries()
  	      }, container = gframe22 )

  gframe23 <- gframe(text = " Sprucing ", horizontal=FALSE, container = OptGroup)
  label23<-glabel(" Data to correct:  \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n", container=gframe23)

  ## CLOSE button
  addSpring(OptGroup)
  gseparator(container = OptGroup)

  gbutton("Save", container = OptGroup, handler = function(h, ...){
	           assign(activeFName, Object, envir = .GlobalEnv)
  	           reset.boundaries()
  	           XPSSaveRetrieveBkp("save")
         } )


  gbutton("Save & Close", handler = do.before.close, container = OptGroup )

  ## status bar
  statbar <- gstatusbar("status", container = SPwin)


#====== PLOT SECTION =======
  SPlayout[1,2] <- img <- tkrplot(getToolkitWidget(SPlayout), fun = draw.plot, hscale=plot_win, vscale=plot_win)
#  img <- tkrplot(getToolkitWidget(MainGroup), fun = draw.plot, hscale=plot_win, vscale=plot_win)
#  add(MainGroup, img)
  addSpring(MainGroup)

  ## interactivity
#  tkbind(img, "<Button-1>", mousedown)
  tkbind(img, "<B1-Motion>", dragmouse)
  tkbind(img, "<ButtonRelease-1>", mouseup)
  tkconfigure(img, cursor = "crosshair")

  visible(SPwin) <- TRUE

#====== Markers at Coreline extremes =======

  refresh<<-FALSE #ora plotto anche il marker componente
# points(point.coords, col=2, cex=1.2, lwd=2, pch=1)
  replot()   #riplotta spettro e marker componente selezionata

}

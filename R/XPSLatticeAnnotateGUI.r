#Funzione per aggiungere Labels a grafici

#'Adds text to a plot produced using the Lattice package
#'This function is called by (\code{XPSOverlay}) and (\code{XPSCustomPlot})
#'
#'@param Xlim the limits of the X axis
#'@param Ylim the limits of the Y axis
#'
#'@examples
#'
#'\dontrun{
#'	XPSLattAnnotate(Xlim, Ylim)
#'}
#'
#'@export
#'

XPSLattAnnotate <- function(Xlim,Ylim){

   CtrlPlot <- function(){
            #CARICO LE OPZIONI DI PLOT SELEZIONATE
#               AnnotateText<-get("A.AnnotateText", envir=MyEnv)
#               TextPosition<-get("A.TextPosition", envir=MyEnv)
#               TextSize<-get("A.TextSize", envir=MyEnv)
#               TextColor<-get("A.TextColor", envir=MyEnv)
               if (TextColor=="Color"){TextColor <<- SpectColor}

               panel.text(x=TextPosition$x,y=TextPosition$y, pos=4, labels=AnnotateText,cex=TextSize,col=TextColor)
               trellis.unfocus()
   }


#--- variables ---
   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FName<-get(activeFName, envir=.GlobalEnv)
   ActiveFName<-get("activeFName", envir=.GlobalEnv)
   SpectIndx<-get("activeSpectIndx", envir=.GlobalEnv)
   SpectList<-XPSSpectList(ActiveFName)   #List of the CoreLines in FName

   Colors<-XPSSettings$Colors
#   Colors<-c("black", "red", "limegreen", "blue", "magenta", "orange", "grey", "cyan", "sienna", "cadetblue", "darkgreen", "grey45", "gold", "violet", "yellow", "lightblue", "turquoise", "pink", "wheat", "thistle")
   TxtSize<-c(0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,3)
   TextPosition<-list(x=NA, y=NA)
   TextSize<-1
   TextColor<-"black"
   SpectColor<-"black"
   AnnotateText<-"?"
   RecPlot<-NULL   #salvo il plot prima di Annotare per poter fare UNDO
   AcceptedPlot <- recordPlot()   #salvo il plot iniziale prima di Annotare per poter fare UNDO
#   assign("A.TextPosition",TextPosition, envir=MyEnv)
#   assign("A.TextSize",TextSize, envir=MyEnv)
#   assign("A.TextColor",TextColor, envir=MyEnv)
#   assign("A.AnnotateText",AnnotateText, envir=MyEnv)
#   assign("A.SpectColor","black", envir=MyEnv)
#   assign("A.RecPlot",RecPlot, envir=MyEnv)
#   assign("A.AcceptedPlot",AcceptedPlot, envir=MyEnv)

#inizializzazione variabili relative allo spettro
   SampData<-as(FName[[SpectIndx]],"matrix") #store spectrum baseline etc in a matrix


#===== G-Window =====

     Annwin <- gwindow("ANNOTATE", visible=FALSE)
     size(Annwin)<-c(270,350)

     Angroup <- ggroup(label="ANNOTATE", horizontal=FALSE, container=Annwin)
     
     INFOframe <-  infoframe <- gframe(text="INFO", horizontal=FALSE, spacing=1, container=Angroup)
     glabel(text="1. Set label and locate position", horizontal=FALSE, container=INFOframe)
     glabel(text="2. Change size and color if needed", horizontal=FALSE, container=INFOframe)
     glabel(text="3. ACCEPT if label OK or UNDO", horizontal=FALSE, container=INFOframe)

     Anframe1 <- gframe(text="Text", spacing=5, container=Angroup)
     T7obj1 <- gedit("", container=Anframe1)
     addHandlerChanged(T7obj1,handler=function(h,...){
                            AnnotateText <<- svalue(T7obj1)
#                            assign("A.AnnotateText", AnnotateText, envir=MyEnv)
                         })

     Anframe2 <- gframe(text=" Set Text Position", spacing=5, container=Angroup)
     gbutton(" LOCATE TEXT ", handler=function(h,...){
                            RecPlot <<- recordPlot()   #save the figure before annotation to make UNDO if needed
                            trellis.focus("panel", 1, 1, clip.off=TRUE, highlight=FALSE)
                            pos<-list(x=NULL, y=NULL)
                            pos<- grid.locator(unit = "points")
                            X1<-min(Xlim)
                            if (FName[[SpectIndx]]@Flags[1]) X1<-max(Xlim)   #Binding Energy Set
                            RangeX<-abs(Xlim[2]-Xlim[1])
                            Y1<-min(Ylim)
                            RangeY<-Ylim[2]-Ylim[1]

                            width <- max(convertX(unit(Xlim, "native"), "points", TRUE))
                            height <- max(convertY(unit(Ylim, "native"), "points", TRUE))
                            if (FName[[SpectIndx]]@Flags[1]){
                               TextPosition$x <<- X1-as.numeric(pos$x)*RangeX/width+RangeX/35  #Binding Energy Set
                            } else {
                               TextPosition$x <<- X1+as.numeric(pos$x)*RangeX/width-RangeX/35  #Kinetic energy scale
                            }
                            TextPosition$y <<- Y1+as.numeric(pos$y)*RangeY/height+RangeY/50
                            if (length(TextPosition)==0)  {
                               return()   
                            }
#                            assign("A.TextPosition",TextPosition, envir=MyEnv)
                            TextSize <<- as.numeric(svalue(AnnoteSize))
#                            assign("A.TextSize",TextSize, envir=MyEnv)
#                            assign("A.RecPlot",RecPlot, envir=MyEnv)
                            CtrlPlot()
                         }, container=Anframe2)

     Anframe5 <- gframe(text="Text Size", spacing=5, container=Angroup)
     AnnoteSize <- gcombobox(TxtSize,selected=3, handler=function(h,...){
#                            TextPosition<-get("A.TextPosition", envir=MyEnv)
                            if (is.na(TextPosition)) {
                                gmessage(msg="Please set the Label Position first!", title="WARNING: position lacking", icon="warning")
                            } else {
#                              AcceptedPlot<-get("A.AcceptedPlot", envir=MyEnv) #recupera il grafico prima dell-ultima aggiunta ANNOTATE
                              replayPlot(AcceptedPlot)
                              TextSize <<- as.numeric(svalue(AnnoteSize))
#                              assign("A.TextSize",TextSize, envir=MyEnv)
                              RecPlot <<- recordPlot()   #save the figure before annotation to make UNDO if needed
#                              assign("A.RecPlot",RecPlot, envir=MyEnv)
                              trellis.focus("panel", 1, 1, clip.off=TRUE, highlight=FALSE)
                              CtrlPlot()
                            }
                         }, container=Anframe5)

     Anframe6 <- gframe(text="Text Color", spacing=5, container=Angroup)
     AnnoteColor <- gcombobox(Colors,selected=1, editable=FALSE, handler=function(h,...){
#                            TextPosition<-get("A.TextPosition", envir=MyEnv)
                            if (is.na(TextPosition)) {
                                gmessage(msg="Please set the Label Position first!", title="WARNING: position lacking", icon="warning")
                            } else {
#                              AcceptedPlot<-get("A.AcceptedPlot", envir=MyEnv) #recupera il grafico prima dell-ultima aggiunta ANNOTATE
                              replayPlot(AcceptedPlot)
                              TextColor <<- svalue(AnnoteColor)
#                              assign("A.TextColor",TextColor, envir=MyEnv)
                              RecPlot <<- recordPlot()   #save the figure before annotation to make UNDO if needed
#                              assign("A.RecPlot",RecPlot, envir=MyEnv)
                              trellis.focus("panel", 1, 1, clip.off=TRUE, highlight=FALSE)
                              CtrlPlot()
                            }
                         }, container=Anframe6)

     gbutton("ADD ARROW", handler=function(h,...){
                            color <- svalue(AnnoteColor)
                            pos1<-locator(n=1, type="p", pch=20, col=color) #first plot the two arrow edges
                            pos2<-locator(n=1, type="n")
		                      arrows(pos1$x, pos1$y, pos2$x, pos2$y, length = 0.05, col = color)
                         }, container=Angroup)

     gbutton(" ACCEPT ", handler=function(h,...){
                            AcceptedPlot <<- recordPlot()   #accept plot when figure is OK
#                            assign("A.AcceptedPlot",AcceptedPlot, envir=MyEnv)
                         }, container = Angroup)

     ResetCK<-gbutton("UNDO ANNOTATE", handler=function(h,...){
#                            RecPlot <- get("A.RecPlot", envir=MyEnv) #recupera il grafico prima dell-ultima aggiunta ANNOTATE
                            replayPlot(RecPlot)
                            trellis.unfocus()
                         }, container=Angroup)

     gbutton(" EXIT ", handler=function(h,...){
                            trellis.unfocus()
                            dispose(Annwin)
                         }, container = Angroup)


     visible(Annwin) <- TRUE
}


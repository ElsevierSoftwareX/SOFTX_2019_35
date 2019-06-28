#Funzione per aggiungere Labels a grafici

#'Annotate: graphic function to add text to plots
#'
#'Add text to spectra plotted using (\code{matplot})base function.
#'No parameters are passed to the this function
#'
#'@seealso \link{matplot}, \link{plot}
#'@examples
#'
#'\dontrun{
#'	annotate()
#'}
#'
#'@export
#'


XPSAnnotate <- function(){

   CtrlPlot <- function(){
            #Load selected options for the plot
               AnnotateText<-get("A.AnnotateText", envir=MyEnv)
               TextPosition<-get("A.TextPosition", envir=MyEnv)
               TextSize<-get("A.TextSize", envir=MyEnv)
               TextColor<-get("A.TextColor", envir=MyEnv)
               XYlimits<-get("A.XYlimits", envir=MyEnv)
               if (TextColor=="Color"){TextColor<-SpectColor}
               if (FName[[SpectIndx]]@Flags[1]){
                   TextPosition$x[1]<-TextPosition$x[1]-(XYlimits[2]-XYlimits[1])/40 #aggiusto la coord X in relazione al cursore del locate
               } else {
                   TextPosition$x[1]<-TextPosition$x[1]+(XYlimits[2]-XYlimits[1])/40 #aggiusto la coord X in relazione al cursore del locate
               }
               TextPosition$y[1]<-TextPosition$y[1]+(XYlimits[4]-XYlimits[3])/80 #aggiusto la coord Y in relazione al cursore del locate
               text(labels=AnnotateText,x=TextPosition$x,y=TextPosition$y,pos=4,cex=TextSize,col=TextColor)
   }




######### variabili #############
   rm(list = ls(envir = MyEnv, all.names = TRUE), envir = MyEnv)

   FName<-get(activeFName, envir=.GlobalEnv)   #Load the active XPSSample
   ActiveFName<-get("activeFName", envir=.GlobalEnv)  #Load the XPSSample name
   SpectIndx<-get("activeSpectIndx", envir=.GlobalEnv)#index of Active CoreLine
   SpectList<-XPSSpectList(ActiveFName)                   #List of all Corelines in the XPSSample

   NComp=length(FName[[SpectIndx]]@Components)

   FitComp1<-""  #Vector containinig the Fit Components
   for (ii in 1:NComp){
      FitComp1[ii]<-paste("C",ii, sep="")
   }
   Colors<-c("black", "red", "limegreen", "blue", "magenta", "orange", "grey", "cyan", "sienna", "cadetblue", "darkgreen", "grey45", "gold", "violet", "yellow", "lightblue", "turquoise", "pink", "wheat", "thistle")
   LType<-c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "F8", "431313", "22848222")
   LWidth<-c(1,1.5,2,2.5,3,3.5,4,4.5,5)
   LCol<-24    #set color line to black
   LW<-1       #set the linewidth to 1
   FontSize<-c(0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
   Title<-""
   
   Legenda<-"FALSE"
#   Pos<-c("topleft", "left", "bottomleft", "top", "center", "bottom", "topright", "right", "bottomright")
   TxtCol<-c("Color", "Black")
   TxtSize<-c(0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2) #,2.2,2.4,2.6,2.8,3)
   TextPosition<-list(x=NA, y=NA)
   TextSize<-1
   TextColor<-"black"
   AnnotateText<-"?"
   RecPlot<-NULL
   AcceptedPlot<-recordPlot()   #save the graph for UNDO option
#   replayPlot(AcceptedPlot)
   assign("A.TextPosition",TextPosition, envir=MyEnv)
   assign("A.TextSize",TextSize, envir=MyEnv)
   assign("A.TextColor",TextColor, envir=MyEnv)
   assign("A.AnnotateText",AnnotateText, envir=MyEnv)
   assign("A.SpectColor","black", envir=MyEnv)
   assign("A.RecPlot",RecPlot, envir=MyEnv)
   assign("A.AcceptedPlot",AcceptedPlot, envir=MyEnv)

#Initialization vars of the XPSSample
   SampData<-as(FName[[SpectIndx]],"matrix") #save the CoreLine with baseline and fit in a matrix
   NC<-ncol(SampData)
   assign("A.NC", NC, envir=MyEnv)
   XYlimits<-par("usr")
   assign("A.XYlimits", XYlimits, envir=MyEnv)

#===== Graphic Library Cntrl ===
   MatPlotMode<-get("MatPlotMode", envir=.GlobalEnv)
   if (MatPlotMode==FALSE){
      gmessage(msg="Overlay or CustomPlot active: DoubleClick on your XPSsample", title = "BAD GRAPHIC MODE",  icon = "error")
      return()
   }

#===== GUI =====

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
                            AnnotateText<-svalue(T7obj1)
                            assign("A.AnnotateText", AnnotateText, envir=MyEnv)
                         })

     Anframe2 <- gframe(text=" Set Text Position", spacing=5, horizontal=FALSE, container=Angroup)
     
     AnGroup2 <- ggroup(horizontal=TRUE, container=Anframe2)
     gbutton(" TEXT POSITION ", handler=function(h,...){
                            TextPosition<-locator(n=1, type="n")
                            assign("A.TextPosition",TextPosition, envir=MyEnv)
                            txt<-round(x=c(TextPosition$x, TextPosition$y), digits=2)
                            txt<-paste("X: ", as.character(txt[1]), ", Y: ", as.character(txt[2]), sep="")
                            svalue(AnnotePosition)<-txt
                         }, container=AnGroup2)
     AnnotePosition<-glabel("Position: ", container = AnGroup2)

     gbutton(" ADD TEXT ", handler=function(h,...){
                            TextSize <- as.numeric(svalue(AnnoteSize))
                            assign("A.TextSize",TextSize, envir=MyEnv)
                            RecPlot<-recordPlot()   #save graph for UNDO option
                            assign("A.RecPlot",RecPlot, envir=MyEnv)
                            CtrlPlot()
                         }, container=Anframe2)
     

     Anframe5 <- gframe(text="Text Size", spacing=5, container=Angroup)
     AnnoteSize <- gcombobox(TxtSize,selected=4, handler=function(h,...){
                            TextPosition<-get("A.TextPosition", envir=MyEnv)
                            if (is.na(TextPosition)) {
                                gmessage(msg="Please set the Label Position first!", title="WARNING: position lacking", icon="warning")
                            } else {
                              AcceptedPlot<-get("A.AcceptedPlot", envir=MyEnv) #Load the graph before the last annotation change
                              replayPlot(AcceptedPlot)
                              TextSize <- as.numeric(svalue(AnnoteSize))
                              assign("A.TextSize",TextSize, envir=MyEnv)
                              RecPlot<-recordPlot()   #save the graph for UNDO option
                              assign("A.RecPlot",RecPlot, envir=MyEnv)
                              CtrlPlot()
                            }
                         }, container=Anframe5)

     Anframe6 <- gframe(text="Text Color", spacing=5, container=Angroup)
     AnnoteColor <- gcombobox(Colors,selected=1, editable=FALSE, handler=function(h,...){
                            TextPosition<-get("A.TextPosition", envir=MyEnv)
                            if (is.na(TextPosition)) {
                                gmessage(msg="Please set the Label Position first!", title="WARNING: position lacking", icon="warning")
                            } else {
                              AcceptedPlot<-get("A.AcceptedPlot", envir=MyEnv) #Load the graph before the last annotation change
                              replayPlot(AcceptedPlot)
                              TextColor <- svalue(AnnoteColor)
                              assign("A.TextColor",TextColor, envir=MyEnv)
                              RecPlot<-recordPlot()   #save the graph for UNDO option
                              assign("A.RecPlot",RecPlot, envir=MyEnv)
                              CtrlPlot()
                            }
                         }, container=Anframe6)

     gbutton("ADD ARROW", handler=function(h,...){
                            color <- svalue(AnnoteColor)
                            pos1<-locator(n=1, type="p", pch=20, col=color) #first mark the arrow start point
                            pos2<-locator(n=1, type="n") #the arrow ending point
		                      arrows(pos1$x, pos1$y, pos2$x, pos2$y, length = 0.05, col = color)
                         }, container=Angroup)

     gbutton(" ACCEPT ", handler=function(h,...){
                            AcceptedPlot<-recordPlot()   #accept the annotation in the graph NO UNDO now possible.
                            assign("A.AcceptedPlot",AcceptedPlot, envir=MyEnv)
                         }, container = Angroup)

     ResetCK<-gbutton("UNDO ANNOTATE", handler=function(h,...){
                            RecPlot<-get("A.RecPlot", envir=MyEnv) #Load the graph before the last annotation change
                            replayPlot(RecPlot)
                         }, container=Angroup)

     gbutton(" EXIT ", handler=function(h,...){
                            rm(list = ls(envir = MyEnv, all.names = TRUE), envir = MyEnv)
                            dispose(Annwin)
                         }, container = Angroup)


     visible(Annwin) <- TRUE
}


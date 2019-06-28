#function to set the dimensions of the analysis window (XPSGUI.r) and personal settings
#XPSsettings structure
#General	                  Colors	   LType	   Symbols	   SymIndx	BaseColor Comp.Color	FitColor
#
#Courier	                  black	      solid	   VoidCircle	     1	cadetblue	grey45	orangered
#normal	                  red3	      dashed	VoidSquare	     0	cadetblue	grey45	orangered
#10	                     limegreen	dotted	VoidTriangleUp	  2	cadetblue	grey45	orangered
#1.8	                     blue	      dotdash	VoidTriangleDwn  6	cadetblue	grey45	orangered
#1486.6	                  magenta	   longdash	Diamond	        5	cadetblue	grey45	orangered
#windows(xpos=600, ypos=5) orange	   twodash	SolidCircle	     16	cadetblue	grey45	orangered
#personal WD               cadetblue	F8	      SolidSquare	     15	cadetblue	grey45	orangered
# NA                       sienna	   431313	SolidTriangleUp  17	cadetblue	grey45	orangered
# NA	                     darkgrey	   22848222	SolidTriangleDwn 25	cadetblue	grey45	orangered
# NA	                     forestgreen	12126262	SolidDiamond	  18	cadetblue	grey45	orangered
# NA	                     gold	      12121262	X	              4	cadetblue	grey45	orangered
# NA	                     darkviolet	12626262	Star	           8	cadetblue	grey45	orangered
# NA	                     greenyellow	52721272	CrossSquare	     7	cadetblue	grey45	orangered
# NA	                     cyan	      B454B222	CrossCircle	     10	cadetblue	grey45	orangered
# NA	                     lightblue	F313F313	SolidDiamond	  18	cadetblue	grey45	orangered
# NA	                     dodgerblue	71717313	DavidStar	     11	cadetblue	grey45	orangered
# NA	                     deeppink3	93213321	SquareCross	     12	cadetblue	grey45	orangered
# NA	                     wheat	      66116611	SquareTriang	  14	cadetblue	grey45	orangered
# NA	                     thistle	   23111111	CircleCross	     13	cadetblue	grey45	orangered
# NA	                     grey40	   222222A2	Cross	           3	cadetblue	grey45	orangered




#'To select a dimensions of the graphic window depending on the dimensions of the screen used
#'
#'To select a dimensions of the graphic window depending on the dimensions of the screen used
#'No parameters are passed to this function
#'
#'
#'@examples
#'
#'\dontrun{
#'	XPSSetWinSize()
#'}
#'
#'@export
#'


XPSPreferences <- function() {


   MatchSymbol<-function(Sym,SymIndx,ii){
            switch(Sym,
                  "VoidCircle" = {SymIndx[ii]<-1},
                  "VoidSquare" = {SymIndx[ii]<-0},
                  "VoidTriangleUp" = {SymIndx[ii]<-2},
                  "VoidTriangleDwn" = {SymIndx[ii]<-6},
                  "Diamond" = {SymIndx[ii]<-5},
                  "SolidCircle" = {SymIndx[ii]<-16},
                  "SolidSquare" = {SymIndx[ii]<-15},
                  "SolidTriangleUp" = {SymIndx[ii]<-17},
                  "SolidTriangleDwn" = {SymIndx[ii]<-25},
                  "SolidDiamond" = {SymIndx[ii]<-18},
                  "X" = {SymIndx[ii]<-4},
                  "Star" = {SymIndx[ii]<-8},
                  "CrossSquare" = {SymIndx[ii]<-7},
                  "CrossCircle" = {SymIndx[ii]<-10},
                  "CrossDiamond" = {SymIndx[ii]<-9},
                  "DavidStar" = {SymIndx[ii]<-11},
                  "SquareCross" = {SymIndx[ii]<-12},
                  "SquareTriang" = {SymIndx[ii]<-14},
                  "CircleCross" = {SymIndx[ii]<-13},
                  "Cross" = {SymIndx[ii]<-3},
                  "Bullet" = {SymIndx[ii]<-20},
                  "FilledCircle" = {SymIndx[ii]<-21},
                  "FilledSquare" = {SymIndx[ii]<-22},
                  "FilledDiamond" = {SymIndx[ii]<-23},
                  "FilledTriangleUp" = {SymIndx[ii]<-24})
            return(SymIndx)
   }




#---variables
   XraySource<-c("Al", "Mg")               #X-ray source (at moment not used)
   OSList<-c("MicroSoft-Device", "MacOS-Device", "Linux-Device")#Possible operating systems
   fontPreferences<-list(font=c("Courier", "LucidaConsole", "Consolas", "SimplifiedArabicFixed", "OCRA-Extended"),   #fonnt used in the quantification table
#   fontPreferences<-list(font=c("Courier", "sans", "helvetica", "times", "monospace"),   #font used in the quantification table
                        style=c("normal", "italic", "oblique"),                                                      #font style
                        size=c(8, 10, 12, 14))                                                                       #font size

#   XPSSettings<-get("XPSSettings", envir=.GlobalEnv)
   FontPref<-list(Font="", Style="", Size="")
   FontPref$Font<-XPSSettings$General[1]
   FontPref$Style<-XPSSettings$General[2]
   FontPref$Size<-XPSSettings$General[3]
   WinSize<-XPSSettings$General[4]
   XSource<-XPSSettings$General[5]
   Gdev<-XPSSettings$General[6]
   WorkingDir<-XPSSettings$General[7]  #personal Working Dir
   Colors<-XPSSettings$Colors
   LType<-XPSSettings$LType
   Symbols<-XPSSettings$Symbols
#   XPSSettings$SymIndx
   BaseLineColor<-XPSSettings$BaseColor[1]
   ComponentColor<-XPSSettings$ComponentsColor[1]
   FitColor<-XPSSettings$FitColor[1]
   Colors<-encodeString(as.character(Colors), width=20, justify="left")
   LType<-encodeString(as.character(LType), width=20, justify="left")
   Symbols<-encodeString(as.character(Symbols), width=20, justify="left")
   GraphParam<-data.frame(Colors=Colors, LType=LType, Symbols=Symbols)
   FitParam<-data.frame(BaseLineColor=BaseLineColor, ComponentColor=ComponentColor, FitColor=FitColor)

#---GUI                       BaseLineColor ComponentColor  FitColor
   mainwin <- gwindow("PREFERENCES", visible=FALSE)
   size(mainwin)<-c(350,490)
   maingroup <- ggroup(horizontal=TRUE, container=mainwin)
   group0 <- ggroup(horizontal=FALSE, container=maingroup)

   group1 <- ggroup(horizontal=FALSE, container=group0)
   frameDim <-gframe(text=" WINDOW DIMENSIONS ",horizontal=FALSE, spacing=5, container=group1)
   LabBox<-ggroup(spacing=1, horizontal=TRUE, container=frameDim)
   glabel("WinSize: ", container=LabBox)
   WSvalue<-glabel("1.8", container=LabBox)
#   WinObj1<-gslider(from = 10, to = 30, by = 2, value = 20, horizontal=TRUE, handler=function(h,...){
   WinObj1<-gslider(from = 1, to = 2.5, by = 0.1, value = 1.8, horizontal=TRUE, handler=function(h,...){
                       wsize<-svalue(WinObj1)
#                       WinSize <<- 0.1*as.numeric(wsize)
                       WinSize <<- as.numeric(wsize)
                       svalue(WSvalue)<-WinSize
                 }, container=frameDim)

   frameDev <- gframe(" SET THE DEVICE FOR YOUR GRAPHIC WINDOW", spacing=5, container=group1)
   GrDevice <- gradio(OSList, horizontal=FALSE, selected=-1, handler=function(h,...){
                      OS<-svalue(GrDevice)
                      if (OS=="Windows") {Gdev<-"X11(xpos=600, ypos=5)"} #top right position
                      if (OS=="MacOS-X") {Gdev<-"quartz()"} #quartz() doesn't allow to set the opening position
                      if (OS=="Linux") {Gdev<-"X11(xpos=600, ypos=5)"}
                 }, container = frameDev) # function(h,...){ }



   group2 <- ggroup(horizontal=TRUE, container=group0)
   frameFont <-gframe(text=" FONT ", horizontal=TRUE, spacing=5, container=group2)
   FontObj <- gradio(fontPreferences$font, selected=-1, handler=function(h,...){
                       Font<-svalue(FontObj)
                       FontPref$Font<<-Font
                    }, container=frameFont)

   frameX <-gframe(text=" X-RAY EXCITATION SOURCE ",  spacing=5, container=group0)
   Xobj <- gradio(XraySource, horizontal=TRUE, selected=1, handler=function(h,...){
                       Source<-svalue(Xobj)
                       if (Source == "Al") {XSource<<-1486.6}
                       if (Source == "Mg") {XSource<<-1254.6}
                    }, container=frameX)

   gbutton("SET and EXIT", handler=function(h,...){
#--- get System info and apply correspondent XPS Settings ---
                       OS<-Sys.info() #get system information
#Reading XPS settings which can be customized by users
                       Ini.pthName <- "/RxpsG/data/XPSSettings.ini"
                       LibPth <- .libPaths()  #path of all the R libraries where RxpsG could be located
                       LL <- length(LibPth)
                       Ini.pthName <- paste(LibPth, Ini.pthName, sep="") #paste acts on all the components of the LibPth vector
                       if (OS["sysname"] != "Linux") { #Windows and Mac OS systems
                          Ini.pthName <- gsub("/","\\", Ini.pthName, fixed=TRUE)   #path/filename for linux,  path\\filename for windows
                       }
                       for(ii in 1:LL){
                          fe <- file.exists(Ini.pthName[ii])
                          if (fe == TRUE) {
                              XPSSettings$General[1]<<-FontPref$Font
                              XPSSettings$General[2]<<-FontPref$Style
                              XPSSettings$General[3]<<-FontPref$Size
                              XPSSettings$General[4]<<-WinSize
                              XPSSettings$General[5]<<-XSource
                              XPSSettings$General[6]<<-Gdev
                              XPSSettings$General[7]<<-WorkingDir  #personal Working Dir
                              for (jj in 8:20){ XPSSettings$General[jj] <<- NA }
                              XPSSettings$Colors<<-gsub("\\s", "",GraphParam$Colors)   #removes all the blank spaces from  Color string vector
                              XPSSettings$LType<<-gsub("\\s", "",GraphParam$LType)     #removes all the blank spaces from  LType string vector
                              XPSSettings$Symbols<<-gsub("\\s", "",GraphParam$Symbols) #removes all the blank spaces from  Symbols string vector
                              for(jj in 1:20){
                                 XPSSettings$SymIndx<<-MatchSymbol(XPSSettings$Symbols[jj],XPSSettings$SymIndx,jj)
                              }
                              XPSSettings$BaseColor<<-gsub("\\s", "",FitParam$BaseLineColor)   #removes all the blank spaces from  Color string vector
                              XPSSettings$ComponentsColor<<-gsub("\\s", "",FitParam$ComponentColor)   #removes all the blank spaces from  Color string vector
                              XPSSettings$FitColor<<-gsub("\\s", "",FitParam$FitColor)   #removes all the blank spaces from  Color string vector
                              ColNames<<-names(XPSSettings)

                              write.table(XPSSettings, file = Ini.pthName[ii], sep=" ", eol="\n", row.names=FALSE, col.names=ColNames)
                              assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                              break
                          }
                       }
                       if (fe == FALSE) {
                          gmessage(msg="ATTENTION: XPSSettings.ini file is lacking. Check RxpsG package", title = "WARNING",icon = "warning" )
                       }
                       dispose(mainwin)
                    }, container = group0)


   frameStyle <-gframe(text=" STYLE ", horizontal=TRUE, spacing=5, container=group2)
   StyleObj <- gradio(fontPreferences$style, selected = -1, handler=function(h,...){
                       Style<-svalue(StyleObj)
                       FontPref$Style<<-Style
                    }, container=frameStyle)

   frameSize <-gframe(text=" SIZE ", horizontal=TRUE, spacing=5, container=group2)
   SizeObj <- gradio(fontPreferences$size, selected=-1, handler=function(h,...){
                       Size<-svalue(SizeObj)
                       FontPref$Size<<-Size
                    }, container=frameSize)

   group3 <- ggroup(horizontal=FALSE, container=maingroup)
   frameGStyle<-gframe(text="PLOT GRAPHIC STYLE", horizontal=FALSE, spacing=10, container=group3)
   GStyle<-gdf(items=GraphParam, container=frameGStyle)
   size(GStyle)<-c(400,340)

   addHandlerChanged(GStyle, handler=function(h,...){
                       GP<-h$obj[]
                       GP$Colors<-encodeString(as.character(GP$Colors), width=20, justify="left")
                       GP$LType<-encodeString(as.character(GP$LType), width=20, justify="left")
                       GP$Symbols<-encodeString(as.character(GP$Symbols), width=20, justify="left")
                       GraphParam<<-GP
                    })

   frameGStyle<-gframe(text="FIT GRAPHIC STYLE", horizontal=FALSE, spacing=10, container=group3)
   FitStyle<-gdf(items=FitParam, container=frameGStyle)
   size(FitStyle)<-c(400,50)

   addHandlerChanged(FitStyle, handler=function(h,...){
                       FP<-h$obj[]
                       FP$BaseLineColor<-encodeString(as.character(FP$BaseLineColor), width=25, justify="left")
                       FP$ComponentColor<-encodeString(as.character(FP$ComponentColor), width=25, justify="left")
                       FP$FitColor<-encodeString(as.character(FP$FitColor), width=25, justify="left")
                       FitParam<<-FP
                    })

   WDFrame <- gframe(text="SELECT NEW WORKING DIR", horizontal=TRUE, spacing=5, container=group3)
   newDir<-gbutton("Browse Dir", spacing=1, handler=function(h, ...){
                     SysName<-Sys.info()
                     SysName<-SysName[1]
                     if (SysName=="Linux") {
                        WDir <- tk_choose.dir()
                     } else {
                        WDir <- choose.dir(default="", caption="SELECT YOUR WORKING DIRECTORY")
                     }
                     WorkingDir <<- paste(dirname(WDir), "/", basename(WDir), sep="") #exchanges backslash from \\ to /
                     ForbidChars <- c("-")
                     xxx <- sapply(ForbidChars, grep, x=WorkingDir)
                     xxx <- sapply(xxx, length )
                     if (sum(xxx)>0) {
                         gmessage(msg="WARNING: Forbidden Character '-' in the Path or Filename. Please remove!" , title = "Working Dir",  icon = "warning")
                         return()
                     }
                     setwd(WorkingDir)
                     assign("WDirPath",WorkingDir, envir=.GlobalEnv)
                     cat("\n New Working Directory: ", WorkingDir)
                     ShortPathName <- WorkingDir
                     if (nchar(ShortPathName) > 40){   #cut workingDir to less than 40 chars
                        splitPathName<-strsplit(ShortPathName, "/")
                        LL<-length(splitPathName[[1]])
                        HeadPathName<-paste(splitPathName[[1]][1],"/", splitPathName[[1]][2], "/ ... ", sep="")
                        ShortPathName<-paste(HeadPathName, substr(splitPathName, LL-30, LL), sep="")
                     }
                     svalue(dispWD) <- ShortPathName
                   }, container=WDFrame)
   dispWD <- glabel(" ", container=WDFrame)

   visible(mainwin) <- TRUE
}


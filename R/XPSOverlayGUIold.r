#function to perform plots in overlay mode

#function to perform plots in overlay mode

#'Performs overlay of XPS-Spectra
#'
#'Provides a userfriendly interface to select XPS-Corelines to overlay
#'and a selection of plotting options for a personalized data representation
#'This function is based on the (/code{Lattice}) Package.  No parameters passed
#'No parameters passed to this function
#'
#'
#'@examples
#'
#'\dontrun{
#'	XPSOverlay()
#'}
#'
#'@export 
#'            


#Plot_Args$factor$labels ->  Plot_Args$labels   Plot_Args$strip$strip.custom$var.name



XPSOverlay <- function(){

#---- chiama la macro che crea il plot in base alle opzioni scelte -----
   CtrlPlot <- function(){
            SelectedNames<-get("O.SelectedNames", envir=MyEnv)
            XPSovEngine(PlotParameters, Plot_Args, AutoKey_Args, SelectedNames)
   }


#----- Aggiorna tabella selezione spettri & corelines -----
   RefreshTab<-function(SelectedNames){
       delete(T1group1, NameTable)
       TabNames<-list(XPSSample=c(SelectedNames$XPSSample, " "), CoreLines=c(SelectedNames$CoreLines, " "))
       LL<-length(TabNames$XPSSample)
       for(ii in 1:LL){
          TabNames$XPSSample[ii]<-encodeString(TabNames$XPSSample[ii], width=44, justify="right")
          TabNames$CoreLines[ii]<-encodeString(TabNames$CoreLines[ii], width=44, justify="right")
       }
       TabNames<-as.data.frame(TabNames)
       NameTable<<-gtable(TabNames, expand=TRUE, container=T1group1) #tabella con la lista dei files selezionati
   }

#--- Routine for drawing Custom Axis
   CustomAx <- function(CustomDta){

               AxWin <- gwindow(title="CUSTOM AXIS", visible=FALSE)
               AxGroup1 <- ggroup(horizontal=FALSE, container=AxWin)
               txt1="1) Set Xmin, Xmax and the number of ticks on the X axis: es. Xmin=0, Xmax=10, Nticks=5"
               txt2="2) Set Tick-Labels (as many labels as the ticks): es. Tick Labels= A,B,C,D,E"
               glabel(txt1, container=AxGroup1)
               glabel(txt2, container=AxGroup1)
               AxFrame <- gframe("SET SCALE RANGE", horizontal=FALSE, container=AxGroup1)
               AxLayout <- glayout(homogeneous=FALSE, spacing=3, container=AxFrame)
               X1<-as.character(round(CustomDta[[1]], 2))
               X2<-as.character(round(CustomDta[[2]], 2))
               msg<-paste("Xmin (min value=", X1, "):", sep="")
               AxLayout[1,1] <- EditXmin<-gedit(initial.msg =msg, width=40, container=AxLayout)
               msg<-paste("Xmax (max value=", X2, "):", sep="")
               AxLayout[1,2] <- EditXmax<-gedit(initial.msg =msg, width=40, container=AxLayout)
               AxLayout[2,1] <- EditNTicks<-gedit(initial.msg ="N.Ticks", container=AxLayout)

               gbutton("     SAVE & EXIT      ", handler=function(h,...){
                        AXmin<-as.numeric(svalue(EditXmin))
                        AXmax<-as.numeric(svalue(EditXmax))
                        NTicks<-as.numeric(svalue(EditNTicks))
                        if (is.null(NTicks)){
                            gmessage("Please N. Major Ticks  required!", icon="warning")
                        } else {
                           dx<-(AXmax-AXmin)/NTicks
                           AXstp<-seq(from=AXmin, to=AXmax, by=dx)
                           Ticklabels<-as.character(round(AXstp,digit=1))
                           if (CustomDta[[3]] == "X") {
                              Plot_Args$scales$x<<-list(at=AXstp, labels=Ticklabels)
                           } else if (CustomDta[[3]] == "Y") {
                              Plot_Args$scales$y<<-list(at=AXstp, labels=Ticklabels)
                           }
                           dispose(AxWin)  # finito di editare i parametri annullo la finestra
                           CtrlPlot()
                           Plot_Args$scales$relation<<-"same"
                        }
               }, container = AxFrame)

               visible(AxWin) <- TRUE
    }



   SetLinesPoints <- function(){
         if ( svalue(T3_SetLines) == "OFF" && svalue(T3_SetSymbols) == "OFF") {
            Plot_Args$type<<-" "  #both: line and symbols
            AutoKey_Args$lines<<-FALSE
            AutoKey_Args$points<<-FALSE
            AutoKey_Args$col<<-"white"
            PlotParameters$Colors<<-"white"
            Plot_Args$par.settings$superpose.symbol$col<<-"white"
            Plot_Args$par.settings$superpose.symbol$pch<<-STypeIndx[1]
         }

         if ( svalue(T3_SetLines) == "ON" && svalue(T3_SetSymbols) == "OFF") {
            Plot_Args$type<<-"l"
            AutoKey_Args$lines<<-TRUE
            AutoKey_Args$points<<-FALSE
            AutoKey_Args$col<<-Colors
            PlotParameters$Colors<<-Colors
            Plot_Args$lty<<-LType
            Plot_Args$par.settings$superpose.line$col<<-Colors #Rainbow plot
            Plot_Args$par.settings$superpose.line$lty<<-"solid"
            if (svalue(T3_BW_Col)=="B/W") {
               AutoKey_Args$col<<-"black"
               PlotParameters$Colors<<-"black"
               Plot_Args$par.settings$superpose.line$col<<-"black" #B/W plot
               Plot_Args$par.settings$superpose.line$lty<<-LType
            }
         }

         if ( svalue(T3_SetLines) == "OFF" && svalue(T3_SetSymbols) == "ON") {
            Plot_Args$type<<-"p"  #both: line and symbols
            AutoKey_Args$lines<<-FALSE
            AutoKey_Args$points<<-TRUE
            AutoKey_Args$col<<-Colors
            PlotParameters$Colors<<-Colors
            Plot_Args$pch<<-STypeIndx
            Plot_Args$par.settings$superpose.symbol$col<<-Colors
            Plot_Args$par.settings$superpose.symbol$pch<<-STypeIndx[1]
            if (svalue(T3_BW_Col)=="B/W") {
               AutoKey_Args$col<<-"black"
               PlotParameters$Colors<<-"black"
               Plot_Args$par.settings$superpose.symbol$pch<<-STypeIndx
               Plot_Args$par.settings$superpose.symbol$col<<-"black"
            }
         }

         if ( svalue(T3_SetLines) == "ON" && svalue(T3_SetSymbols) == "ON") {
            Plot_Args$type<<-"b"  #both: line and symbols
            AutoKey_Args$lines<<-TRUE
            AutoKey_Args$points<<-TRUE
            Plot_Args$lty<<-LType
            Plot_Args$pch<<-STypeIndx
            if (svalue(T3_BW_Col)=="B/W") {
               AutoKey_Args$col<<-"black"
               PlotParameters$Colors<<-"black"
               Plot_Args$par.settings$superpose.line$lty<<-LType
               Plot_Args$par.settings$superpose.line$col<<-"black" #B/W plot
               Plot_Args$par.settings$superpose.symbol$pch<<-STypeIndx
               Plot_Args$par.settings$superpose.symbol$col<<-"black"
            } else {
               AutoKey_Args$col<<-Colors
               PlotParameters$Colors<<-Colors
               Plot_Args$par.settings$superpose.line$lty<<-"solid"
               Plot_Args$par.settings$superpose.line$col<<-Colors #Rainbow plot
               Plot_Args$par.settings$superpose.symbol$pch<<-STypeIndx[1]
               Plot_Args$par.settings$superpose.symbol$col<<-Colors
            }
         }
         CtrlPlot()
   }

setFileCheckBox<-function(){
         SelectedNames<-get("O.SelectedNames", envir=MyEnv) #inizialmente O.SelectedNames==(NULL, NULL)
         SaveSel<-get("O.SaveSelection", envir=MyEnv)
         FNameList<-svalue(T1FNameListCK)
         if (length(FNameList)==0){       #se ho deselezionato l'ultimo FName
             LL<-length(SelectedNames$XPSSample)  #rimuovo FName dalla lista dei files selezionati
             SelectedNames$XPSSample<-SelectedNames$XPSSample[-LL]
             LL<-length(SelectedNames$CoreLines)
             if (SelectedNames$CoreLines[LL]=="-----") {
                 SelectedNames$CoreLines<-SelectedNames$CoreLines[-LL]
             }
             RefreshTab(SelectedNames)   #aggiorno la tabella con in nomi dei files scelti
             assign("O.SelectedNames", SelectedNames, envir=MyEnv)
             NCoreLines<-0
             assign("O.NCoreLines", NCoreLines, envir=MyEnv)
             assign("O.NamesList", SelectedNames, envir=MyEnv)
             assign("O.SaveSelection", TRUE, envir=MyEnv)  #precedenti scelte salvate, TRUE per non avere messaggio di errore
             delete(T1frameCoreLines ,T1groupCoreLines) #cancello la lista delle coreline quando ho fatto la selezione
         } else if (get("O.SaveSelection", envir=MyEnv)==FALSE){
             FNameList<-svalue(T1FNameListCK)
             LL<-length(FNameList)
             FNameList<-FNameList[-LL]
             svalue(T1FNameListCK)<-FNameList
             gmessage("Save Spectra Selection before proceeding" , title = "WARNING: SELECTION NOT SAVED",  icon = "error")
         } else {
             assign("O.SaveSelection", FALSE, envir=MyEnv) #selezione precedente salvata, riattivo il controllo
             SelectedNames<-get("O.SelectedNames", envir=MyEnv) #inizialmente O.SelectedNames==(NULL, NULL)
             FName<-svalue(T1FNameListCK)
             if (length(FName)>0) {
                 SelectedNames$XPSSample<-append(SelectedNames$XPSSample, FName) #Carico nomefile in XPSSampleFName
                 SelectedNames$CoreLines<-append(SelectedNames$CoreLines, "-----")  #aggiungo "" per avere identico numero di righe nelle due colonne
             } else {
                 tmp<-get("O.NamesList", envir=MyEnv) #resetto la lista
                 SelectedNames<-tmp
             }
             RefreshTab(SelectedNames)   #aggiorno la tabella con in nomi dei files scelti
             assign("O.SelectedNames", SelectedNames, envir=MyEnv)
             delete(T1frameCoreLines ,T1groupCoreLines) #cancello la lista delle coreline quando ho fatto la selezione
             CoreLineList<-XPSSpectList(svalue(T1FNameListCK))
             T1groupCoreLines <<- ggroup(horizontal=FALSE,container = T1frameCoreLines)
             NCoreLines<-0
             assign("O.NCoreLines", NCoreLines, envir=MyEnv)
             LL<-length(CoreLineList)
             if (LL > 7) {               #se N. Corelines>7 splitto le corelines su due colonne
                 glabel("                                                  ", container=T1groupCoreLines)
                 lyt = glayout(spacing=3, container=T1groupCoreLines)
                 T1CoreLineCK <<- gcheckboxgroup(CoreLineList,checked=FALSE, handler= function(h, ...){
                                  NCoreLines<-get("O.NCoreLines", envir=MyEnv)
                                  NCoreLines<-NCoreLines+1
                                  assign("O.NCoreLines", NCoreLines, envir=MyEnv)
                                  FName<-svalue(T1FNameListCK)
                                  if (length(FName)==0) { #si e' selezionata una coreline senza aver prima selezionato l'XPSSample
                                      gmessage(msg="Please select a XPS-Sample" , title = "NO XPS-SAMPLE!",  icon = "warning")
                                      svalue(T1CoreLineCK)<-NULL
                                      return()
                                  }
                                  NamesList<-get("O.SelectedNames", envir=MyEnv)   #uso una variabile temporanea per poter cambiare scelta nel checkbox
                                  SpectList<-svalue(T1CoreLineCK)
                                  LL<-length(SpectList)
                                  LLL<-length(NamesList$CoreLines)
                                  if (LL == 0) SpectList<-"-----"  #se annullo tutte le selezioni del gcheckboxgroup coreline
                                  if (NamesList$CoreLines[LLL] == "-----" && LL==1) { #ho selezionato l'XPSSampleName e sono alla prima coreline
                                      NamesList$CoreLines[LLL]<-SpectList #sostituisco ----- con NomeCoreline
                                  } else {
                                      NamesList$CoreLines<-c(NamesList$CoreLines[1:LLL-1], SpectList) #aggiungo la corelinie selezionata alla lista
                                  }
                                  if (LL>0) NamesList$XPSSample<-c(NamesList$XPSSample, rep(FName, LL-1))  #aggiungo tanti XPSFName alla lista XPSSample quante corelines vengono selezionate
                                  RefreshTab(NamesList)   #aggiorno la tabella con in nomi dei files scelti
                                  assign("O.NamesList", NamesList, envir=MyEnv)
                 }, container=T1groupCoreLines)

                 for(ii in 1:LL){
                     tkpack.forget(T1CoreLineCK$widgets[[ii]]$button)  # unparent widgets (uses library call)
                 }
                     for (ii in 1:7) {lyt[ii,1] = T1CoreLineCK$widgets[[ii]]$button}   #ricostruisco il ceckbox su due colonne secondo il MIO layout lyt
                     for (ii in 7:LL) {lyt[ii-6,2] = T1CoreLineCK$widgets[[ii]]$button}
                 } else {
                     glabel("                                                  ", container=T1groupCoreLines)
                     T1CoreLineCK <<- gcheckboxgroup(CoreLineList,checked=FALSE, handler= function(h, ...){
                                     NCoreLines<-get("O.NCoreLines", envir=MyEnv)
                                     NCoreLines<-NCoreLines+1
                                     assign("O.NCoreLines", NCoreLines, envir=MyEnv)
                                     FName<-svalue(T1FNameListCK)
                                     if (length(FName)==0) { #si e' selezionata una coreline senza aver prima selezionato l'XPSSample
                                         gmessage(msg="Please select a XPS-Sample" , title = "NO XPS-SAMPLE!",  icon = "warning")
                                         svalue(T1CoreLineCK)<-NULL
                                         return()
                                     }
                                     NamesList<-get("O.SelectedNames", envir=MyEnv)   #uso una variabile temporanea per poter cambiare scelta nel checkbox
                                     SpectList<-svalue(T1CoreLineCK)
                                     LL<-length(SpectList)
                                     LLL<-length(NamesList$CoreLines)
                                     if (LL == 0) SpectList<-"-----"  #se annullo tutte le selezioni del gcheckboxgroup coreline
                                     if (NamesList$CoreLines[LLL] == "-----" && LL==1) { #ho selezionato l'XPSSampleName e sono alla prima coreline
                                        NamesList$CoreLines[LLL]<-SpectList #sostituisco ----- con NomeCoreline
                                     } else {
                                        NamesList$CoreLines<-c(NamesList$CoreLines[1:LLL-1], SpectList) #aggiungo la corelinie selezionata alla lista
                                     }
                                     if (LL>0) NamesList$XPSSample<-c(NamesList$XPSSample, rep(FName, LL-1))  #aggiungo tanti XPSFName alla lista XPSSample quante corelines vengono selezionate
                                     RefreshTab(NamesList)   #aggiorno la tabella con in nomi dei files scelti
                                     assign("O.NamesList", NamesList, envir=MyEnv)
                     }, container=T1groupCoreLines)
                 }
                 SaveButton<<-gbutton("SAVE SELECTION", handler=function(h,...){
                                     NCoreLines<-get("O.NCoreLines", envir=MyEnv)
                                     FName<-svalue(T1FNameListCK)
                                     SpectList<-svalue(T1CoreLineCK)
                                     if (length(FName)==0 || length(SpectList)==0){
                                          gmessage(msg="No XPS-Sample or CoreLine selected!" , title = "WARNING:",  icon = "error")
                                          return()
                                     }
                                     assign("O.SaveSelection", TRUE, envir=MyEnv)
                                     svalue(T1FNameListCK)<-NULL
                                     svalue(T1CoreLineCK)<-NULL
                                     SelectedNames<-get("O.NamesList", envir=MyEnv)
                                     assign("O.SelectedNames", SelectedNames, envir=MyEnv)
                                     SpectList<-unlist(strsplit(SelectedNames$CoreLines[1], "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                                     assign("activeFName", SelectedNames[[1]][1], envir=.GlobalEnv)    #forzo i parametri dell'activeXPSSample
                                     assign("activeSpectName", SpectList[2], envir=.GlobalEnv)     #a coincidere con quelli del primo XPSsample selezionato
                                     assign("activeSpectIndx", as.numeric(SpectList[1]), envir=.GlobalEnv)

                 }, container=T1groupCoreLines)
             }
   }

#----- reset parametri ai valori iniziali -----
   ResetPlot <- function(){
            svalue(T1FNameListCK) <<- FALSE
            svalue(T1OvTypeCK1) <<- 1
            svalue(T1OvTypeCK2) <<- 1
            svalue(T1LimitRTF) <<- FALSE
            svalue(objFunctNorm) <<- FALSE
            svalue(objFunctAlign) <<- FALSE
            svalue(objFunctRev) <<- TRUE
            svalue(objFunctSwitch) <<- FALSE
            svalue(objFunctAmpli) <<- -1
            svalue(objFunctFact) <<- ""
            enabled(objFunctFact) <<- FALSE
            svalue(XOffsetobj) <<- 0
            svalue(YOffsetobj) <<- 0
            svalue(objFunctPseudo3D) <<- FALSE
            svalue(objFunctTreD) <<- FALSE
            svalue(objTreDAspect) <<- "1/1"
            svalue(T2AzymutRot) <<- 35
            svalue(T2ZenithRot) <<- 15
            svalue(x1) <<- ""
            svalue(x2) <<- ""
            svalue(y1) <<- ""
            svalue(y2) <<- ""
            svalue(T3_BW_Col) <<- "B/W"
            svalue(T3_Grid) <<- "Grid OFF"
            svalue(T3_SetLines) <<- 1
            svalue(T3_SetSymbols) <<- 2
            svalue(T3_LineType) <<- "patterns"
            svalue(T3_LinWidth) <<- 1
            svalue(T3_SymType) <<- "single-symbol"
            svalue(T3_SymSize) <<- 0.8
            svalue(T3_FitCompStyle) <<- "dotted"
            svalue(T3_PanStripCol) <<- ""
            svalue(T4_LBTR) <<- "LeftBottom"
            svalue(T4_XScale) <<- "Regular"
            svalue(T4_YScale) <<- "Regular"
            svalue(T4_TitSize) <<- 1.4
            svalue(T4_MainTitChange) <<- ""
            svalue(T4_AxNumSize) <<- 1
            svalue(T4_AxLabSize) <<- 1
            svalue(T4_XAxNameChange) <<- ""
            svalue(T4_YAxNameChange) <<- ""
            svalue(T4_ZAxNameChange) <<- ""
            svalue(T4_XStep) <<- ""
            svalue(T4_YStep) <<- ""
            svalue(legendCK) <<- FALSE
            svalue(LegPosCK) <<- -1
            svalue(LegColCK) <<- 1
            svalue(TSizeCK) <<- 1
            svalue(DistCK) <<- 0.1
            svalue(LineWdhCK) <<- 1
            svalue(TxtColCK) <<- "B/W"

            XPSSettings<<-get("XPSSettings", envir=.GlobalEnv)
            Colors<<-XPSSettings$Colors
            LType<<-XPSSettings$LType
            SType<<-XPSSettings$Symbols
            STypeIndx<<-XPSSettings$SymIndx
            FitColors<<-c(XPSSettings$BaseColor[1], XPSSettings$ComponentsColor[1], XPSSettings$FitColor[1])
            CLPalette<<-data.frame(Colors=Colors, stringsAsFactors=FALSE)
            FitPalette<<-data.frame(FitColors=FitColors, stringsAsFactors=FALSE)

            PlotParameters<<-DefaultPlotParameters

            Plot_Args<<-list( x=formula("y ~ x"), data=NULL, PanelTitles=list(), groups=NULL,layout=NULL,
                                  xlim=NULL,ylim=NULL,
                                  pch=STypeIndx,cex=1,lty=LType,lwd=1,type="l",
                                  background="transparent",  col="black",
                                  main=list(label=NULL,cex=1.5),
                                  xlab=list(label=NULL, rot=0, cex=1.2),
                                  ylab=list(label=NULL, rot=90, cex=1.2),
                                  zlab=NULL,
                                  scales=list(cex=1, tck=c(1,0), alternating=c(1), relation="same",
                                              x=list(log=FALSE), y=list(log=FALSE), axs="i"),
                                  xscale.components = xscale.components.subticks,
                                  yscale.components = yscale.components.subticks,
                                  par.settings = list(superpose.symbol=list(pch=STypeIndx, fill="black"), #setta colore riempimento simboli
                                        superpose.line=list(lty=LType, col="black"), #necessario per settare colori legende
                                        par.strip.text=list(cex=1),
                                        strip.background=list(col="grey90") ),
                                  auto.key = FALSE,
                                  grid = FALSE
                             )

            AutoKey_Args <<- list(space="top",
                                  text=get("activeSpectName", envir=.GlobalEnv),
                                  cex = 1,
                                  type= "l",
                                  lines=TRUE,
                                  points=FALSE,
                                  col="black",
                                  columns=1,  #leggende incolonnate in verticale
                                  list(corner=NULL,x=NULL,y=NULL)
                             )
   }


#----- Variabili -----
   FName<-get(activeFName, envir=.GlobalEnv)   #carico l'XPSSample dataFrame attivo in FName variabile globale
   ActiveFName<-get("activeFName", envir=.GlobalEnv)  #carico il nome XPSSample (stringa)
   SpectIndx<-get("activeSpectIndx", envir=.GlobalEnv)#indice della CoreLine attiva
   SpectList<-XPSSpectList(ActiveFName)   #sequenze delle CoreLines XPSSample attivo
   NComp=length(FName[[SpectIndx]]@Components)
   NCorelines<-NULL
   FitComp1<-""  #creo vettore contenente i nomi delle componenti del fit fatto sullo spettro attivo
   for (ii in 1:NComp){
      FitComp1[ii]<-paste("C",ii, sep="")
   }
   # LISTE DEI NOMI ALTRI SPETTRI
   FNameListTot<-as.array(XPSFNameList())     #lista di tutti gli XPSSample caricati nel WorkSpace
   LL=length(FNameListTot)
   jj<-1
   SelectedNames<-list(XPSSample=NULL, CoreLines=NULL, Ampli=NULL)
   NamesList=list(XPSSample=NULL, CoreLines=NULL)
   plot.new()                                 #reset fineStra grafica



#----- salvo variabili nel MyEnv -----
   rm(list = ls(envir = MyEnv, all.names = TRUE), envir = MyEnv)

   SpectName<-""

   # LISTE VARIABILI GRAFICHE
   PatternList<-NULL
   FontSize<-c(0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)

   XPSSettings<-get("XPSSettings", envir=.GlobalEnv)
   Colors<-XPSSettings$Colors
   LType<-XPSSettings$LType
   SType<-XPSSettings$Symbols
   STypeIndx<-XPSSettings$SymIndx
   FitColors<-c(XPSSettings$BaseColor[1], XPSSettings$ComponentsColor[1], XPSSettings$FitColor[1])
   CLPalette<-data.frame(Colors=Colors, stringsAsFactors=FALSE)
   VarNames<-c("BasLnCol", "CompCol", "FitCol")
   FitPalette<-data.frame(VarNames=VarNames, FitColors=FitColors, stringsAsFactors=FALSE)
#-------------------------------------------------------------------------------------------------
#   LType<-c("solid", "dashed", "dotted", "dotdash", "longdash",     #definisco 20 tipi divesi di line pattern
#            "twodash", "F8", "431313", "22848222", "12126262",
#            "12121262", "12626262", "52721272", "B454B222", "F313F313",
#            "71717313", "93213321", "66116611", "23111111", "222222A2" )
#
#   SType<-c("VoidCircle", "VoidSquare", "VoidTriangleUp", "VoidTriangleDwn",  "Diamond",
#            "X", "Star", "CrossSquare", "CrossCircle", "CrossDiamond",
#            "SolidSquare", "SolidCircle", "SolidTriangleUp", "SolidTriangleDwn", "SolidDiamond",
#            "DavidStar", "SquareCross", "SquareTriang", "CircleCross", "Cross")
#   STypeIndx<-c(1,  0,  2,  6,  5,
#                4,  8,  7,  10, 9,
#                15, 16, 17, 25, 18,
#                11, 12, 14, 13, 3)
#
#   Colors<-c("black", "red", "limegreen", "blue", "magenta", "orange", "cadetblue", "sienna",
#             "darkgrey", "forestgreen", "gold", "darkviolet", "greenyellow", "cyan", "lightblue",
#             "turquoise", "deeppink3", "wheat", "thistle", "grey40")
#-------------------------------------------------------------------------------------------------
   LWidth<-c(1,1.25,1.5,1.75,2,2.25,2.5,3, 3.5,4)
   SymSize<-c(0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2) #lattice prende indici simboli piuttosto che nomesimbolo
   PanelTitles<-NULL
   LegPos<-c("OutsideTop","OutsideRight","OutsideLeft", "OutsideBottom",
             "InsideTopRight","InsideTopLeft","InsideBottomRight","InsideBottomLeft")
   LegOrient<-c("Vertical", "Horizontal")
   LegLineWdh<-c(1,1.5,2,2.5,3,3.5,4,4.5,5)
   LegTxtCol<-c("RainBow", "Black")
   LegTxtSize<-c(0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
   LegDist<-c(0,0.01,0.02,0.04,0.08,0.1,0.12,0.14,0.16,0.18,0.2)
   ColorList<-NULL
   exit<-NULL

#--- controlli opzioni menu
   PlotParameters<-list()
   PlotParameters$Aligne<-FALSE
   PlotParameters$RTFLtd<-FALSE #restrict plot to RTF region
   PlotParameters$Normalize<-FALSE
   PlotParameters$Reverse<-TRUE #assi rovesciati per Bind. Energy
   PlotParameters$SwitchE<-FALSE
   PlotParameters$XOffset<-0
   PlotParameters$YOffset<-0
   PlotParameters$OverlayType<-"Spectrum"
   PlotParameters$OverlayMode<-"Single-Panel"
   PlotParameters$Colors<-"B/W"
   PlotParameters$CompLty<-"dotted"
   PlotParameters$FitCol<-FitColors
#--- controllo opzioni menu legenda
   PlotParameters$Labels<-NULL
   PlotParameters$Legenda<-FALSE
   PlotParameters$LegPos<-"topright"
   PlotParameters$LegLineWdh<-1
   PlotParameters$LegTxtCol<-"RainBow"
   PlotParameters$LegTxtSize<-1
   PlotParameters$LegDist<-0
#--- controllo opzioni 3D
   PlotParameters$Pseudo3D<-FALSE
   PlotParameters$TreD<-FALSE
   PlotParameters$TreDAspect<-c(1,1)
   PlotParameters$AzymuthRot<- 35
   PlotParameters$ZenithRot<- 15

   DefaultPlotParameters<-PlotParameters

#--- comandi diretti a lattice
   Plot_Args<-list( x=formula("y ~ x"), data=NULL, PanelTitles=list(), groups=NULL,layout=NULL,
                    xlim=NULL, ylim=NULL,
                    pch=STypeIndx,cex=1,lty=LType,lwd=1,type="l",
                    background="transparent", col="black",
                    main=list(label=NULL,cex=1.5),
                    xlab=list(label=NULL, rot=0, cex=1.2),
                    ylab=list(label=NULL, rot=90, cex=1.2),
                    zlab=NULL,
                    scales=list(cex=1, tck=c(1,0), alternating=c(1), tick.number=5, relation="same",
                                x=list(log=FALSE), y=list(log=FALSE), axs="i"),
                    xscale.components = xscale.components.subticks,
                    yscale.components = yscale.components.subticks,
                    par.settings = list(superpose.symbol=list(pch=STypeIndx,fill="black"), #setta colore riempimento simboli
                                        superpose.line=list(lty=LType, col="black"), #necessario per settare colori legende
                                        par.strip.text=list(cex=1),
                                        strip.background=list(col="grey90") ),
                    auto.key = FALSE,
                    grid = FALSE
                  )


   AutoKey_Args <- list( space="top",
                         text=get("activeSpectName", envir=.GlobalEnv),
                         cex = 1,
                         type= "l",
                         lines=TRUE,
                         points=FALSE,
                         col="black",
                         columns=1,  #leggende incolonnate in verticale
                         list(corner=NULL,x=NULL,y=NULL)
                       )

   assign("O.SelectedNames", SelectedNames, envir=MyEnv)
   assign("O.NamesList",NamesList , envir=MyEnv)
   assign("O.SaveSelection", TRUE, envir=MyEnv) #all'inizio forzo il controllo della selezione a TRUE in modo da evitae messaggi di errore



#===== Reset della finestra Grafica =====

   plot.new()
   assign("MatPlotMode", FALSE, envir=.GlobalEnv)  #basic matplot function used to plot data
#   assign("LatticePlotMode", TRUE, envir=.GlobalEnv)  #Advanced Lattice graphics off


#===== NoteBook =====

   win <- gwindow(" OVERLAY SPECTRA ", visible=FALSE)
   size(win)<- c(400,400)
   maingroup<-ggroup(horizontal=FALSE, container=win)
   nb <- gnotebook(expand=TRUE, container = maingroup)

# --- TAB1 ---
#XPS Sample selecion & representation options

     T1group1 <- ggroup(label="XPS SAMPLE SELECTION", spacing=5, horizontal=FALSE, container=nb)

#FRAME1: PIU XPS Samples PIU corelines
     layoutT1 <- glayout(homogeneous=FALSE, spacing=5, container=T1group1)

     layoutT1[1,1] <- T1frameOvType <- gframe(text="SELECT PLOT OPTIONS", horizontal=FALSE, spacing=5, container=layoutT1)
     T1groupOpt <- ggroup(horizontal=TRUE, spacing=5, container = T1frameOvType)
     T1OvTypeCK1 <- gradio(c("Spectrum", "Spectrum+Baseline", "Spectrum+Fit"),selected=1, handler= function(h, ...){
                            PlotParameters$OverlayType<<-svalue(T1OvTypeCK1)
                            CtrlPlot()
                   }, container=T1groupOpt)
     T1OvTypeCK2 <- gradio(c("Single-Panel", "Multi-Panel"),selected=1, handler= function(h, ...){
                            PlotParameters$OverlayMode<<-svalue(T1OvTypeCK2)
                            if (svalue(T1OvTypeCK2)=="Single-Panel") {
                               enabled(T4_PanelTitles)<-FALSE
                               Plot_Args$scales$relation<<-"same"
                            }
                            if (svalue(T1OvTypeCK2)=="Multi-Panel") {
                               enabled(T4_PanelTitles)<-TRUE
                               Plot_Args$scales$relation<<-"free"
                               SelectedNames<-get("O.SelectedNames", envir=MyEnv)
                               LL<-length(SelectedNames$XPSSample)
                               PanelTitles<<-NULL
                               for (ii in 1:LL){
                                   SpectName<-unlist(strsplit(SelectedNames$CoreLines[ii], "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                                   SpectName<-SpectName[2]
                                   PanelTitles<<-c(PanelTitles, paste(SpectName, SelectedNames$XPSSample[ii], sep=" ")) #List of Titles for the Multipanel
                               }
                               Plot_Args$PanelTitles<<-PanelTitles
                               assign("O.PanelTitles", PanelTitles, envir=MyEnv)
                            }
                            CtrlPlot()
                   }, container=T1groupOpt)

     T1groupLim <- ggroup(horizontal=TRUE, spacing=7, container = T1frameOvType)
     T1LimitRTF <- gcheckbox("Limit Plot To Fit Region",checked=FALSE, handler=function(h,...){
                            PlotParameters$RTFLtd<<-svalue(T1LimitRTF)
                            CtrlPlot() ####PLOT FOLLOWING SELECTIONS
                   }, container=T1groupLim)

     layoutT1[1,2] <- T1frameFName <- gframe(text="SELECT XPS-SAMPLE", spacing=5, container=layoutT1)
     layoutT1[1,3] <- T1frameCoreLines <- gframe(text="SELECT CORE LINE",  spacing=5, container=layoutT1)
     T1groupCoreLines <- ggroup(horizontal=FALSE,container = T1frameCoreLines)
     glabel("                                                  ", container=T1groupCoreLines)
     T1CoreLineCK <- gcheckboxgroup(NULL,checked=FALSE, handler=NULL, container=T1groupCoreLines)

     T1FNameListCK <- gcheckboxgroup(FNameListTot,checked=FALSE, handler=function(h,...){
                            setFileCheckBox()
     }, container=T1frameFName)

     T1groupButtons<-ggroup(horizontal=TRUE,container = T1group1)
     gbutton("PLOT", handler=function(h,...){
                           if (get("O.SaveSelection", envir=MyEnv)==FALSE){
                              gmessage("Save Spectra Selection before plotting" , title = "WARNING: SELECTION NOT SAVED",  icon = "error")
                              return()
                           } else {
                              SelectedNames<-get("O.SelectedNames", envir=MyEnv)
                              LL=length(SelectedNames$XPSSample)
                              SelectedNames$Ampli<-rep(1, LL)
                              assign("O.SelectedNames", SelectedNames, envir=MyEnv)
                              delete(layoutAmpli,objFunctAmpli)
                              layoutAmpli[1,2] <<- objFunctAmpli <<- gcombobox(SelectedNames$XPSSample, selected=-1, editable=FALSE, handler=function(h,...){enabled(objFunctFact)<-TRUE}, container=layoutAmpli)  #devo inserire i nomi degli spettri selezionati
                              CtrlPlot() ####PLOT FOLLOWING SELECTIONS
                           }
     }, container=T1groupButtons)

     gbutton("CLEAR LAST XPS-SAMPLE", handler=function(h,...){
                           NCoreLines<-get("O.NCoreLines", envir=MyEnv)
                           NamesList<-get("O.NamesList", envir=MyEnv)
                           LL<-length(NamesList$XPSSample) #NamesList$XPSSample e NamesList$CoreLines hanno stessa lunghezza
                           if (NCoreLines==LL) {
                              NamesList<-list(XPSSample="   ", CoreLines="   ")  #liste dummy per iniziare: NB ogni colonna ha due elementi altrimenti errore....
                           } else {
                              NamesList<-list(XPSSample=NamesList[[1]][1:(LL-NCoreLines)], CoreLines=NamesList[[2]][1:(LL-NCoreLines)])
                           }
                           FNameListTot<-as.array(XPSFNameList())   #lista di tutti gli XPSSample caricati nel WorkSpace
                           SpectList<-svalue(T1CoreLineCK)
                           LL<-length(SpectList)
                           SpectList<-SpectList[-LL]
                           svalue(T1CoreLineCK)<-SpectList
                           RefreshTab(NamesList)   #aggiorno la tabella con in nomi dei files scelti
                           assign("O.NamesList", NamesList, envir=MyEnv)
                           assign("O.SelectedNames", NamesList, envir=MyEnv)
                           LL=length(SelectedNames$XPSSample)
                           SelectedNames$Ampli<-rep(1, LL)
                           delete(layoutAmpli,objFunctAmpli)
                           layoutAmpli[1,2] <<- objFunctAmpli <<- gcombobox(SelectedNames$XPSSample, selected=-1, editable=FALSE, handler=function(h,...){enabled(objFunctFact)<-TRUE}, container=layoutAmpli)
     }, container=T1groupButtons)

     gbutton("RESET LIST", handler=function(h,...){
                           svalue(T1FNameListCK) <<- NULL
                           delete(T1groupCoreLines, T1CoreLineCK)
                           delete(T1groupCoreLines, SaveButton)
                           T1CoreLineCK <<- gcheckboxgroup(NULL,checked=FALSE, handler=NULL, container=T1groupCoreLines)
                           add(T1groupCoreLines, T1CoreLineCK)
                           NamesList<-list(XPSSample=NULL, CoreLines=NULL)
                           SelectedNames<-list(XPSSample=c(" ", " "),CoreLines=c(" "," "))   #liste dummy per iniziare: NB ogni colonna ha due elementi altrimenti errore....
                           RefreshTab(SelectedNames)   #aggiorno la tabella con in nomi dei files scelti
                           SelectedNames<-list(XPSSample=NULL,CoreLines=NULL,Ampli=NULL )   #liste dummy per iniziare: NB ogni colonna ha due elementi altrimenti errore....
                           delete(layoutAmpli, layoutAmpli[1,2])
                           layoutAmpli[1,2] <<- objFunctAmpli <<- gcombobox(c("        "), selected=1, editable=FALSE, handler=function(h,...){enabled(objFunctFact)<-TRUE}, container=layoutAmpli)
                           assign("O.NamesList", NamesList, envir=MyEnv)
                           assign("O.SelectedNames", SelectedNames, envir=MyEnv)
                           assign("O.SaveSelection", TRUE, envir=MyEnv)
                           plot.new()
     }, container=T1groupButtons)

     gbutton("RESET PLOT", handler=function(h,...){
                           svalue(T1FNameListCK) <<- NULL
                           delete(T1groupCoreLines, T1CoreLineCK)
                           delete(T1groupCoreLines, SaveButton)
                           T1CoreLineCK <<- gcheckboxgroup(NULL,checked=FALSE, handler=NULL, container=T1groupCoreLines)
                           add(T1groupCoreLines, T1CoreLineCK)
                           NamesList<-list(XPSSample=NULL, CoreLines=NULL)
                           SelectedNames<-list(XPSSample=c(" ", " "),CoreLines=c(" "," "))   #liste dummy per iniziare: NB ogni colonna ha due elementi altrimenti errore....
                           RefreshTab(SelectedNames)   #aggiorno la tabella con in nomi dei files scelti
                           SelectedNames<-list(XPSSample=NULL,CoreLines=NULL,Ampli=NULL )   #liste dummy per iniziare: NB ogni colonna ha due elementi altrimenti errore....
                           delete(layoutAmpli, layoutAmpli[1,2])
                           layoutAmpli[1,2] <<- objFunctAmpli <<- gcombobox(c("        "), selected=1, editable=FALSE, handler=function(h,...){enabled(objFunctFact)<-TRUE}, container=layoutAmpli)
                           assign("O.NamesList", NamesList, envir=MyEnv)
                           assign("O.SelectedNames", SelectedNames, envir=MyEnv)
                           assign("O.SaveSelection", TRUE, envir=MyEnv)
                           ResetPlot()
                           plot.new()
     }, container=T1groupButtons)

     gbutton("UPDATE XPS-SAMPLE LIST", handler=function(h,...){
                           FName<<-get(activeFName, envir=.GlobalEnv)   #carico l'XPSSample dataFrame attivo in FName variabile globale
                           ActiveFName<<-get("activeFName", envir=.GlobalEnv)  #carico il nome XPSSample (stringa)
                           SpectIndx<<-get("activeSpectIndx", envir=.GlobalEnv)#indice della CoreLine attiva
                           SpectList<<-XPSSpectList(ActiveFName)   #sequenze delle CoreLines XPSSample attivo
                           NComp<<-length(FName[[SpectIndx]]@Components)
                           NCorelines<<-NULL
                           FitComp1<<-""  #build vector containing names of the fit components on the Active Spectrum
                           for (ii in 1:NComp){
                               FitComp1[ii]<-paste("C",ii, sep="")
                           }
                           # LISTE DEI NOMI ALTRI SPETTRI
                           FNameListTot<-as.array(XPSFNameList())     #list of all XPSSample in Envir=.GlobalEnv
                           LL=length(FNameListTot)
                           jj<-1
                           SelectedNames<-list(XPSSample=NULL, CoreLines=NULL, Ampli=NULL)
                           NamesList=list(XPSSample=NULL, CoreLines=NULL)
                           assign("O.NamesList", NamesList, envir=MyEnv)
                           assign("O.SelectedNames", SelectedNames, envir=MyEnv)
                           assign("O.SaveSelection", TRUE, envir=MyEnv)

                           delete(T1frameFName, T1FNameListCK)       #update panel
                           T1FNameListCK <<- gcheckboxgroup(FNameListTot,checked=FALSE, handler=function(h,...){
                                               setFileCheckBox()
                           }, container=T1frameFName)

                           ResetPlot()
                           plot.new()
     }, container=T1groupButtons)

     dummy<-list(XPSSample=c("   ", "  "),CoreLines=c("   ", "  "))   #liste dummy per iniziare: NB ogni colonna ha due elementi altrimenti errore....
     dummy$XPSSample<-encodeString(dummy$XPSSample, width=40, justify="right")
     dummy$CoreLines<-encodeString(dummy$CoreLines, width=40, justify="right")
     NameTable<<-gtable(dummy, expand=TRUE, fill=TRUE, container=T1group1) #tabella con la lista dei files selezionati


# --- TAB2 ---

###Funct1: NORMALIZE

   T2group1 <- ggroup(label="FUNCTIONS",horizontal=FALSE, container=nb)

   T2frame2 <- gframe(" FUNCTIONS ", horizontal=FALSE, spacing=5, container=T2group1)
   T2group2 <- ggroup(label="FUNCTIONS", horizontal=TRUE, container=T2frame2)

   objFunctNorm <- gcheckbox("Normalize",checked=FALSE, handler=function(h,...){
                    PlotParameters$Normalize<<-svalue(objFunctNorm)
                    CtrlPlot() ####PLOT FOLLOWING SELECTIONS
                 }, container=T2group2)

###Funct2: Y-Aligne

   objFunctAlign <- gcheckbox("Aligne bkg to 0",checked=FALSE, handler=function(h,...){
                    PlotParameters$Aligne<<-svalue(objFunctAlign)
                    CtrlPlot() ####PLOT FOLLOWING SELECTIONS
                 }, container=T2group2)

###Funct3: Reverse X axis

   objFunctRev<- gcheckbox("Reverse X axis",checked=TRUE, handler=function(h,...){
                    PlotParameters$Reverse<<-svalue(objFunctRev)
                    CtrlPlot() ####PLOT FOLLOWING SELECTIONS
                 }, container=T2group2)

###Funct4: Switch Binding to Kinetic Energy scale

   objFunctSwitch <- gcheckbox("Switch BE/KE energy scale",checked=FALSE, handler=function(h,...){
                    PlotParameters$SwitchE<<-svalue(objFunctSwitch)
                    CtrlPlot() ####PLOT FOLLOWING SELECTIONS
                 }, container=T2group2)

###Funct5: Amplify

   layoutAmpli <- glayout(homogeneous=FALSE, spacing=1, container=T2frame2)
   layoutAmpli[1,1] <- glabel("XPSSamp.", spacing=1, container=layoutAmpli)
   layoutAmpli[1,2] <- objFunctAmpli <- gcombobox(c("   "), selected=-1, editable=FALSE, handler=function(h,...){
                    enabled(objFunctFact)<-TRUE
                 }, container=layoutAmpli)

   layoutAmpli[1,3] <- glabel("ScaleFact.", spacing=5, container=layoutAmpli)
   layoutAmpli[1,4] <- objFunctFact <- gedit("", handler=function(h,...){
                    indx <- as.numeric(svalue(objFunctAmpli, index=TRUE))
                    SelectedNames<-get("O.SelectedNames", envir=MyEnv)
                    SelectedNames$Ampli[indx]<-as.numeric(svalue(objFunctFact))
                    assign("O.SelectedNames", SelectedNames, envir=MyEnv)
                    CtrlPlot() ####PLOT FOLLOWING SELECTIONS
                 }, container=layoutAmpli)


###Funct6: X, Y offset
   layoutAmpli[2,1] <- glabel("X-Offset", spacing=1, container=layoutAmpli)
   layoutAmpli[2,2] <- XOffsetobj <- gedit("", initial.msg = "X_Off= ", container=layoutAmpli)
   addHandlerChanged(XOffsetobj, handler=function(h,...){
                        PlotParameters$XOffset<<-as.numeric(svalue(XOffsetobj))
                        CtrlPlot()
                    })
   layoutAmpli[2,3] <- glabel("Y-Offset", spacing=1, container=layoutAmpli)
   layoutAmpli[2,4] <- YOffsetobj <- gedit("", initial.msg = "Y_Off= ",container=layoutAmpli)
   addHandlerChanged(YOffsetobj, handler=function(h,...){
                        PlotParameters$YOffset<<-as.numeric(svalue(YOffsetobj))
                        CtrlPlot()
                    })
   layoutAmpli[3,1] <- objFunctPseudo3D <- gcheckbox("Pseudo-3D Rendering",checked=FALSE, handler=function(h,...){
                           Pseudo3D<-svalue(objFunctPseudo3D)
                           if (Pseudo3D) {
                              PlotParameters$Pseudo3D<<-TRUE
                           } else {
                              PlotParameters$Pseudo3D<<-FALSE
                           }
                           CtrlPlot()
                    }, container=layoutAmpli)



###Funct7: 3D

   T2frame3 <- gframe(" 3D plot ", horizontal=FALSE,spacing=5, container=T2group1)
   layoutTreD <- glayout(homogeneous=FALSE, spacing=7, container=T2frame3)

   layoutTreD[1,1] <- objFunctTreD <- gcheckbox("3D",checked=FALSE, handler=function(h,...){
                    PlotParameters$TreD<<-svalue(objFunctTreD)
                    OvType<-svalue(T1OvTypeCK1)
                    if (OvType != "Spectrum") {
                       gmessage("3D plot OK only for SPECTRUM type plot" , title = "WARNING: WRONG TYPE PLOT",  icon = "warning")
                    } else {
                       if (PlotParameters$TreD) {
                           PlotParameters$OverlayMode<<-"TreD"
                           enabled(T4_ZAxNameChange)<-TRUE
                       } else {
                           PlotParameters$OverlayMode<<-svalue(T1OvTypeCK2)
                           Plot_Args$zlab$label<<-NULL
                           Plot_Args$ylab$label<<-NULL
                           enabled(T4_ZAxNameChange)<-FALSE
                       }
                       CtrlPlot()
                    }
                 }, container=layoutTreD)

   layoutTreD[1,2] <- AspGroup<-ggroup(horizontal=TRUE, spacing=1, container = layoutTreD)
   glabel("X|Y aspect ratio.", container=AspGroup)
   objTreDAspect <- gcombobox(c( "3|1", "2|1","1|1", "1|2", "1|3"), selected=3,spacing=1,editable=FALSE, handler=function(h,...){
                    indx<-svalue(objTreDAspect, index=TRUE)
                    aspect<-matrix(c(0.3,0.5,1,2,3, 1,1,1,1,1), nrow=5) # "|" sostituito da ","  come vuole aspect
                    PlotParameters$TreDAspect<<-as.vector(aspect[indx,])
                    CtrlPlot()
                }, container=AspGroup)


   layoutTreD[2,1] <- AzyGroup<-ggroup(horizontal=TRUE, spacing=1, container = layoutTreD)
   glabel("Azymuth rotation:", container=AzyGroup)
   T2AzymutRot<-gslider(from = 0, to = 90, by = 5, value = 35, handler=function(h,...){
                    PlotParameters$AzymuthRot<<-svalue(T2AzymutRot)
                    CtrlPlot() ###Plot following the selections
                 }, container=AzyGroup)

   layoutTreD[2,2] <- ZenGroup<-ggroup(horizontal=TRUE, spacing=1, container = layoutTreD)
   glabel("Zenith rotation:", container=ZenGroup)
   T2ZenithRot<-gslider(from = 0, to = 90, by = 5, value = 15, handler=function(h,...){
                    PlotParameters$ZenithRot<<-svalue(T2ZenithRot)
                    CtrlPlot() ###Plot following the selections
                 }, container=ZenGroup)

###Funct8: Zoom
   T2frame4 <-gframe(text=" ZOOM & CURSOR POSITION ", horizontal=FALSE, spacing=5, container=T2group1)
   glabel(text="Set zoom area corners with SX mouse button ", container=T2frame4)
   T2group5 <- ggroup(label="FUNCTIONS", horizontal=TRUE, container=T2frame4)
   gbutton(" Set Zoom Limits ", handler=function(h,...){
                    if (PlotParameters$OverlayMode=="Multi-Panel") {
                       gmessage("ZOOM option" , title = "WARNING: ZOOM OPTION NOT AVAILABLE IN MULTI-PANEL MODE",  icon = "warning")
                    } else {
                       FName<-get(activeFName, envir=.GlobalEnv)   #carico l'XPSSample dataFrame attivo in FName variabile globale
                       ActiveFName<-get("activeFName", envir=.GlobalEnv)  #carico il nome XPSSample (stringa)
                       SpectIndx<-get("activeSpectIndx", envir=.GlobalEnv)#indice della CoreLine attiva

                       trellis.focus("panel", 1, 1, highlight=FALSE)
                       pos<-list(x=0, y=0)   #valori iniziali per attivare while loop
                       pos1<-list(x=NULL, y=NULL)
                       pos2<-list(x=NULL, y=NULL)

                       Xlim<-get("O.Xlim", envir=MyEnv)  #recupera il range X da OverlayEngine
                       Ylim<-get("O.Ylim", envir=MyEnv)  #recupera il range Y da OverlayEngine
                       X1<-min(Xlim)
                       X2<-max(Xlim)
                       RangeX<-Xlim[2]-Xlim[1]
                       Y1<-min(Ylim)
                       RangeY<-Ylim[2]-Ylim[1]
                       width <- max(convertX(unit(Xlim, "native"), "points", TRUE))
                       height <- max(convertY(unit(Ylim, "native"), "points", TRUE))
                       RevAx<-svalue(objFunctRev)   #the X axis reversed?

                       #First zoom area corner
                       pos <- grid.locator(unit = "points")
                       if (is.null(pos)) break ## non-left click

                       if (FName[[SpectIndx]]@Flags[1] && RevAx==TRUE) { #Binding energy set
                          pos1$x<- X2-as.numeric(pos$x)*RangeX/width   #reversed scale
                       }
                       else if (! FName[[SpectIndx]]@Flags[1] && RevAx==FALSE) { #Kinetic energy scale
                          pos1$x<- X1+as.numeric(pos$x)*RangeX/width           #not reversed scale
                       }
                       if (FName[[SpectIndx]]@Flags[1] && RevAx==FALSE) { #Binding energy set
                          pos1$x<- X1 + as.numeric(pos$x)*RangeX/width       #not reversed scale
                       }
                       else if (! FName[[SpectIndx]]@Flags[1] && RevAx==TRUE) {#Kinetic energy scale
                          pos1$x<- X2-as.numeric(pos$x)*RangeX/width         #reversed scale
                       }
                       pos1$y<-as.numeric(pos$y)*RangeY/height+Y1
                       #shows the first marker
                       panel.superpose(x=pos1$x,y=pos1$y,subscripts=c(1,1),groups=1, type="p", pch=3, cex=0.8, lwd=1.8, col="red")

                       #Second zoom area corner
                       pos <- grid.locator(unit = "points")
                       if (is.null(pos)) break ## non-left click

                       if (FName[[SpectIndx]]@Flags[1] && RevAx==TRUE) { #Binding energy set
                          pos2$x<- X2-as.numeric(pos$x)*RangeX/width   #reversed scale
                       }
                       else if (! FName[[SpectIndx]]@Flags[1] && RevAx==FALSE) { #Kinetic energy scale
                          pos2$x<- X1+as.numeric(pos$x)*RangeX/width           #not reversed scale
                       }
                       if (FName[[SpectIndx]]@Flags[1] && RevAx==FALSE) { #Binding energy set
                          pos2$x<- X1 + as.numeric(pos$x)*RangeX/width       #not reversed scale
                       }
                       else if (! FName[[SpectIndx]]@Flags[1] && RevAx==TRUE) {#Kinetic energy scale
                          pos2$x<- X2-as.numeric(pos$x)*RangeX/width         #reversed scale
                       }
                       pos2$y<-as.numeric(pos$y)*RangeY/height+Y1
                       #shows the second marker
                       panel.superpose(x=pos2$x,y=pos2$y,subscripts=c(1,1),groups=1, type="p", pch=3, cex=0.8, lwd=1.8, col="red")

                       #define zoom area with a rectangle
                       pos$x<-c(pos1$x, pos2$x, pos2$x, pos1$x, pos1$x)
                       pos$y<-c(pos1$y, pos1$y, pos2$y, pos2$y, pos1$y)
                       panel.superpose(x=pos$x,y=pos$y,subscripts=c(1,1),groups=1, type="l", lwd=1, col="black")

                       trellis.unfocus()
                       if (FName[[SpectIndx]]@Flags) { #Binding energy set
                          Plot_Args$xlim<<-sort(c(x1, x2), decreasing=TRUE)
                          Plot_Args$ylim<<-sort(c(y1, y2))
                       } else {
                          Plot_Args$xlim<<-sort(c(x1, x2))
                          Plot_Args$ylim<<-sort(c(y1, y2))
                       }

                    }
                 }, container = T2group5)

   gbutton("      OK      ", handler=function(h,...){ CtrlPlot() }, container = T2group5)

   gbutton("  RESET PLOT  ", handler=function(h,...){
                    Plot_Args$xlim<<-NULL    #xlim viene ricalcolato in XPSOverlayEngine
                    Plot_Args$ylim<<-NULL    #ylim viene ricalcolato in XPSOverlayEngine
                    CtrlPlot()
                 }, container = T2group5)

   glabel(text="Exact Range Values:", spacing=1, container=T2frame4)
   T2group6 <- ggroup(horizontal=TRUE, container=T2frame4)
   x1<-gedit("", width=15, initial.msg = "Xmin= ", container=T2group6)
   x2<-gedit("", width=15, initial.msg = "Xmax= ", container=T2group6)
   y1<-gedit("", width=15, initial.msg = "Ymin= ", container=T2group6)
   y2<-gedit("", width=15, initial.msg = "Ymax= ", container=T2group6)
   tkconfigure(x1$widget, width=12)
   tkconfigure(x2$widget, width=12)
   tkconfigure(y1$widget, width=12)
   tkconfigure(y2$widget, width=12)

   gbutton("  OK  ", handler=function(h,...){
                   x1<-as.numeric(svalue(x1))
                   x2<-as.numeric(svalue(x2))
                   y1<-as.numeric(svalue(y1))
                   y2<-as.numeric(svalue(y2))
                   if (FName[[SpectIndx]]@Flags) { #Binding energy set
                       Plot_Args$xlim<<-sort(c(x1, x2), decreasing=TRUE)
                       Plot_Args$ylim<<-sort(c(y1, y2))
                   } else {
                       Plot_Args$xlim<<-sort(c(x1, x2))
                       Plot_Args$ylim<<-sort(c(y1, y2))
                   }
                   CtrlPlot() }, container = T2group6)

   glabel(text="Position SX button  EXIT DX button", spacing=1,container=T2frame4)
   T2group7 <- ggroup(label="FUNCTIONS", horizontal=TRUE, container=T2frame4)
   gbutton("Cursor Position", handler=function(h,...){
                    if (PlotParameters$OverlayMode=="Multi-Panel") {
                       gmessage("CURSOR POSITION option" , title = "WARNING: CURSOR POSITION OPTION NOT AVAILABLE IN MULTI-PANEL MODE",  icon = "warning")
                    } else {
                       pos<-c(1,1) # solamente per entrare nel loop
                       FName<-get(activeFName, envir=.GlobalEnv)   #carico l'XPSSample dataFrame attivo in FName variabile globale
                       ActiveFName<-get("activeFName", envir=.GlobalEnv)  #carico il nome XPSSample (stringa)
                       SpectIndx<-get("activeSpectIndx", envir=.GlobalEnv)#indice della CoreLine attiva

                       trellis.focus("panel", 1, 1, highlight=FALSE)
                       pos<-list(x=0, y=0)   #valori iniziali per attivare while loop
                       if (is.null(Plot_Args$xlim)) {  #non ho fissato xlim per fare lo zoom
                          Xlim<-get("O.Xlim", envir=MyEnv)
                          Ylim<-get("O.Ylim", envir=MyEnv)
                       } else {
                          Xlim<-Plot_Args$xlim
                          Ylim<-Plot_Args$ylim
                       }
                       X1<-min(Xlim)
                       X2<-max(Xlim)
                       RangeX<-abs(Xlim[2]-Xlim[1])
                       Y1<-min(Ylim)
                       RangeY<-Ylim[2]-Ylim[1]

                       width <- max(convertX(unit(Xlim, "native"), "points", TRUE))
                       height <- max(convertY(unit(Ylim, "native"), "points", TRUE))
                       while (! is.null(pos)) {
                          pos <- grid.locator(unit = "points")
                          if (is.null(pos)) break ## non-left click
                          RevAx<-svalue(objFunctRev)
                          if (FName[[SpectIndx]]@Flags[1] && RevAx==TRUE) { #Binding energy set
                             pos$x<- X2-as.numeric(pos$x)*RangeX/width   #reversed scale
                          }
                          else if (! FName[[SpectIndx]]@Flags[1] && RevAx==FALSE) { #Kinetic energy scale
                             pos$x<- X1+as.numeric(pos$x)*RangeX/width           #not reversed scale
                          }
                          else if (FName[[SpectIndx]]@Flags[1] && RevAx==FALSE) { #Binding energy set
                             pos$x<- X1 + as.numeric(pos$x)*RangeX/width       #not reversed scale
                          }
                          else if (! FName[[SpectIndx]]@Flags[1] && RevAx==TRUE) {#Kinetic energy scale
                             pos$x<- X2-as.numeric(pos$x)*RangeX/width         #reversed scale
                          }
                          pos$y<-as.numeric(pos$y)*RangeY/height+Y1
                          pos$x<-round(pos$x,digits=2)
                          pos$y<-round(pos$y,digits=2)
                          txt<-paste("X: ", as.character(pos$x), ", Y: ", as.character(pos$y), sep="")
                          svalue(CursorPos)<-txt
                          tcl("update", "idletasks")  #impone che il txt caricato dallo svalue venga flushato nella glabel
                       }
                       trellis.unfocus()
                     }
                  }, container = T2group7)

   CursorPos<-glabel("Cursor position: ", container = T2group7)

   gbutton(" RESET PLOT ", handler=function(h,...){
                   ResetPlot()
                   CtrlPlot()
               }, container=T2group1)

   gbutton(" EXIT ", handler=function(h,...){
                   rm(list = ls(envir = MyEnv, all.names = TRUE), envir = MyEnv)
				       dispose(win)
                 }, container = T2group1)


# --- TAB3 ---

# Rendering options
   T3group1 <- ggroup(label="RENDERING", horizontal=FALSE, container=nb)
   T3group2 <- ggroup(horizontal=TRUE, container=T3group1)
   T3group3 <- ggroup(horizontal=FALSE, container=T3group2)
   layoutRend <- glayout(homogeneous=FALSE, spacing=3, container=T3group2)

   layoutRend[1,1] <-T3F_BW_Col <- gframe("COLOR", spacing=5, container=layoutRend)
   T3_BW_Col <- gcombobox(c("B/W", "RainBow"), selected=1, editable=FALSE, handler=function(h,...){
                             if(svalue(T3_BW_Col)=="B/W") {
                                svalue(T3_LineType)<<-"patterns"
                                PlotParameters$Colors<<-"black"
                                Plot_Args$lty<<-LType
                                Plot_Args$pch<<-STypeIndx
                                if (length(svalue(T3_LineType))==0) svalue(T3_SymType)<-"multi-symbols"
                                if (length(svalue(T3_SymType))==0) svalue(T3_LineType)<-"patterns"
                                Plot_Args$par.settings$superpose.symbol$col<<-"black"
                                Plot_Args$par.settings$superpose.symbol$pch<<-STypeIndx
                                Plot_Args$par.settings$superpose.line$col<<-"black"
                                Plot_Args$par.settings$superpose.line$lty<<-LType
                                Plot_Args$par.settings$strip.background$col<<-"grey90"
                                AutoKey_Args$col<<-"black"
                             } else {
                                svalue(T3_LineType)<<-"solid"
                                Plot_Args$lty<<-"solid"
                                Plot_Args$pch<<-STypeIndx[1]
                                PlotParameters$Colors<<-Colors
                                Plot_Args$par.settings$superpose.symbol$fill<<-Colors
                                Plot_Args$par.settings$superpose.line$col<<-Colors
                                Plot_Args$par.settings$superpose.line$lty<<-"solid"
                                Plot_Args$par.settings$strip.background$col<<-"lightskyblue1"
                                AutoKey_Args$col<<-Colors
                             }
                             CtrlPlot() }, container=T3F_BW_Col)


   layoutRend[1,2] <-T3F_Grid <- gframe("GRID", spacing=5, container=layoutRend)
   T3_Grid <- gcombobox(c("Grid ON", "Grid OFF"), selected=-1, editable=FALSE, handler=function(h,...){
                             if(svalue(T3_Grid)=="Grid ON") {
                                Plot_Args$grid<<-TRUE
                             } else {
                                Plot_Args$grid<<-FALSE
                             }
                             CtrlPlot() }, container=T3F_Grid)

   layoutRend[2,1] <- T3F_SetLines <- gframe("SET LINES", spacing=5, container=layoutRend)
   T3_SetLines<- gradio(c("ON", "OFF"), selected=1, horizontal = TRUE, handler=function(h,...){
                               SetLinesPoints()
                           }, container=T3F_SetLines)
                            




   layoutRend[2,2] <- T3F_SetSymbols <- gframe("SET SYMBOLS", horizontal=TRUE, spacing=5, container=layoutRend)
   T3_SetSymbols<- gradio(c("ON", "OFF"), selected=2, horizontal=TRUE, handler=function(h,...){
                               SetLinesPoints()
                            }, container=T3F_SetSymbols)




   layoutRend[3,1] <- T3F_SetLines <- gframe("LINE TYPE", spacing=5, container=layoutRend)
   T3_LineType <- gcombobox(c("solid", "patterns"), selected=2, editable=FALSE, handler=function(h,...){
                             Plot_Args$type<<-"l"
                             palette<-svalue(T3_BW_Col)
                             if (svalue(T3_LineType)=="solid") {
                                svalue(T3_BW_Col)<<-"RainBow"
                                Plot_Args$lty<<-"solid"
                                Plot_Args$pch<<-STypeIndx[1]
                                PlotParameters$Colors<<-Colors
                                Plot_Args$par.settings$superpose.symbol$fill<<-Colors
                                Plot_Args$par.settings$superpose.line$col<<-Colors
                                Plot_Args$par.settings$superpose.line$lty<<-"solid"
                                Plot_Args$par.settings$strip.background$col<<-"lightskyblue"
                                AutoKey_Args$col<<-Colors
                             }
                             if (svalue(T3_LineType)=="patterns") {
                                svalue(T3_BW_Col)<<-"B/W"
                                PlotParameters$Colors<<-"black"
                                Plot_Args$lty<<-LType
                                Plot_Args$pch<<-STypeIndx
                                Plot_Args$par.settings$superpose.symbol$col<<-"black"
                                Plot_Args$par.settings$superpose.symbol$pch<<-STypeIndx
                                Plot_Args$par.settings$superpose.line$col<<-"black"
                                Plot_Args$par.settings$superpose.line$lty<<-LType
                                Plot_Args$par.settings$strip.background$col<<-"gray90"
                                AutoKey_Args$col<<-"black"
                             }
                             CtrlPlot()
                           }, container=T3F_SetLines)

   layoutRend[3,2] <- T3F_LinWidth <- gframe("LINE WIDTH", spacing=5, container=layoutRend)
   T3_LinWidth <- gcombobox(LWidth, selected=1, editable=FALSE, handler= function(h,...){
                              Plot_Args$lwd<<-as.numeric(svalue(T3_LinWidth))
                              CtrlPlot()
                           }, container=T3F_LinWidth)


   layoutRend[4,1] <- T3F_SetSymbols <- gframe("SYMBOLS", spacing=5, container=layoutRend)
   T3_SymType <- gcombobox(c("single-symbol", "multi-symbols"), selected=2, editable=FALSE, handler=function(h,...){
                              if (svalue(T3_SymType)=="single-symbol") {
                                 svalue(T3_BW_Col)<<-"RainBow"
                                 Plot_Args$lty<<-"solid"
                                 Plot_Args$pch<<-STypeIndx[1]
                                 PlotParameters$Colors<<-Colors
                                 Plot_Args$par.settings$superpose.symbol$fill<<-Colors
                                 Plot_Args$par.settings$superpose.line$col<<-Colors
                                 Plot_Args$par.settings$superpose.line$lty<<-"solid"
                                 Plot_Args$par.settings$strip.background$col<<-"lightskyblue"
                                 AutoKey_Args$col<<-Colors
                              }
                              if (svalue(T3_SymType)=="multi-symbols") {
                                 svalue(T3_BW_Col)<<-"B/W"
                                 PlotParameters$Colors<<-"black"
                                 Plot_Args$lty<<-LType
                                 Plot_Args$pch<<-STypeIndx
                                 Plot_Args$par.settings$superpose.symbol$col<<-"black"
                                 Plot_Args$par.settings$superpose.symbol$pch<<-STypeIndx
                                 Plot_Args$par.settings$superpose.line$col<<-"black"
                                 Plot_Args$par.settings$superpose.line$lty<<-LType
                                 Plot_Args$par.settings$strip.background$col<<-"gray90"
                                 AutoKey_Args$col<<-"black"
                              }
                              CtrlPlot()
                            }, container=T3F_SetSymbols)

   layoutRend[4,2] <- T3F_SymSize <- gframe("SYMSIZE", spacing=5, container=layoutRend)
   T3_SymSize <- gcombobox(SymSize, selected=4, editable=FALSE, handler= function(h,...){
                              Plot_Args$cex<<-as.numeric(svalue(T3_SymSize))
                              CtrlPlot()
                            }, container=T3F_SymSize)

   layoutRend[5,1] <- T3F_FitCompStyle <- gframe("FIT COMPONENT LINESTYLE", spacing=5, container=layoutRend)
   T3_FitCompStyle <- gcombobox(c("dotted", "solid", "dashed"), selected=1, editable=FALSE, handler= function(h,...){
                                 PlotParameters$CompLty<<-svalue(T3_FitCompStyle)
                             CtrlPlot() }, container=T3F_FitCompStyle)




   layoutRend[5,2] <- T3F_PanStripCol <- gframe("PANEL STRIP COLOR", spacing=5, container=layoutRend)
   T3_PanStripCol <- gcombobox(c("white","grey", "darkgrey","lightblue","blue","darkblue","deepskyblue","lightbeige","beige","darkbeige","lightpink","pink","darkpink","lightgreen","green","darkgreen"), selected=-1, editable=FALSE, handler= function(h,...){
                             StripCol<-svalue(T3_PanStripCol)
                             if(StripCol=="grey")               { StripCol<-"grey90"
                             } else if (StripCol=="darkgrey")   { StripCol<-"gray60"

                             } else if (StripCol=="lightblue")  { StripCol<-"lightskyblue1"
                             } else if(StripCol=="blue")        { StripCol<-"lightskyblue3"
                             } else if(StripCol=="darkblue")    { StripCol<-"steelblue3"

                             } else if (StripCol=="lightbeige") { StripCol<-"beige"
                             } else if(StripCol=="beige")       { StripCol<-"bisque2"
                             } else if(StripCol=="darkbeige")   { StripCol<-"navajowhite4"

                             } else if (StripCol=="pink")       { StripCol<-"lightpink2"
                             } else if(StripCol=="darkpink")    { StripCol<-"lightpink4"

                             } else if (StripCol=="lightgreen") { StripCol<-"darkseagreen1"
                             } else if(StripCol=="green")       { StripCol<-"darkseagreen2"
                             } else if(StripCol=="darkgreen")    { StripCol<-"mediumseagreen"
                             }
                             Plot_Args$par.settings$strip.background$col<<-StripCol
                             CtrlPlot() }, container=T3F_PanStripCol)

   T3F_CL_Colors <- gframe("SET CORELINE PALETTE", spacing=5, container=T3group3)
   T3_CL_Colors <- gdf(items=CLPalette, selected=-1, editable=FALSE, container=T3F_CL_Colors )  #edit Palette preferences
   addHandlerChanged(T3_CL_Colors, handler=function(h,...){
                             CLPalette$Colors  <<- Colors <<- h$obj[]
                             PlotParameters$Colors<<-Colors
                             Plot_Args$par.settings$superpose.symbol$fill<<-Colors
                             Plot_Args$par.settings$superpose.line$col<<-Colors
                             AutoKey_Args$col<<-Colors
                             Plot_Args$par.settings$superpose.symbol$col<<-Colors
                             CtrlPlot()
                       })

   T3F_Fit_Colors <- gframe("SET FIT PALETTE", spacing=5, container=T3group3)
   T3_Fit_Colors <- gdf(items=FitPalette, selected=-1, editable=FALSE, container=T3F_Fit_Colors )  #edit Palette preferences
   addHandlerChanged(T3_Fit_Colors, handler=function(h,...){
                             FitPalette$FitColors <<- FitColors <<- h$obj[,2]
                             PlotParameters$FitCol <<- FitColors           #save Palette preferences
                             CtrlPlot()
                       })


   gbutton(" RESET PLOT ", handler=function(h,...){
                             ResetPlot()
                             CtrlPlot()
                            }, container=T3group1)

   gbutton(" EXIT ", handler=function(h,...){
                              rm(list = ls(envir = MyEnv, all.names = TRUE), envir = MyEnv)
				                  dispose(win)
                       }, container = T3group1)


# --- TAB4 ---

# Axis Rendering options

   T4group1 <- ggroup(label="AXES", horizontal=FALSE, container=nb)
   layoutAxis <- glayout(homogeneous=FALSE, spacing=3, container=T4group1)

   layoutAxis[1,1] <- T4F_LBTR <- gframe("TICKS", spacing=5, container=layoutAxis)
   T4_LBTR <- gcombobox(c("LeftBottom", "TopRight", "Both", "Custom X", "Custom Y"), selected=1, editable=FALSE, handler= function(h,...){
                             if (svalue(T4_LBTR,index=TRUE)==1) {
                                Plot_Args$scales$tck<<-c(1,0)
                                Plot_Args$scales$alternating<<-c(1)
                             } else if (svalue(T4_LBTR,index=TRUE)==2) {
                                Plot_Args$scales$tck<<-c(0,1)
                                Plot_Args$scales$alternating<<-c(2)
                             } else if (svalue(T4_LBTR,index=TRUE)==3) {
                                Plot_Args$scales$tck<<-c(1,1)
                                Plot_Args$scales$alternating<<-c(3)
                             } else if (svalue(T4_LBTR,index=TRUE)==4 || svalue(T4_LBTR,index=TRUE)==5) {
                                Plot_Args$scales$relation<<-"free"
                                Xlim<-get("O.Xlim", envir=MyEnv)  #recupera il range X da OverlayEngine
                                Ylim<-get("O.Ylim", envir=MyEnv)  #recupera il range Y da OverlayEngine
                                if (svalue(T4_LBTR)=="Custom X") {
                                   CustomDta<-list(Xlim[1], Xlim[2], "X")
                                   CustomAx(CustomDta)
                                }
                                if (svalue(T4_LBTR)=="Custom Y") {
                                   txt1="1) Ymin, Ymax and the number of ticks on the Y axis: es. Ymin=0, Ymax=35, Nticks=7"
                                   txt2="2) Set Tick-Labels (as many labels as the ticks): es. Tick Labels= 0,5, ,15,20, ,30"
                                   CustomDta<-list(Ylim[1], Ylim[2], "Y")
                                   CustomAx(CustomDta)
                                }
                             }
                             CtrlPlot()
                             }, container=T4F_LBTR)


   layoutAxis[1,2] <- T4F_XScale <- gframe("X SCALE", spacing=5, container=layoutAxis)
   T4_XScale <- gcombobox(c("Regular", "Power", "Log.10", "Log.e"), selected=1, editable=FALSE, handler= function(h,...){
                             if (svalue(T4_XScale,index=TRUE)==1) {
                                Plot_Args$scales$x$log<<-FALSE
                                Plot_Args$xscale.components<<-xscale.components.subticks
                             } else if (svalue(T4_XScale,index=TRUE)==2) {
                                Plot_Args$scales$X$log<<-10    # 10^ power scale
                                Plot_Args$xscale.components<<-xscale.components.logpower
                             } else if (svalue(T4_XScale,index=TRUE)==3) {
                                Plot_Args$scales$X$log<<-10    # log10 scale
                                Plot_Args$xscale.components<<-xscale.components.log10ticks
                             } else if (svalue(T4_XScale,index=TRUE)==4) {
                                Plot_Args$scales$X$log<<-"e"   # log e scale
                                Plot_Args$xscale.components<<-xscale.components.subticks
                             }
                             CtrlPlot() }, container=T4F_XScale)

   layoutAxis[1,3] <- T4F_YScale <- gframe("Y SCALE", spacing=5, container=layoutAxis)
   T4_YScale <- gcombobox(c("Regular", "Power", "Log.10", "Log.e"), selected=1, editable=FALSE, handler= function(h,...){
                             if (svalue(T4_YScale,index=TRUE)==1) {
                                Plot_Args$scales$y$log<<-FALSE
                                Plot_Args$yscale.components<<-yscale.components.subticks
                             } else if (svalue(T4_YScale,index=TRUE)==2) {
                                Plot_Args$scales$y$log<<-10
                                Plot_Args$yscale.components<<-yscale.components.logpower
                             } else if (svalue(T4_YScale,index=TRUE)==3) {
                                Plot_Args$scales$y$log<<-10
                                Plot_Args$yscale.components<<-yscale.components.log10ticks
                             } else if (svalue(T4_YScale,index=TRUE)==4) {
                                Plot_Args$scales$y$log<<-"e"
                                Plot_Args$yscale.components<<-yscale.components.subticks
                             }
                             CtrlPlot() }, container=T4F_YScale)

   layoutAxis[2,1] <- T4F_TitSize <- gframe("TITLE SIZE", spacing=5, container=layoutAxis)
   T4_TitSize <- gcombobox(FontSize, selected=5, editable=FALSE, handler= function(h,...){
                             if (PlotParameters$OverlayMode=="Single-Panel" || PlotParameters$OverlayMode=="TreD") {
                                 Plot_Args$main$cex<<-svalue(T4_TitSize)
                             } else if (PlotParameters$OverlayMode=="Multi-Panel") {
                                 Plot_Args$par.strip.text$cex<<-as.numeric(svalue(T4_TitSize))
                             }
                             CtrlPlot() }, container=T4F_TitSize)

   layoutAxis[2,2] <- T4F_MainTitChange <- gframe("CHANGE SINGLE-PANEL TITLE", spacing=5, container=layoutAxis)
   T4_MainTitChange <- gedit("", handler=function(h,...){
                             if (svalue(T4_MainTitChange)==""){return()}
                             if (PlotParameters$OverlayMode=="Single-Panel") {
                                Plot_Args$scales$relation<<-"same"
                                Plot_Args$main$label<<-svalue(T4_MainTitChange)
                                CtrlPlot()
                             } else {
                               return()
                             }
                         }, container=T4F_MainTitChange)

   layoutAxis[2,3] <- T4F_PanelTitles <- gframe("CHANGE MULTI-PANEL TITLES", spacing=5, container=layoutAxis)
   T4_PanelTitles <- gbutton(text="Change Titles", spacing=5, handler=function(h,...){
                                TitleWin <- gwindow(title="MultiPanel Labels", visible=FALSE) # accendo una seconda finestra che di puo' allargare sulle dimensioni del fitParam dataframe
                                TitleGroup <- ggroup(horizontal=FALSE, container=TitleWin)
                                glabel("                           EDIT TITLES                                   ", container=TitleGroup) #label in modo da estendere le dimensioni della finestra
                                PanelTitles<-get("O.PanelTitles", envir=MyEnv)

                                LL=length(PanelTitles)
                                PTitles<-data.frame(TITLES=PanelTitles, stringsAsFactors=FALSE)
                                TitleDFrame <- gdf(items=PTitles, container=TitleGroup)   #LegFrame e' il puntatore a gdf()
                                size(TitleDFrame)<-c(150,200)
                                addHandlerChanged(TitleDFrame, handler=function(h,...){ #addHandlerChanged scarica il dataframe modificato in NewFirParam che e' salvato al di fuori di saveFitParam attraverso la <<-
                                      PanelTitles <<- h$obj[]
                                })
                                gbutton("     SAVE TITLES && EXIT      ", handler=function(h,...){
                                      Plot_Args$PanelTitles<<-PanelTitles
                                      assign("O.PanelTitles", PanelTitles, envir=MyEnv)
                                      dispose(TitleWin)  # finito di editare i parametri annullo la finestra
                                      CtrlPlot()
                                }, container = TitleGroup)
                                visible(TitleWin) <- TRUE
                       }, container=T4F_PanelTitles)



   layoutAxis[3,1] <- T4F_AxNumSize <- gframe("AXIS NUMBER SIZE", spacing=5, container=layoutAxis)
   T4_AxNumSize <- gcombobox(FontSize, selected=3, editable=FALSE, handler= function(h,...){
                             Plot_Args$scales$cex<<-svalue(T4_AxNumSize)
                             CtrlPlot() }, container=T4F_AxNumSize)

   layoutAxis[3,2] <- T4F_AxLabSize <- gframe("AXIS LABEL SIZE", spacing=5, container=layoutAxis)
   T4_AxLabSize <- gcombobox(FontSize, selected=3, editable=FALSE, handler= function(h,...){
                             Plot_Args$xlab$cex<<-svalue(T4_AxLabSize)
                             Plot_Args$ylab$cex<<-svalue(T4_AxLabSize)
                             CtrlPlot() }, container=T4F_AxLabSize)

   layoutAxis[4,1] <- T4F_XAxNameChange <- gframe("CHANGE X-LABEL", spacing=5, container=layoutAxis)
   T4_XAxNameChange <- gedit("", handler=function(h,...){
                             if(svalue(T4_XAxNameChange)==""){retutn()}
                             Plot_Args$xlab$label<<-svalue(T4_XAxNameChange)
                             CtrlPlot() } , container=T4F_XAxNameChange)

   layoutAxis[4,2] <- T4F_YAxNameChange <- gframe("CHANGE Y-LABEL", spacing=5, container=layoutAxis)
   T4_YAxNameChange <- gedit("",handler=function(h,...){
                             if(svalue(T4_YAxNameChange)==""){retutn()}
                             Plot_Args$ylab$label<<-svalue(T4_YAxNameChange)
                             CtrlPlot() }, container=T4F_YAxNameChange)

   layoutAxis[4,3] <- T4F_ZAxNameChange <- gframe("CHANGE Z-LABEL", spacing=5, container=layoutAxis)
   T4_ZAxNameChange <- gedit("",handler=function(h,...){
                             if(svalue(T4_ZAxNameChange)==""){retutn()}
                             Plot_Args$zlab$label<<-svalue(T4_ZAxNameChange)
                             CtrlPlot() }, container=T4F_ZAxNameChange)

   layoutAxis[6,1] <- T4F_XStep <- gframe("X STEP", spacing=5, container=layoutAxis)
   T4_XStep <- gedit("",handler=function(h,...){
                             dx<-as.numeric(svalue(T4_XStep))
                             Xlim<-get("O.Xlim", envir=MyEnv)  #recupera il range X da OverlayEngine
                             Nticks<-as.integer(abs(Xlim[2]-Xlim[1])/dx)
                             Plot_Args$scales$x$tick.number<<-Nticks
                             CtrlPlot() }, container=T4F_XStep)

   layoutAxis[6,2] <- T4F_YStep <- gframe("Y STEP", spacing=5, container=layoutAxis)
   T4_YStep <- gedit("",handler=function(h,...){
                             dy<-as.numeric(svalue(T4_YStep))
                             Ylim<-get("O.Ylim", envir=MyEnv)  #recupera il range Y da OverlayEngine
                             Nticks<-as.integer((Ylim[2]-Ylim[1])/dy)
                             Plot_Args$scales$y$tick.number<<-Nticks
                             CtrlPlot() }, container=T4F_YStep)

   T4F_XYrange <- gframe("CHANGE X, Y RANGE", spacing=5, horizontal=FALSE, container=T4group1)
   T4_XYgroup <- ggroup(horizontal=TRUE, container=T4F_XYrange)
   xx1<-gedit("", initial.msg = "Xmin= ", container=T4_XYgroup)
   xx2<-gedit("", initial.msg = "Xmax= ", container=T4_XYgroup)
   yy1<-gedit("", initial.msg = "Ymin= ", container=T4_XYgroup)
   yy2<-gedit("", initial.msg = "Ymax= ", container=T4_XYgroup)

   T4_OKgroup <- ggroup(horizontal=TRUE, container=T4F_XYrange)  #needed only to have a small OK button
   gbutton("  OK  ", width=25, handler=function(h,...){
                   xx1<-as.numeric(svalue(xx1))
                   xx2<-as.numeric(svalue(xx2))
                   yy1<-as.numeric(svalue(yy1))
                   yy2<-as.numeric(svalue(yy2))
                   if (is.na(xx1*xx2*yy1*yy2)) {
                       gmessage("ATTENTION: plase set all the xmin, xmax, ymin, ymax values!", title = "CHANGE X Y RANGE", icon = "error")
                   } else {
                      if (FName[[SpectIndx]]@Flags) { #Binding energy set
                          Plot_Args$xlim<<-Xlim<-sort(c(xx1, xx2), decreasing=TRUE)
                          Plot_Args$ylim<<-Ylim<-sort(c(yy1, yy2))
                          assign("O.Xlim", Xlim, envir=MyEnv)
                          assign("O.Ylim", Ylim, envir=MyEnv)
                      } else {
                          Plot_Args$xlim<<-Xlim<-sort(c(xx1, xx2))
                          Plot_Args$ylim<<-Ylim<-sort(c(yy1, yy2))
                          assign("O.Xlim", Xlim, envir=MyEnv)
                          assign("O.Ylim", Ylim, envir=MyEnv)
                      }
                   }
                   CtrlPlot() }, container = T4_OKgroup)


   gbutton(" RESET PLOT ", handler=function(h,...){
                             ResetPlot()
                             CtrlPlot()
                            }, container=T4group1)

   gbutton(" EXIT ", handler=function(h,...){
                              rm(list = ls(envir = MyEnv, all.names = TRUE), envir = MyEnv)
				                  dispose(win)
                       }, container = T4group1)


# --- TAB5 ---

### LEGEND SETTINGS

   T5group1 <- ggroup(label="LEGEND", horizontal=FALSE, container=nb)

   layoutLeg <- glayout(homogeneous=FALSE, spacing=3, container=T5group1)

   layoutLeg[1,1]<-T5F_legendCK <- gframe(text="Enable Legend", spacing=5, container=layoutLeg)
   legendCK <- gcheckbox("Enable Legend ON/OFF", checked=FALSE,handler=function(h,...){
                          SelectedNames<-get("O.SelectedNames", envir=MyEnv)
                          Legends<-SelectedNames$CoreLines
                          for(ii in seq_along(Legends)){
                             tmp<-unlist(strsplit(Legends[ii], "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                             Legends[ii]<-tmp[2]
                          }
                          AutoKey_Args$text<<-Legends  #load the Legends in the slot of the AutoKey_Args = List of parameters defining legend properties
                          if (svalue(legendCK)==TRUE) {
		           	           Plot_Args$auto.key <<- AutoKey_Args  #Save the AutoKey_Args list of par in Plot_Args$auto.key
                             if (svalue(T3_SetLines)=="ON") {   #selezionate LINEE
                                Plot_Args$par.settings$superpose.line$col<<-"black" #B/W plot
                                Plot_Args$par.settings$superpose.line$lty<<-LType
                                if (PlotParameters$OverlayMode=="Multi-Panel") {
                                   Plot_Args$par.settings$superpose.line$lty<<-"solid"
                                   Plot_Args$scales$relation<<-"free"
                                }
 		           	              if (svalue(T3_BW_Col)=="RainBow") {                    #COLOR plot
                                   Plot_Args$par.settings$superpose.line$col<<-Colors
                                   Plot_Args$par.settings$superpose.line$lty<<-"solid"
                                }
                             }
                             if (svalue(T3_SetSymbols)=="ON") {   #selezionate SIMBOLI
                                Plot_Args$par.settings$superpose.symbol$col<<-"black"  #B/W plot
                                Plot_Args$par.settings$superpose.symbol$pch<<-STypeIndx
                                if (PlotParameters$OverlayMode=="Multi-Panel") {
                                   Plot_Args$par.settings$superpose.symbol$pch<<-1
                                   Plot_Args$par.settings$superpose.symbol$col<<-"black"
                                   Plot_Args$scales$relation<<-"free"
                                }
 		           	              if (svalue(T3_BW_Col)=="RainBow") {                       #COLOR plot
                                   Plot_Args$par.settings$superpose.symbol$col<<-Colors
                                   Plot_Args$par.settings$superpose.symbol$pch<<-1
                                }
                             }
                          } else {
		           	           Plot_Args$auto.key <<- FALSE
	           	           }
                          CtrlPlot()
                       }, container=T5F_legendCK)

   layoutLeg[1,2]<-T5F_LegFNameCK <- gframe(text="Add XPSSamp Name", spacing=5, container=layoutLeg)
   LegFNameCK <- gcheckbox("XPSSamp.Name ON/OFF", checked=FALSE,handler=function(h,...){
                          if (is.logical(Plot_Args$auto.key)){
                             gmessage("PLEASE ENABLE LEGENDS", icon="warning")
                             svalue(LegFNameCK)<-FALSE
                          } else {
                             SelectedNames<-get("O.SelectedNames", envir=MyEnv)
                             if (svalue(LegFNameCK)==TRUE) {
                                Legends<-SelectedNames$CoreLines
                                for (ii in seq_along(SelectedNames$XPSSample)){
                                    tmp<-unlist(strsplit(Legends[ii], "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                                    Legends[ii]<-paste(tmp[2], "_", SelectedNames$XPSSample[ii], sep="")
                                }
                                Plot_Args$auto.key$text<<-as.vector(Legends)
                             } else {
                                Legends<-SelectedNames$CoreLines
                                for(ii in seq_along(Legends)){
                                   tmp<-unlist(strsplit(Legends[ii], "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                                   Legends[ii]<-tmp[2]
                                }
                                Plot_Args$auto.key$text<<-as.vector(Legends)
                             }
                          }
                          CtrlPlot()
                       }, container=T5F_LegFNameCK)


   layoutLeg[2,1]<-T5F_LegPosCK <- gframe(text="Legend Position", spacing=5, container=layoutLeg)
   LegPosCK <- gcombobox(LegPos,selected=-1, toolkit = guiToolkit(), handler=function(h,...){
                           if (PlotParameters$OverlayMode=="Multi-Panel"||PlotParameters$OverlayMode=="TreD") {
                               gmessage("WARNING: LEGEND POSITION OPTION NOT AVAILABLE FOR MULTIPANEL OR 3D-PLOTS", title = "Legend Position",  icon = "warning")
                           } else {
	                           switch(svalue(LegPosCK),
                                 "OutsideTop" = { Plot_Args$auto.key$space <<- "top" },
				                     "OutsideRight" = { Plot_Args$auto.key$space <<- "right" },
				                     "OutsideLeft"  = { Plot_Args$auto.key$space <<- "left" },
			                        "OutsideBottom" = { Plot_Args$auto.key$space <<- "bottom" },
				                     "InsideTopRight" = { Plot_Args$auto.key$space <<- NULL
                                                      Plot_Args$auto.key$corner<<-c(1,1)
                                                      Plot_Args$auto.key$x<<- 0.95
                                                      Plot_Args$auto.key$y<<- 0.95 },
				                     "InsideTopLeft" =  { Plot_Args$auto.key$space <<- NULL
                                                      Plot_Args$auto.key$corner<<-c(0,1)
                                                      Plot_Args$auto.key$x<<- 0.05
                                                      Plot_Args$auto.key$y<<- 0.95 },
                                 "InsideBottomRight" = { Plot_Args$auto.key$space <<- NULL
                                                      Plot_Args$auto.key$corner<<-c(1,0)
                                                      Plot_Args$auto.key$x<<- 0.95
                                                      Plot_Args$auto.key$y<<- 0.05 },
				                     "InsideBottomLeft"  = {	Plot_Args$auto.key$space <<- NULL
                                                      Plot_Args$auto.key$corner<<-c(0,0)
                                                      Plot_Args$auto.key$x<<- 0.05
                                                      Plot_Args$auto.key$y<<- 0.05 },
                             )
                          }
                          CtrlPlot()
                       }, container=T5F_LegPosCK)

   layoutLeg[2,2]<-T5F_LegColCK <- gframe(text="Group Legend and organize in columns", spacing=5, container=layoutLeg)
   LegColCK <- gedit(initial.msg ="Col. numb.", selected=1, editable=FALSE, handler=function(h,...){
                          columns<-svalue(LegColCK)
                          Plot_Args$auto.key$columns<<-as.numeric(svalue(LegColCK))
                          CtrlPlot()
                       }, container=T5F_LegColCK)

   layoutLeg[3,1]<-T5F_TSizeCK <- gframe(text="Text Size", spacing=5, container=layoutLeg)
   TSizeCK <- gcombobox(LegTxtSize,selected=1, toolkit = guiToolkit(), handler=function(h,...){
		           	        Plot_Args$auto.key$cex <<- as.numeric(svalue(TSizeCK))
                          CtrlPlot()
                       }, container=T5F_TSizeCK)

   layoutLeg[3,2]<-T5F_DistCK <- gframe(text="Distance from Margin", spacing=5, container=layoutLeg)
   DistCK <- gcombobox(LegDist,selected=5, toolkit = guiToolkit(), handler=function(h,...){
                           if (PlotParameters$OverlayMode=="Multi-Panel" || PlotParameters$TreD==TRUE) {
                               gmessage("WARNING: LEGEND POSITION OPTION NOT AVAILABLE FOR MULTIPANEL OR 3D-PLOTS", title = "Legend Position",  icon = "warning")
                           } else {
                              Dist<-as.numeric(svalue(DistCK))
			                     switch(svalue(LegPosCK),
                                 "OutsideTop" = { Plot_Args$auto.key$space <<- "top"
                                               Plot_Args$auto.key$y<<- 1+Dist },
				                     "OutsideRight" = { Plot_Args$auto.key$space <<- "right"
                                                    Plot_Args$par.settings$layout.widths$right.padding<<-8-Dist*40
                                                    Plot_Args$par.settings$layout.widths$key.right<<-Dist*10 },
				                     "OutsideLeft" = { Plot_Args$auto.key$space <<- "left"
                                                    Plot_Args$par.settings$layout.widths$left.padding<<-8-Dist*40
                                                    Plot_Args$par.settings$layout.widths$key.left<<-Dist*10 },
			                        "OutsideBottom" = { Plot_Args$auto.key$space <<- "bottom"
                                                  Plot_Args$auto.key$y<<- 1-Dist },
				                     "InsideTopRight" = { Plot_Args$auto.key$space <<- NULL
                                                   Plot_Args$auto.key$corner<<-c(1,1)
                                                   Plot_Args$auto.key$x<<- 1-Dist
                                                   Plot_Args$auto.key$y<<- 1-Dist },
				                     "InsideTopLeft" =  { Plot_Args$auto.key$space <<- NULL
                                                   Plot_Args$auto.key$corner<<-c(0,1)
                                                   Plot_Args$auto.key$x<<- Dist
                                                   Plot_Args$auto.key$y<<- 1-Dist },
                                 "InsideBottomRight" = { Plot_Args$auto.key$space <<- NULL
                                                      Plot_Args$auto.key$corner<<-c(1,0)
                                                      Plot_Args$auto.key$x<<- 1-Dist
                                                      Plot_Args$auto.key$y<<- Dist },
				                     "InsideBottomLeft"  = {	Plot_Args$auto.key$space <<- NULL
                                                      Plot_Args$auto.key$corner<<-c(0,0)
                                                      Plot_Args$auto.key$x<<- Dist
                                                      Plot_Args$auto.key$y<<- Dist },
                              )
                           }
                           CtrlPlot()
                       }, container=T5F_DistCK)

   layoutLeg[4,1]<-T5F_LineWdhCK <- gframe(text="Line/Symbol weight", spacing=5, container=layoutLeg)
   LineWdhCK <- gcombobox(LWidth,selected=1, toolkit = guiToolkit(), handler=function(h,...){
                          weight<-as.numeric(svalue(LineWdhCK))
                          if (svalue(T3_SetLines)=="ON") {   #selezionate LINEE
                             Plot_Args$par.settings$superpose.line$lwd<<-weight
                          }
                          if (svalue(T3_SetSymbols)=="ON") {   #selezionati SIMBOLI
                             Plot_Args$par.settings$superpose.symbol$cex<<-weight
                          }
                          CtrlPlot()
                       }, container=T5F_LineWdhCK)

   layoutLeg[4,2]<-T5F_TxtColCK <- gframe(text="Legend text Color", spacing=5, container=layoutLeg)
   TxtColCK <- gcombobox(c("B/W", "RainBow"),selected=1, toolkit = guiToolkit(), handler=function(h,...){
                          if  (svalue(TxtColCK)=="B/W"){
                              Plot_Args$auto.key$col<<-"black"
                          } else {
                              Plot_Args$auto.key$col<<-Colors
                          }
                          CtrlPlot()
                       }, container=T5F_TxtColCK)



   layoutLeg[5,1]<-T5F_ChangLeg <- gbutton(text="Change Legend", spacing=5, handler=function(h,...){
                                LegWin <- gwindow(title="XPS Sample Legends", visible=FALSE) # accendo una seconda finestra che di puo' allargare sulle dimensioni del fitParam dataframe
                                LegGroup <- ggroup(horizontal=FALSE, container=LegWin)
                                glabel("                           EDIT LEGENDS                                   ", container=LegGroup) #label in modo da estendere le dimensioni della finestra
                                SelectedNames<-get("O.SelectedNames", envir=MyEnv)
                                LL=length(SelectedNames$XPSSample)
                                Legends<-data.frame(LEGENDS=rep("-", LL), stringsAsFactors=FALSE)
                                LegDFrame <- gdf(items=Legends, container=LegGroup)   #LegFrame e' il puntatore a gdf()
                                size(LegDFrame)<-c(150,150)
                                addHandlerChanged(LegDFrame, handler=function(h,...){ #addHandlerChanged scarica il dataframe modificato in NewFirParam che e' salvato al di fuori di saveFitParam attraverso la <<-
                                      Legends <<- h$obj[]
                                })
                                gbutton("     SAVE LEGENDS & EXIT      ", handler=function(h,...){
                                      svalue(NewLegend)<-NULL
                                      Plot_Args$auto.key$text<<-as.vector(Legends)
                                      dispose(LegWin)  # finito di editare i parametri annullo la finestra
                                      unblockHandler(NewLegend)
                                      CtrlPlot()
                                }, container = LegGroup)
                                visible(LegWin) <- TRUE
                                CtrlPlot()
                       }, container=layoutLeg)

   layoutLeg[5,2]<-NewLegend<-gbutton(text=" Annotate ", handler=function(h,...){
                                Xlim<-Plot_Args$xlim   #in caso di Zoom Xlim, Ylim non Nulli
                                Ylim<-Plot_Args$ylim
                                if (is.null(Xlim)){    #non e' stato selezionato alcuno zoom
                                   Xlim<-get("O.Xlim", envir=MyEnv)  #recupera il range X da OverlayEngine
                                   Ylim<-get("O.Ylim", envir=MyEnv)  #recupera il range Y da OverlayEngine
                                }
                                XPSLattAnnotate(Xlim, Ylim)
                       }, container=layoutLeg)


   gbutton(" RESET PLOT ", handler=function(h,...){
                               ResetPlot()
                               CtrlPlot()
                       }, container=T5group1)


   gbutton(" EXIT ", handler=function(h,...){
                              rm(list = ls(envir = MyEnv, all.names = TRUE), envir = MyEnv)
				                  dispose(win)
                       }, container = T5group1)


#----- END NOTEBOOK -----

   enabled(objFunctFact)<-FALSE
   enabled(T4_ZAxNameChange)<-FALSE
   enabled(T4_PanelTitles)<-FALSE
   svalue(nb) <- 5 #refresh notebook pages
   svalue(nb) <- 4 #refresh notebook pages
   svalue(nb) <- 3
   svalue(nb) <- 2
   svalue(nb) <- 1
   tcl("update", "idletasks")
   visible(win) <- TRUE
}

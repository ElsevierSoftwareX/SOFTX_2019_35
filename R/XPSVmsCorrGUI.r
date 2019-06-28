#function to correct VAMAS-data for the analyzer transmission factor

#'Corrects XPSSample Vamas_type data for the analyzer transmission
#'
#'This routine divides raw data intensity for the value of the analyzer transmission
#'The routine applies the correction to the selected coreline or to all the XPSSample
#'spectra, baselines and fit components when present.
#'
#'
#'@examples
#'
#'\dontrun{
#'	XPSVmsCorr()
#'}
#'
#'@export
#'


XPSVmsCorr <- function(){

   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FName<-NULL
   activeFName<-""
   SpectList<-""
   FNameList<-XPSFNameList()
   NSpect<-NULL

   mainwin <- gwindow("DATA TRANSMISSION CORRECTION", visible=FALSE)
   maingroup<- ggroup(label="TransCorr", horizontal=FALSE, container=mainwin)

#   group1 <- ggroup(label="XPSSample selection", horizontal=FALSE, container=maingroup)
   frame1 <-gframe(text="SELECT THE SOURCE XPS-SAMPLE", spacing=5, container=maingroup)
   obj1 <- gcombobox(FNameList, selected=-1, horizontal=TRUE, handler=function(h,...){
                          SelectedFName<-svalue(obj1)
                          FName<<-get(SelectedFName,envir=.GlobalEnv)  #carico in SampID il relativo XPSSAmple
                          activeFName<<-get("activeFName",envir=.GlobalEnv)  #carico in SampID il relativo XPSSAmple
                          SpectList<<-c(XPSSpectList(SelectedFName), "All")
                          NSpect<<-length(FName)
                          delete(frame2,obj2)
                          obj2 <<- gcombobox(SpectList, selected=-1, handler=function(h,...){
                                                  enabled(correct)<-TRUE
                                                  enabled(replace)<-TRUE
                                             }, container=frame2)
                          add(frame2,obj2)
                          plot(FName)
                          enabled(obj2) <- TRUE #abilito la scelta della coreline
                          msg=""
                          for (ii in 1:NSpect) {
                              LL=length(FName[[ii]]@Flags)   #File letti con vecchia routine senza correzione automatica
                              if (LL==3){
                                 FName[[ii]]@Flags<<-c(FName[[ii]]@Flags, FALSE)  #setto flag correzione a falso
                              }
                              if (FName[[ii]]@Flags[[4]]==FALSE) {                #questa condizione e' quando vecchia lettura o quando si elimina correzione transmission da una o piu' corelines
                                  msg<-c(msg,paste("CRTL of Raw Spectrum", FName[[ii]]@Symbol, ": => OK for correction"))
                              }
                              if (FName[[ii]]@Flags[[4]]==TRUE) {
                                  msg<-c(msg,paste("Raw Spectrum", FName[[ii]]@Symbol, ": => WARNING correction ALREADY DONE!"))
                              }
                          }
                          svalue(infoWin)<-msg
                          tcl("update", "idletasks")
                       }, container=frame1)


#   group2 <- ggroup(label="VAMAS DATA CORRECTION", horizontal=FALSE, container=maingroup)
   frame2 <-gframe(text="SELECT SPECTRUM TO CORRECT", spacing=5, container=maingroup)
   obj2 <- gcombobox(SpectList, selected=-1, horizontal=TRUE, container=frame2)

   frame3 <-gframe(text="CORRECTION FOR THE ANALYZER TRANSMISSION", spacing=5, container=maingroup)
   correct<-gbutton(" Correct Data ", handler=function(h,...){
                             msg<-""
                             Spect<-svalue(obj2)
                             if (Spect=="All") {
                                for (ii in 1:NSpect) {
                                   if (FName[[ii]]@Flags[[4]]==FALSE) {
                                      FName[[ii]]@.Data[[2]]<<-FName[[ii]]@.Data[[2]]/FName[[ii]]@.Data[[3]] # correzione RAW DATA per transmission
                                      LL=length(FName[[ii]]@RegionToFit$x)
                                      if (LL>0) {
                                         idx1=findXIndex(FName[[ii]]@.Data[[1]], FName[[ii]]@RegionToFit$x[1])   #RegionToFot e' un sottoinsieme di .Data
                                         idx2=findXIndex(FName[[ii]]@.Data[[1]], FName[[ii]]@RegionToFit$x[LL])
                                         TrasmFact<-FName[[ii]]@.Data[[3]][idx1:idx2]               #estraggo i valori di trasmisione relativi a RegionToFit
                                         FName[[ii]]@RegionToFit$y<<-FName[[ii]]@RegionToFit$y/TrasmFact # correzione REGIONTOFIT per transmission se presente fit
                                         FName[[ii]]@Baseline$y<<-FName[[ii]]@Baseline$y/TrasmFact # correzione BASELINE per transmissionse presente fit
                                         FName[[ii]]@Fit$y<<-FName[[ii]]@Fit$y/TrasmFact
                                         NComp<-length(FName[[ii]]@Components)
                                         if (NComp>0) {
                                            for (jj in 1:NComp) {
                                                FName[[ii]]@Components[[jj]]@ycoor<<-FName[[ii]]@Components[[jj]]@ycoor/TrasmFact # correzione COMPONENTI FIT per transmission
                                                tmp<-as.matrix(FName[[ii]]@Components[[jj]]@param)     #valori h: start min max
                                                tmp[1,]<-tmp[1,]/mean(TrasmFact)                 #scelgo la media dei valori di trasmissione per correggere
                                                FName[[ii]]@Components[[jj]]@param<<-as.data.frame(tmp) #rimetto i parametri corretti nella slot dell'XPSSample
                                                FName[[ii]]@Flags[[4]]<<-TRUE                    #setto a TRUE: correzione per transmission OK
                                            }
                                         }
                                      }
                                      msg<-c(msg,paste("Raw Spectrum", FName[[ii]]@Symbol, "Corrected!"))
                                      svalue(infoWin)<-msg
                                      tcl("update", "idletasks")
                                   } else {
                                      msg<-c(msg,paste("Skip", FName[[ii]]@Symbol, "Correction!"))
                                      svalue(infoWin)<-msg
                                      tcl("update", "idletasks")
                                   }
                                }
                             } else {
                                SourceCoreline<-unlist(strsplit(Spect, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                                idx<-as.integer(SourceCoreline[1])
                                if (FName[[idx]]@Flags[[4]]==FALSE) {
                                   FName[[idx]]@.Data[[2]]<<-FName[[idx]]@.Data[[2]]/FName[[idx]]@.Data[[3]] # correzione RAW DATA per transmission
                                   LL=length(FName[[idx]]@RegionToFit$x)
                                   if (LL>0) {
                                      idx1=findXIndex(FName[[idx]]@.Data[[1]], FName[[idx]]@RegionToFit$x[1])   #RegionToFot e' un sottoinsieme di .Data
                                      idx2=findXIndex(FName[[idx]]@.Data[[1]], FName[[idx]]@RegionToFit$x[LL])
                                      TrasmFact<-FName[[idx]]@.Data[[3]]               
                                      TrasmFact<-TrasmFact[idx1:idx2]#estraggo i valori di trasmisione relativi a RegionToFit
                                      FName[[idx]]@RegionToFit$y<<-FName[[idx]]@RegionToFit$y/TrasmFact # correzione REGIONTOFIT per transmission se presente fit
                                      FName[[idx]]@Baseline$y<<-FName[[idx]]@Baseline$y/TrasmFact # correzione BASELINE per transmissionse presente fit
                                      FName[[idx]]@Fit$y<<-FName[[idx]]@Fit$y/TrasmFact
                                      NComp<-length(FName[[idx]]@Components)
                                      if (NComp>0) {
                                         for (jj in 1:NComp) {
                                             FName[[idx]]@Components[[jj]]@ycoor<<-FName[[idx]]@Components[[jj]]@ycoor/TrasmFact # correzione COMPONENTI FIT per transmission
                                             tmp<-as.matrix(FName[[idx]]@Components[[jj]]@param)     #valori h: start min max
                                             tmp[1,]<-tmp[1,]/mean(TrasmFact)                 #scelgo la media dei valori di trasmissione per correggere
                                             FName[[idx]]@Components[[jj]]@param<<-as.data.frame(tmp) #rimetto i parametri corretti nella slot dell'XPSSample
                                         }
                                      }
                                   }
                                   FName[[idx]]@Flags[[4]]<<-TRUE                    #setto a TRUE: correzione per transmission OK
                                   msg<-c(msg,paste("Raw Spectrum", FName[[idx]]@Symbol, "Corrected!"))
                                   svalue(infoWin)<-msg
                                   tcl("update", "idletasks")
                                } else {
                                   msg<-c(msg,paste("Skipped", FName[[idx]]@Symbol, "Correction!"))
                                   svalue(infoWin)<-msg
                                   tcl("update", "idletasks")
                                }
                             }
                          }, container=frame3)

   frame4 <-gframe(text="RAW DATA GENERATION", spacing=5, container=maingroup)
   replace<-gbutton("Replace Raw Data", handler=function(h,...){
                             msg<-""
                             Spect<-svalue(obj2)
                             if (Spect=="All") {
                                for (ii in 1:NSpect) {
                                   if (FName[[ii]]@Flags[[4]]==TRUE) {
                                      FName[[ii]]@.Data[[2]]<<-FName[[ii]]@.Data[[2]]*FName[[ii]]@.Data[[3]] # elimino correzione RAW DATA per transmission
                                      LL=length(FName[[ii]]@RegionToFit$x)
                                      if (LL>0) {
                                         idx1=findXIndex(FName[[ii]]@.Data[[1]], FName[[ii]]@RegionToFit$x[1])   #RegionToFot e' un sottoinsieme di .Data
                                         idx2=findXIndex(FName[[ii]]@.Data[[1]], FName[[ii]]@RegionToFit$x[LL])
                                         TrasmFact<-FName[[ii]]@.Data[[3]][idx1:idx2]               #estraggo i valori di trasmisione relativi a RegionToFit
                                         FName[[ii]]@RegionToFit$y<<-FName[[ii]]@RegionToFit$y*TrasmFact # elimino correzione REGIONTOFIT per transmission se presente fit
                                         FName[[ii]]@Baseline$y<<-FName[[ii]]@Baseline$y*TrasmFact # elimino correzione BASELINE per transmissionse presente fit
                                         FName[[ii]]@Fit$y<<-FName[[ii]]@Fit$y*TrasmFact
                                         NComp<-length(FName[[ii]]@Components)
                                         if (NComp>0) {
                                            for (jj in 1:NComp) {
                                                FName[[ii]]@Components[[jj]]@ycoor<<-FName[[ii]]@Components[[jj]]@ycoor*TrasmFact # elimino correzione COMPONENTI FIT per transmission
                                                tmp<-as.matrix(FName[[ii]]@Components[[jj]]@param)     #valori h: start min max
                                                tmp[1,]<-tmp[1,]*mean(TrasmFact)                  #scelgo la media dei valori di trasmissione per eliminare correzione
                                                FName[[ii]]@Components[[jj]]@param<<-as.data.frame(tmp) #rimetto i parametri corretti nella slot dell'XPSSample
                                                FName[[ii]]@Flags[[4]]<<-FALSE                    #setto a TRUE: correzione per transmission eliminata
                                            }
                                         }
                                      }
                                      msg<-c(msg,paste("Spectrum", FName[[ii]]@Symbol, "Original Raw Data Replaced!"))
                                      svalue(infoWin)<-msg
                                      tcl("update", "idletasks")
                                   } else {
                                      msg<-c(msg,paste("Skipped", FName[[ii]]@Symbol, "Correction!"))
                                      svalue(infoWin)<-msg
                                      tcl("update", "idletasks")
                                   }
                                }
                             } else {
                                SourceCoreline<-unlist(strsplit(Spect, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                                idx<-as.integer(SourceCoreline[1])
                                if (FName[[idx]]@Flags[[4]]==TRUE) {
                                   FName[[idx]]@.Data[[2]]<<-FName[[idx]]@.Data[[2]]*FName[[idx]]@.Data[[3]] # correzione RAW DATA per transmission
                                   LL=length(FName[[idx]]@RegionToFit$x)
                                   if (LL>0) {
                                      idx1=findXIndex(FName[[idx]]@.Data[[1]], FName[[idx]]@RegionToFit$x[1])   #RegionToFot e' un sottoinsieme di .Data
                                      idx2=findXIndex(FName[[idx]]@.Data[[1]], FName[[idx]]@RegionToFit$x[LL])
                                      TrasmFact<-FName[[idx]]@.Data[[3]][idx1:idx2]               #estraggo i valori di trasmisione relativi a RegionToFit
                                      FName[[idx]]@RegionToFit$y<<-FName[[idx]]@RegionToFit$y*TrasmFact # correzione REGIONTOFIT per transmission se presente fit
                                      FName[[idx]]@Baseline$y<<-FName[[idx]]@Baseline$y*TrasmFact # correzione BASELINE per transmissionse presente fit
                                      FName[[idx]]@Fit$y<<-FName[[idx]]@Fit$y*TrasmFact
                                      NComp<-length(FName[[idx]]@Components)
                                      if (NComp>0) {
                                         for (jj in 1:NComp) {
                                             FName[[idx]]@Components[[jj]]@ycoor<<-FName[[idx]]@Components[[jj]]@ycoor*TrasmFact # correzione COMPONENTI FIT per transmission
                                             tmp<-as.matrix(FName[[idx]]@Components[[jj]]@param)     #valori h: start min max
                                             tmp[1,]<-tmp[1,]*mean(TrasmFact)                 #scelgo la media dei valori di trasmissione per correggere
                                             FName[[idx]]@Components[[jj]]@param<<-as.data.frame(tmp) #rimetto i parametri corretti nella slot dell'XPSSample
                                         }
                                      }
                                   }
                                   FName[[idx]]@Flags[[4]]<<-TRUE                    #setto a TRUE: correzione per transmission OK
                                   msg<-c(msg,paste("Spectrum", FName[[idx]]@Symbol, ": original Raw Data Replaced!"))
                                   svalue(infoWin)<-msg
                                   tcl("update", "idletasks")
                                } else {
                                   msg<-c(msg,paste("Skipped", FName[[idx]]@Symbol, "Replacement!"))
                                   svalue(infoWin)<-msg
                                   tcl("update", "idletasks")
                                }
                             }
                          }, container=frame4)



   frame4 <-gframe(text="CONTROL INFOs", spacing=5, container=maingroup)
   infoWin <- gtext(text="", container = frame4)
   size(infoWin) <- c(400, 200)

   gbutton(" SAVE ", handler=function(h,...){
      activeFName<-FName@Filename
      activeSpectIndx <- 1
      activeSpectName<-   FName[[1]]@Symbol
      assign(activeFName, FName, envir=.GlobalEnv) #salvo le modifiche nel destination file
      assign("activeFName", activeFName, envir=.GlobalEnv) #setto lo l'XPSSample attivo eguale destinaiton filename
      assign("activeSpectName", activeSpectName,envir=.GlobalEnv) #setto lo spettro attivo eguale alla coreline aggiunta
      assign("activeSpectIndx", activeSpectIndx,envir=.GlobalEnv) #setto l'indice attivo eguale a quello della coreline aggiunta
      XPSSaveRetrieveBkp("save")
   }, container=maingroup)


   gbutton(" SAVE & EXIT ", handler=function(h,...){
      activeFName<-FName@Filename
      activeSpectIndx <- 1
      activeSpectName<-   FName[[1]]@Symbol
      assign(activeFName, FName, envir=.GlobalEnv) #salvo le modifiche nel destination file
      assign("activeFName", activeFName, envir=.GlobalEnv) #setto lo l'XPSSample attivo eguale destinaiton filename
      assign("activeSpectName", activeSpectName,envir=.GlobalEnv) #setto lo spettro attivo eguale alla coreline aggiunta
      assign("activeSpectIndx", activeSpectIndx,envir=.GlobalEnv) #setto l'indice attivo eguale a quello della coreline aggiunta
      dispose(mainwin)
      XPSSaveRetrieveBkp("save")
   }, container=maingroup)



   enabled(obj2) <- FALSE #disabilito la scelta della coreline
   enabled(correct) <- FALSE #disabilito la scelta della coreline
   enabled(replace) <- FALSE #disabilito la scelta della coreline
   visible(mainwin) <- TRUE
}
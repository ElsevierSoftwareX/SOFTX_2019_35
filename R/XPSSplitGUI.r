# GUI to split an XPSSample in groups its corelines
# The GUI shows a list of the XPSSamples loaded
# For the selected XPSSample the GUI shows the list of Corelines acquired
# A group of corelines can be chosen through a checkbox
# The chosen corelines may be saved in a new XPSSample.

#'To select split multiple acquisitions performed with the ScientaGammadata instrument
#'
#'Acquisitions on multiple samples may be included in a single .PXT file
#'This function allows splitting spectra corresponding to each sample in 
#'individual files.
#'No parameters are passed to this function
#'
#'
#'@examples
#'
#'\dontrun{
#'	XPSSplit()
#'}
#'
#'@export 
#'




XPSSplit <- function() {

     if (is.na(activeFName)){
         gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
         return()
     }

     FName<-get(activeFName, envir=.GlobalEnv)   #carico l'XPSSample dataFrame attivo in FName variabile globale
     activeFName<-get("activeFName", envir=.GlobalEnv)  #carico il nome XPSSample (stringa)
     FNameListTot<-XPSFNameList()     #lista di tutti gli XPSSample caricati nel WorkSpace
     jj<-grep(activeFName, FNameListTot)
     CoreLineList<-XPSSpectList(activeFName)
     LL<-length(CoreLineList)
     if (LL>20) {
        CoreLineList1<-CoreLineList[1:20]
        CoreLineList2<-CoreLineList[21:LL]
        TwoCKlists<-TRUE
     } else {
        CoreLineList1<-CoreLineList[1:LL]
        CoreLineList2<-NULL
        TwoCKlists<-FALSE
     }

#--- GUI
     Swin <- gwindow(" SPLIT PXT FILEDATA ", visible=FALSE)
     maingroup<-ggroup(horizontal=TRUE, container=Swin)

     T1group1 <- ggroup(label="XPS SAMPLE SELECTION", spacing=5, horizontal=FALSE, container=maingroup)

#FRAME1: PIU XPS Samples PIU corelines
     layoutT1 <- glayout(homogeneous=FALSE, spacing=5, container=T1group1)
     layoutT1[1,1] <- T1frameFName <- gframe(text="Select the XPS-SAMPLE", spacing=5, container=layoutT1)
     T1FNameListCK <- gradio(FNameListTot,selected=jj, container=T1frameFName)

     layoutT1[1,2] <- T1frameCoreLines <- gframe(text="Select the CORE LINES to export",  spacing=5, container=layoutT1)
     T1groupCoreLines <- ggroup(horizontal=TRUE,container = T1frameCoreLines)
     T1CoreLineCK1 <- gcheckboxgroup(CoreLineList1,checked=FALSE, handler=NULL, container=T1groupCoreLines)
     T1CoreLineCK2 <- gcheckboxgroup(CoreLineList2,checked=FALSE, handler=NULL, container=T1groupCoreLines)

     addHandlerChanged(T1FNameListCK, handler=function(h,...){
                            activeFName<-svalue(T1FNameListCK)
                            delete(T1frameCoreLines ,T1groupCoreLines) #cancello la lista delle coreline quando ho fatto la selezione
                            CoreLineList<-XPSSpectList(activeFName)
                            LL<-length(CoreLineList)
                            if (LL>20) {
                               CoreLineList1<-CoreLineList[1:20]
                               CoreLineList2<-CoreLineList[21:LL]
                               TwoCKlists<-TRUE
                            } else {
                               CoreLineList1<-CoreLineList[1:LL]
                               CoreLineList2<-NULL
                               TwoCKlists<-FALSE
                            }
                            T1groupCoreLines <<- ggroup(horizontal=TRUE,container = T1frameCoreLines)
                            T1CoreLineCK1 <<- gcheckboxgroup(CoreLineList1,checked=FALSE, handler=NULL, container=T1groupCoreLines)
                            T1CoreLineCK2 <<- gcheckboxgroup(CoreLineList2,checked=FALSE, handler=NULL, container=T1groupCoreLines)
                            FName<-get(activeFName, envir=.GlobalEnv)
                            plot(FName)
                      })

     gbutton("EXPORT DATA TO .RData FILE", handler=function(h,...){
                            activeFName<-svalue(T1FNameListCK)
                            CoreLineList<-svalue(T1CoreLineCK1, index=TRUE)
                            if (TwoCKlists) {
                               CoreLineList2<-svalue(T1CoreLineCK2, index=TRUE)
                               CoreLineList2<-CoreLineList2+20
                               CoreLineList<-c(CoreLineList,CoreLineList2)
                            }
                            FName<-get(activeFName, envir=.GlobalEnv)
#--- building the NewXPSSample datafile
	                         NewFName <- new("XPSSample")
                            LL<-length(CoreLineList)
                            for (ii in 1:LL){
                                NewFName[[ii]]<-FName[[CoreLineList[ii]]]
                            }
                            NewFName@Project<-FName@Project
                            NewFName@Sample<-FName@Sample
                            NewFName@Comments<-FName@Comments
                            NewFName@User<-FName@User
                            spectNames<-names(FName)
                            NewFName@names<-spectNames[CoreLineList]
                            plot(NewFName)

                            FileName<-gfile(type="save", initial.filename=NULL, initial.dir=getwd() ) #scelgo filename con pannello interattivo                         SpectName<-svalue(EAobj1)
#--- original expression    filename<-paste(dirname(PathName), "/", basename(PathName), sep="") #cambia i separatori da \\ a /
                            activeFName<-basename(FileName) #solo il nomefile
                            NewFName@Filename<-activeFName
                            FileName<-unlist(strsplit(FileName, "\\."))
                            if( is.na(FileName[2])) {        #se non c'e' estensione allora aggiungo  .txt
                               FileName<-paste(FileName[1],".RData",sep="")
                            } else {
                               FileName<-paste(FileName[1],".RData",sep="")
                               gmessage("Extension of the destination file forced to .RData!" , title = "DESTINATION FILE EXTENSION",  icon = "warning")
                            }
                            assign(activeFName,NewFName, envir=.GlobalEnv) #salvo il nuovo XPSSample nel GlobalEnvir
                            assign("activeFName",activeFName, envir=.GlobalEnv) #new XPSSample come attivo
                            assign("activeSpectName",CoreLineList[1], envir=.GlobalEnv) #prima coreline del new XPSSample come attiva
                            command=paste("save(", activeFName,", file='",FileName, "', compress=TRUE)", sep="")
                            eval(parse(text=command),envir=.GlobalEnv)
                            cat("\n Data saved in: ", FileName)
                            XPSSaveRetrieveBkp("save")

     }, container=T1group1)

     gbutton(" CLEAR SELECTIONS ", handler=function(h,...){
                           activeFName<-svalue(T1FNameListCK)
                           svalue(T1CoreLineCK1)<-FALSE   #resetto la lista delle coreline precedentemente selezionate
                           svalue(T1CoreLineCK2)<-FALSE   #resetto la lista delle coreline precedentemente selezionate
                           FName<-get(activeFName, envir=.GlobalEnv)
                           plot(FName)
     }, container=T1group1)

     gbutton(" EXIT ", handler=function(h,...){
    	                     dispose(Swin)
     }, container=T1group1)

     visible(Swin) <- TRUE
     Swin$set_modal(TRUE)

}



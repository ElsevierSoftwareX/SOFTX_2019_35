# Per recuperare e modificare le INFO dell'XPSSample
# revised October 2014

#'XPSSampleInfo to show/modify XPSSample INFOs is saved during acquisition
#'
#'@examples
#'
#'\dontrun{
#'	XPSSampleInfo()
#'}
#'
#'@export
#'


XPSSampleInfo <- function() {
   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
      activeFName<-get("activeFName", envir=.GlobalEnv)
      FName<-get(activeFName, envir=.GlobalEnv)
      CLineList<-XPSSpectList(activeFName)
      if (is.na(CLineList)){
         gmessage("ATTENTION NO CORELINES FOUND: please control your XPSSample datafile!" , title = "WARNING",  icon = "warning")
      }
      Data<-list(Project=NULL, Sample=NULL, Comments=NULL, User=NULL, names=NULL)
#---leggo le informazioni dell XPS Sample
      Data$Project<-FName@Project
      Data$Sample<-FName@Sample
      Data$Comments<-FName@Comments
      Data$User<-FName@User
      Data$names<-FName@names
      Data[[6]]<-"                                                  "  #aggiungo una riga di 50 caratteri per espandere il GDF()
      VarNames<-names(Data)
      newData<-Data
      Data <- data.frame(INFO=cbind(VarNames,Data), stringsAsFactors=FALSE) #gdf() non gestisce i nomi delle righe: aggiungo la colonna dei nomi

      DFwin <- gwindow(title="XPS-SAMPLE INFO", visible=FALSE) # accendo una finestra dove riporre eil dataframe
      size(DFwin)<-c(600,500)
      DFgroup <- ggroup(horizontal=FALSE, container=DFwin)
      txt<-paste("   ", activeFName, "  :   EDIT/CHANGE THE XPS-SAMPLE INFO", sep="")
      glabel(txt, container=DFgroup) #label in modo da estendere le dimensioni della finestra

      DFrame <- gdf(items=Data, container=DFgroup) #DFrame e' il puntatore a gdf()
      size(DFrame)<-c(600,150)

      addHandlerChanged(DFrame, handler=function(h,...){ #addHandlerChanged scarica il dataframe modificato in NewFirParam che e' salvato al di fuori di saveFitParam attraverso la <<-
                    newData <<- h$obj[]  #questa e' una lista con 2 colonne
      })

      SelectCL<-gradio(CLineList, selected=-1, horizontal=TRUE, container=DFgroup)
      addHandlerChanged(SelectCL, function(h, ...){
                    idx<-svalue(SelectCL, index=TRUE)
#                    txt<-array(dim=6)
                    txt<-NULL
                    txt[1]<-paste("Core Line : ",slot(FName[[idx]],"Symbol"),"\n", sep="")
                    txt[2]<-paste("N. data   : ",length(FName[[idx]]@.Data[[1]]))
                    txt[3]<-paste("baseline  : ",ifelse(hasBaseline(FName[[idx]]),"YES", "NO"),"\n", sep="")
                    txt[4]<-paste("fit       : ",ifelse(hasFit(FName[[idx]]),"YES", "NO"),"\n", sep="")
                    txt[5]<-paste("n. comp.  : ",ifelse(hasComponents(FName[[idx]]),length(FName[[idx]]@Components), "NONE"),"\n", sep="")
                    txt[6]<-(" Info:\n")
                    info<<-FName[[idx]]@Info
                    svalue(CLinfo)<-c(txt,info)
      })

      CLinfo<-gtext(container=DFgroup)
      size(CLinfo)<-c(600,280)

      DFlayout <- glayout(homogeneous=FALSE, spacing=5, container=DFgroup)
      DFlayout[2,1]<-gbutton(" SAVE & EXIT ", handler=function(h,...){
                      FName@Project<<-newData[[2]][1]    #salvo gli elementi della seconda colonna del data.frame
                      FName@Sample<<-newData[[2]][2]
                      FName@Comments<<-newData[[2]][3]
                      FName@User<<-newData[[2]][4]
                      FName@names<<-names(FName)         #per salvare correttamente i nomi delle Corelines
                      assign(activeFName, FName, envir=.GlobalEnv)
                      dispose(DFwin)
                      XPSSaveRetrieveBkp("save")
                  }, container=DFlayout)


      visible(DFwin)<-TRUE
}

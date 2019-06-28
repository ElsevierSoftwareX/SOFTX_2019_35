#Function to perform quantifications on XPS spectra
#The GUI allows the selection of corelines and indivisual spectral components
#for the computation of the atomic concentrations.
#XPSquantify and XPScalc used only by XPSMoveComponent.

#'Perform the elemental quantification for a selected XPS-Sample
#'
#'Provides a userfriendly interface with the list of Corelines of the selected XPS-Sample
#'Each Coreline can be count/omit from the elemental quantification.
#'If peak fitting is present also each of the fitting component can be count/omit from the
#'elemental quantification. 
#'Finally also relative RSF of the coreline or individual fitting components can be modified.
#'No parametrs are passed to this function.
#'
#'@examples
#'
#'\dontrun{
#'	XPSQuant()
#'}
#'
#'@export
#'

XPSQuant <- function(){

#XPScalc cannot be applieed since here you can select fit components and their RSF
   quant <- function(CoreLineComp){
      N_CL<-length(CoreLineComp)
      CoreLine<-names(CoreLineComp)
      CoreLine<-unlist(strsplit(CoreLine, "\\."))   #skip the number at coreline name beginning
      CoreLine<-CoreLine[2]
      maxFitComp<-0
      for(ii in 1:length(CoreLineComp)){
         LL=length(CoreLineComp[[ii]])
         if (LL > maxFitComp) { maxFitComp<-LL }
      }
      sumCoreLine<-rep(0,N_CL)
      sumAreaComp<-rep(0,N_CL)
      sumComp<-matrix(0,nrow=N_CL,ncol=maxFitComp)  #define a zero matrix
      AreaComp<-matrix(0,nrow=N_CL,ncol=maxFitComp)
      maxNchar<-0
      for(ii in 1:N_CL){
         sumCoreLine[ii]<-0
         indx<-CoreLineIndx[ii]
         if (svalue(CoreLineCK[[ii]])=="TRUE") {   #if a coreline is selected
            N_comp<-length(CoreLineComp[[ii]])      #this is the number of fit components
            RSF <- FName[[indx]]@RSF               #Sensitivity factor of the coreline
            E_stp <- abs(FName[[indx]]@.Data[[1]][2]-FName[[indx]]@.Data[[1]][1]) #energy step
            if (RSF != 0) {  #Sum is made only on components with RSF != 0 (Fit on Auger or VB not considered)
               sumAreaComp[ii]<-sum(FName[[indx]]@RegionToFit$y-FName[[indx]]@Baseline$y)*E_stp/RSF  #Coreline contribution corrected for the relative RSF
               sumCoreLine[ii]<-sum(FName[[indx]]@RegionToFit$y-FName[[indx]]@Baseline$y)*E_stp      #Integral undeer coreline spectrum
            } else {
               sumAreaComp[ii]<-sumCoreLine[ii]<-sum(FName[[indx]]@RegionToFit$y-FName[[indx]]@Baseline$y)*E_stp #if RSF not defined the integral under the coreline is considered
               sumCoreLine[ii]<-sumCoreLine[ii]<-sum(FName[[indx]]@RegionToFit$y-FName[[indx]]@Baseline$y)*E_stp #if RSF not defined the integral under the coreline is considered
            }
            txt<-as.character(round(sumCoreLine[ii], 2))

            if (hasComponents(FName[[indx]])) {   #is fit present on the coreline?
               for(jj in 1:N_comp){    #ii runs on CoreLines, jj runs on coreline fit components
                  comp<-CoreLineComp[[ii]][jj]
                  RSF<-FName[[indx]]@Components[[comp]]@rsf
                  if (RSF!=0) { #if the RSF is lacking(es. Auger, VB spectra...) : it is not possible to make correction for the RSF...
                     AreaComp[ii,jj]<-(sum(FName[[indx]]@Components[[comp]]@ycoor)-sum(FName[[indx]]@Baseline$y))*E_stp/RSF  #(area of the single component -  area backgroound) corrected for the RSF
                     sumComp[ii,jj]<-(sum(FName[[indx]]@Components[[comp]]@ycoor)-sum(FName[[indx]]@Baseline$y))*E_stp       #simple area of the coreline

                  } else {
                     AreaComp[ii,jj]<-(sum(FName[[indx]]@Components[[comp]]@ycoor)-sum(FName[[indx]]@Baseline$y))*E_stp   #se RSF non e' definito calcolo il semplice integrale della componente
                     sumComp[ii,jj]<-(sum(FName[[indx]]@Components[[comp]]@ycoor)-sum(FName[[indx]]@Baseline$y))*E_stp    #se RSF non e' definito calcolo il semplice integrale della componente
                  }
                  txt<-as.character(round(sumComp[ii,jj], 2))
                  Nch<-nchar(txt)
                  if (Nch>maxNchar) { maxNchar<-Nch } #calculate the max number of characters of numbers describing component areas
		            if (RSF != 0) {  #summation only on components with RSF !=0 (no fit on Auger o VB
                     sumAreaComp[ii]<-sum(AreaComp[ii,])
                     sumCoreLine[ii]<-sum(sumComp[ii,])
                  }
               }
            }
         }
      }

      AreaTot<-sum(sumAreaComp)
      sumTot<-sum(sumCoreLine)
      txt<-as.character(round(sumTot, 2))  #print original integral area witout RSF corrections
      maxNchar<-max(c(10,nchar(txt)+2))

      lgth<-c(10, maxNchar, 8, 8, 8, 9)    #width of table columns "Components", "Area", ", FWHM", "BE(eV)", "RSF", "TOT%"
      totLgth<-sum(lgth)+1
      cat("\n")

      txt<-paste("   File Name:", FName@Filename)  #Filename
      cell<-printCell("label",txt,CellB=" ",totLgth,"left")  #call cellprint in modality LABEL  alignment LEFT
      QTabTxt<-c(QTabTxt,cell,"\n") #add QTabTxt the information to compose the quant table
      TabTxt<-c(TabTxt,txt,"\n")
      txt<-"-"  #separator
      cell<-printCell("separator", "-","",totLgth,"left")     #call cellprint in modality SEPARATOR  alignment LEFT
      TabTxt<-c(TabTxt, cell)

      QTabTxt<-c(QTabTxt,"\n")
      TabTxt<-c(TabTxt,"\n")

      txt<-c("Components", "Area(cps)", "FWHM", "RSF", "BE(eV)", "TOT.(%)")
      cell<-printCell("tableRow", txt, CellB=" ", lgth, "center")
      QTabTxt<-c(QTabTxt,cell,"\n")
      TabTxt<-c(TabTxt,cell,"\n")
      cell<-printCell("separator", "-","",totLgth,"left")
      TabTxt<-c(TabTxt, cell)

      QTabTxt<-c(QTabTxt,"\n")
      TabTxt<-c(TabTxt,"\n")

  	   for(ii in 1:N_CL){
          indx<-CoreLineIndx[ii]
          if (svalue(CoreLineCK[[ii]])=="TRUE") {

              Component<-names(CoreLineComp[ii])
              Area<-sprintf("%1.2f", sumCoreLine[ii])  #round number to 2 decimals and trasform in string
              RSF<-sprintf("%1.3f",FName[[indx]]@RSF)
              Mpos<-NULL
              Mpos<-findMaxPos(FName[[indx]]@RegionToFit)    #Mpos[1]==position of spectrum max,    Mpos[2]==spectrum max value
              BE<-sprintf("%1.3f",Mpos[1])
              if (RSF=="0.000") { #RSF not defined (es. Auger, VB spectra...) : cannot make correction for RSF
                 Conc<-sprintf("%1.2f",0)
              } else {
                 Conc<-sprintf("%1.2f",100*sumAreaComp[ii]/AreaTot)
              }
 		        txt<-c(Component, Area, " ", RSF, BE, Conc )   #total concentration relative to the coreline: FWHM e BE not print
# 		        txt<-c(Component, Area, " ", " ", BE, Conc )
              cell<-printCell("tableRow", txt, CellB=" ", lgth, "center")
              QTabTxt<-c(QTabTxt,cell,"\n")                  #this QTabTxt will appear in the Quant widget
              cell<-printCell("tableRow", txt, CellB="|", lgth, "center")
              TabTxt<-c(TabTxt,cell,"\n")                    #this QTabTxt will appear in the R consolle

              if (hasComponents(FName[[indx]])) {
                 N_comp=length(CoreLineComp[[ii]]) #number of fit components
                 for(jj in 1:N_comp){ #ii runs on the corelines, jj runs on the fit components
                    comp<-CoreLineComp[[ii]][jj]
                    Area<-sprintf("%1.2f",sumComp[ii,jj]) #area of component jj coreline ii
                    FWHM<-sprintf("%1.2f",FName[[indx]]@Components[[comp]]@param[3,1]) #FWHM component ii
                    RSF<-sprintf("%1.3f",FName[[indx]]@Components[[comp]]@rsf) #RSF component ii
                    BE<-sprintf("%1.2f",FName[[indx]]@Components[[comp]]@param[2,1]) #BE component ii
                    if (RSF=="0.000") {
                       Conc<-sprintf("%1.2f",0)
                    } else {
                       Conc<-sprintf("%1.2f",100*AreaComp[ii,jj]/AreaTot)  #Concentration component ii
                    }
     		           txt<-c(comp, Area, FWHM, RSF, BE, Conc) #make string to print
                    cell<-printCell("tableRow", txt, CellB=" ", lgth, "center") #print string in modality TABLEROW
                    QTabTxt<-c(QTabTxt,cell,"\n")
                    cell<-printCell("tableRow", txt, CellB="|", lgth, "center") #print string in modality TABLEROW
                    TabTxt<-c(TabTxt,cell,"\n")
                 }
              }
              QTabTxt<-c(QTabTxt,"\n")
              TabTxt<-c(TabTxt,"\n")

          }
       }

       Font <- get("XPSSettings", envir=.GlobalEnv)[[1]][1]
       FStyle <- get("XPSSettings", envir=.GlobalEnv)[[1]][2]
       FSize <- get("XPSSettings", envir=.GlobalEnv)[[1]][3]
       svalue(QTable)<<-capture.output(cat("\n", QTabTxt))
       font(QTable) <- list(weight="light", family=Font, style=FStyle, size=FSize)
       cat("\n", TabTxt)
   }


   ResetComp <- function(ii){
       indx<-CoreLineIndx[ii]
       if (svalue(CoreLineCK[[ii]])=="FALSE") {   #if the coreline is not selected the fit component are disabled
          if (hasComponents(FName[[indx]])) {     #Does the coreline possess fitting components?
             svalue(ComponentCK[[ii]])<-""
          }
          names(CoreLineComp)[ii]<<-""
       }
       if (svalue(CoreLineCK[[ii]])=="TRUE") {    #if the coreline is selected all the fitting components are selected
          if (hasComponents(FName[[indx]])) {
             svalue(ComponentCK[[ii]])<-names(FName[[indx]]@Components)
          }
          names(CoreLineComp)[ii]<<-CoreLineNames[ii]  #reset coreline name: it could be dropped if all fitting components are cancelled
       }
   }

   SetRSF <- function(ii){
        kk<-1
        for(ii in 1:NCoreLines){
           Eq.RSF <- TRUE
           indx<-CoreLineIndx[ii]
           LL=length(unlist(OrigCoreLinComp[ii]))    #the list vectors may have different lengths (different N. Fit components)
           if (LL>0){                                #The RSF may be changed then control the RSF of all the fit components
                                                     #The un-selected components will be not considered in the quantification
              refRSF<-svalue(RSFCK[[kk]])            #set the referenceRSF as the first of the CorelineComponent
              for(jj in 1:LL){
                 newRSF<-svalue(RSFCK[[kk]])
                 if (newRSF != refRSF) { Eq.RSF <- FALSE } #at end of for Eq.RSF==TRUE means all RSF are equal
                 slot(FName[[indx]]@Components[[jj]], "rsf")<<-as.numeric(newRSF) #load the new RSFR in the relative slot of the XPSSample
                 kk<-kk+1
              }
           }
           if (Eq.RSF == TRUE){                      #if all Component RSF are equal set same value also in the Coreline RSF-Slot
              FName[[indx]]@RSF<<-as.numeric(refRSF)
           }
           if (LL == 0) {
              newRSF<-svalue(RSFCK[[kk]])
              FName[[indx]]@RSF<<-as.numeric(newRSF) #load the new RSFR in the relative slot of the XPSSample
              kk<-kk+1
           }
        }
   }


   CKHandlers <- function(){
         for(ii in 1:NCoreLines){
#----HANDLER on Widget-CoreLineCK to call ResetComp()
            if ((svalue(CoreLineCK[[ii]]) != CLChecked[ii]) && length(CLChecked[ii])>0) {
               CLChecked[ii]<<-svalue(CoreLineCK[[ii]])
               ResetComp(ii)     #set/reset fit components
            }
#----HANDLER on Widget-ComponentCK
            tmp1<-svalue(ComponentCK[[ii]])
            tmp2<-unlist(CoreLineComp[[ii]])
            if(svalue(CoreLineCK[[ii]])==FALSE && is.null(tmp2)==TRUE) {  #Coreline has only baseline is un-selected
               tmp1<-""
               tmp2<-""
               svalue(CoreLineCK[[ii]])<-FALSE
            } else if(length(tmp1)==0 && length(tmp2)>0) {  #Coreline having fit is un-selected
               tmp1<-""
               svalue(CoreLineCK[[ii]])<-FALSE
               CLChecked[ii]<<-FALSE
            } else {
               svalue(CoreLineCK[[ii]])<-TRUE
               CLChecked[ii]<<-TRUE
            }
            if(is.null(tmp2)=="TRUE") {
               tmp2<-""
            }  # the correspondent coreline does NOT possess fitting components but only the BaseLine
            if (all(tmp1==tmp2)==FALSE) {    #if the two vector contain different elements
               CoreLineComp[ii]<<-list(tmp1) #modify component [ii] of the list following the checkbox component selections
               if (nchar(tmp1)==0) {
                  names(CoreLineComp)[ii]<<-""
                  svalue(CLChecked[ii])<<-""
               }
               if (nchar(tmp1)>0) {
                  CoreLineComp<<-setNames(CoreLineComp, CoreLineNames)
               }
            }
         }

   }


#----MakeNb makes the notebook: a coreline for each notebook page
   MakeNb <- function(){
      kk<-1
      CoreLineCK<-list()     #define a list of Gwidget
      ComponentCK<-list()
      RSFCK<-list()
      for(ii in 1:NCoreLines){
          Qgroup[[ii]] <<- ggroup(label=CoreLineNames[ii], horizontal=TRUE, container=Qnb)
          Qlayout[[ii]] <<- glayout(homogeneous=FALSE, spacing=3, container=Qgroup[[ii]])
          NoComp<-"FALSE"
          indx<-CoreLineIndx[ii]
          tmp<-names(FName[[indx]]@Components)
          if (is.null(tmp)) { NoComp<-"TRUE" }
          LL<-length(tmp)    # the vector of the list may have different length (different N FitComp)
          CoreLineComp<<-c(CoreLineComp, list(tmp))   #create a list containing the fit components of corelines
          txt<- paste("CORE LINE", ii , sep="")

          Qlayout[[ii]][1,1] <<- CLframe[[ii]] <<- gframe(text=txt, horizontal=FALSE, spacing=5, container=Qlayout[[ii]])
          CoreLineCK[[ii]] <<- gcheckbox(CoreLineNames[ii], checked=CLChecked[ii], handler=function(h, ...){
                                             ii<-svalue(Qnb) #set the ii value to the active notebook page
                                             CKHandlers()
                                      }, container=CLframe[[ii]])  #here CoreLineName is an array

          Qlayout[[ii]][1,2] <<- CMPframe[[ii]] <- gframe(text="COMPONENTS", horizontal=FALSE, spacing=5, container=Qlayout[[ii]])
          ComponentCK[[ii]] <<- gcheckboxgroup(unlist(CoreLineComp[ii]), checked=TRUE, handler=function(h,...){
                                             ii<-svalue(Qnb) #set the ii value to the active notebook page
                                             CKHandlers()
                                      }, container=CMPframe[[ii]]) #here CoreLineName is an array

          Qlayout[[ii]][1,3] <<- RSFframe[[ii]] <- gframe(text="RSF", horizontal=FALSE, spacing=5, container=Qlayout[[ii]])
          if ( NoComp=="TRUE"){
             ComponentCK[[ii]]<<-""   #NO fitting components only baseline
             OldRSF<<-FName[[indx]]@RSF
             RSFCK[[kk]] <<- gedit(text=OldRSF, handler=NULL, container=RSFframe[[ii]])
             kk<-kk+1
          } else {
             for(jj in 1:LL){
                OldRSF<<-FName[[indx]]@Components[[jj]]@rsf
                RSFCK[[kk]] <<- gedit(text=OldRSF, handler=NULL, container=RSFframe[[ii]])
                kk<-kk+1
             }
          }
      }
      CoreLineComp<<-setNames(CoreLineComp, CoreLineNames)   #the list contains the names of the corelines and relative FitComponents
      OrigCoreLinComp<<-CoreLineComp

      ii<-NCoreLines+1
      Qgroup[[ii]] <<- ggroup(label=" QUANTIFY ", horizontal=FALSE, container=Qnb)

      Qlayout[[ii]] <<- glayout(homogeneous=FALSE, spacing=3, container=Qgroup[[ii]])
      Qlayout[[ii]][1,1] <<- gbutton(" QUANTIFY ", handler=function(h,...){        #glayout is generated in MakeNB()
                                          SetRSF()
                                          quant(CoreLineComp)
                                      }, container = Qlayout[[ii]])


      Qlayout[[ii]][1,2] <<- gbutton(" SAVE & EXIT ", handler=function(h,...){
                                           SetRSF()
                                           ActiveFName<-get("activeFName", envir=.GlobalEnv)
                                           assign(ActiveFName, FName, .GlobalEnv)  #save the fit parameters in the activeSample
                                           dispose(Qwin)
                                           XPSSaveRetrieveBkp("save")
                                     }, container = Qlayout[[ii]])


      QTable <<- gtext(text="", wrap=FALSE, container = Qgroup[[ii]])
      size(QTable) <<- c(550, 370)
      svalue(QTable)<-""
      for (ii in (NCoreLines+1):1){
          svalue(Qnb)<<-ii #refresh all pages until from the last to the first
      }

      return(CoreLineComp)
   }

#----Reset vars resets variables to initial values
   ResetVars <- function(){
      FName<<-get(activeFName, envir = .GlobalEnv)   #load the active XPSSample dataFrame
      ActiveFName<<-get("activeFName", envir = .GlobalEnv)  #load the name of the active XPSSample
      ActiveSpectIndx<<-get("activeSpectIndx", envir = .GlobalEnv)  #name of the active coreline
      FNameList<<-XPSFNameList()                     #list of all XPSSamples
      FNameIdx<<-grep(ActiveFName,FNameList)
      SpectList<<-XPSSpectList(ActiveFName)          #list of all CoreLines of the active XPSSample

      CoreLineNames<<-""
      CoreLineIndx<<-NULL
      FitComp<<-""
      NComp<<-NULL
      CoreLineComp<<-list()
      OrigCoreLinComp<<-list()
      CompChecked<<-NULL

      CoreLineCK<<-list()     #define a list of the Gwidget
      ComponentCK<-list()
      RSFCKv<-list()

      Qgroup<<-list()
      Qlayout<<-list()
      CLframe<<-list()
      CMPframe<<-list()
      RSFframev<<-list()
      QTable<<-list()

      NCoreLines<<-length(SpectList)
      NoFitFoundv<<-0
      NmaxFitCompv<<-0
      QTabTxt<<-""    #text containing Quantification results for the Qtable gtext()
      TabTxt<<-""     #text containing Quantification results for the RStudio consolle

      RegionToFit<-0
      jj<-1
      for(ii in 1:NCoreLines){
         if (length(FName[[ii]]@RegionToFit) > 0){ #a Baseline is defined
            RegionToFit <- 1
            CoreLineNames[jj]<<-SpectList[ii]  #Save the coreline name where a baseline is defined
            CoreLineIndx[jj]<<-ii              #vector containing indexes of the corelines where a baseline is defined
            jj<-jj+1
            NFC<-length(FName[[ii]]@Components)
            if (NFC > NmaxFitComp) {NmaxFitComp<<-NFC}
         }
      }
      if (RegionToFit == 0){
         gmessage(msg="WARNING: NO FIT REGIONS DEFINED ON THIS XPS SAMPLE", title = "WARNING", icon = "warning")
         return()
      }
      NCoreLines<<-length(CoreLineIndx) #now only the corelines with baseline are considered for the quantification
      CLChecked<<-rep("TRUE",NCoreLines)
   }

#---- variables ----
   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }

   FName<-NULL
   ActiveFName<-""
   FName<-get(activeFName, envir = .GlobalEnv)   #load the active XPSSample
   ActiveFName<-get("activeFName", envir = .GlobalEnv)  #load the XPSSample name
   ActiveSpectIndx<-get("activeSpectIndx", envir = .GlobalEnv)  #load the name of the active spectrum
   FNameList<-XPSFNameList()                     #list of all XPSSamples
   FNameIdx<-grep(ActiveFName,FNameList)
   SpectList<-XPSSpectList(ActiveFName)          #list of all the corelines of the active XPSSample
   CoreLineNames<-""
   CoreLineIndx<-NULL
   FitComp<-""
   NComp<-NULL
   CoreLineComp<-list()
   OrigCoreLinComp<-list()
   CompChecked<-NULL

   CoreLineCK<-list()     #define a list of the Gwidgets
   ComponentCK<-list()
   RSFCK<-list()

   Qgroup<-list()
   Qlayout<-list()
   CLframe<-list()
   CMPframe<-list()
   RSFframe<-list()
   QTable<<-list()

   NCoreLines<-length(SpectList)
   NoFitFound<-0
   NmaxFitComp<-0
   QTabTxt<-""    #text containing Quantification results for the Qtable gtext()
   TabTxt<-""     #text containing Quantification results for the RStudio consolle

   RegionToFit<-0
   jj<-1
   for(ii in 1:NCoreLines){
      if (length(FName[[ii]]@RegionToFit) > 0){ #a baseline is defined
         RegionToFit <- 1
         CoreLineNames[jj]<-SpectList[ii]  #Save the coreline name where a baseline is defined
         CoreLineIndx[jj]<-ii              #vector containing indexes of the corelines where a baseline is defined
         jj<-jj+1
         NFC<-length(FName[[ii]]@Components)
         if (NFC > NmaxFitComp) {NmaxFitComp<-NFC}
      }
   }
   if (RegionToFit == 0){
      gmessage(msg="WARNING: NO FIT REGIONS DEFINED ON THIS XPS SAMPLE", title = "WARNING", icon = "warning")
      return()
   }
   NCoreLines<-length(CoreLineIndx) #now only the corelines with baseline are considered for the quantification
   CLChecked<-rep("TRUE",NCoreLines)



#===== GUI =====

   Qwin <- gwindow(" QUANTIFICATION FUNCTION ", visible=FALSE)
   QmainGroup <-ggroup(horizontal=FALSE, container=Qwin)

   Qgroup0 <- ggroup(horizontal=TRUE, container=QmainGroup)
   glabel(text="Select XPS Data File", container=Qgroup0)
   SelXPSData <- gcombobox(FNameList, selected=FNameIdx, handler=function(h, ...){
                                            SelectedFName<-svalue(SelXPSData)
                                            FName<<-get(SelectedFName,envir=.GlobalEnv)  #carico in SampID il relativo XPSSAmple
                                            SpectList<<-XPSSpectList(SelectedFName)
                                            assign("activeFName", SelectedFName, envir=.GlobalEnv)
                                            assign("activeSpectIndx", 1, envir=.GlobalEnv)
                                            assign("activeSpectName", SpectList[1], envir=.GlobalEnv)
                                            ResetVars()
                                            delete(Qframe1,Qnb)
                                            Qnb <<- gnotebook(expand=TRUE, container = Qframe1)
                                            add(Qframe1,Qnb)
                                            CoreLineComp<<-MakeNb()
                                            plot(FName)
                                      }, container=Qgroup0)


   Qframe1 <- gframe(text="Core Lines", spacing=5,horizontal=FALSE, container=QmainGroup)
   Qnb <- gnotebook(expand=TRUE, container = Qframe1)
   CoreLineComp<-MakeNb()

   CoreLineComp<-setNames(CoreLineComp, CoreLineNames)   #the list contains the names of the coreline and the relative FitComponents
   OrigCoreLinComp<-CoreLineComp<<-CoreLineComp

   visible(Qwin) <- TRUE
   for(ii in (NCoreLines+1):1){   #reset nb pages from the last to the first
       svalue(Qnb)<-ii
   }

}

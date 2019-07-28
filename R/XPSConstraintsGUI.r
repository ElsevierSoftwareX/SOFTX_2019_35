#Function to edit/set/reset fit parameters:
# allowed function: FIX, Link fit parameters
#                   force the parameter to assume a given value respect to another (es. BE1 == 0.5+BE2)
# 
#function based on the XPSconstrain() function

#'GUI to add a fit constraints to the fitting functions of a XPSCoreLine
#'
#'Add constraints among fit components defined for a XPSCoreLine object. 
#'This function is called after the definition of the baseline (\code{XPSbaseline}) 
#'and fitting components (\code{XPSaddFitComponent}). No parameters are passed to
#'this function
#'@seealso \link{XPSconstrain}, \link{XPSbaseline}, \link{XPSaddFitComponent}, \link{XPSfitAlgorithms} 
#'
#'@examples
#'
#'\dontrun{
#'	XPSConstraints()
#'}
#'
#'@export
#'


XPSConstraints <- function(){

#---------- setCommand ----------
   setCommand <- function(h,...){
           Nc1<-as.integer(gsub("[^0-9]", "", component1))  #extract index from string component1
           Nc2<-as.integer(gsub("[^0-9]", "", component2))  #extract index from string component2
           switch(operation,
          "fix" = {
              FName[[SpectIndx]]<-XPSconstrain(FName[[SpectIndx]],Nc1,action=operation,variable=parameter,value=setValue, expr=NULL)
              if (parameter=="sigma") {
                 FName[[SpectIndx]]<-FixCtrl(FName[[SpectIndx]])  #controls that no other links present for ComponentToLink otherwise errors are generated
              }
           },
           "link" = {
              if (parameter=="sigma") {
#Attention: first all links have to be saved in SigmaCtrl
#then when pressing the button SAVE calls setlinks() and LinkCtrl() to control all links on sigma
#and then XPSconstrain() is executed to set the links and save informarion in the XPSSample
                 SigmaCtrl$FitComp<<-c(1:NComp)
                 SigmaCtrl$CompLnkd[Nc1]<<-Nc1 #save the linked component
                 SigmaCtrl$ToComp[Nc1]<<-Nc2   #save the linked-TO component
                 SigmaCtrl$Expression[Nc1]<<-linkExpression   #save the link expression
              } else {
                 value<-NULL
                 expression=paste(parameter,Nc2,linkExpression,sep="") #save linked component and link expression in the XPSSample
                 FName[[SpectIndx]]<<-XPSconstrain(FName[[SpectIndx]],Nc1,action="link",variable=parameter,value=value,expr=expression)
              }
           },
           "remove" = {
              if (length(Nc1)==0){
                 cat("\n Please specify the Fit component!\n")
                 return()
              }
              FName[[SpectIndx]]<-XPSconstrain(FName[[SpectIndx]],Nc1,action="remove",variable=NULL,value=NULL,expr=NULL)
              assign(ActiveFName, FName, envir=.GlobalEnv)
           },
           "edit" = {
              # do nothing !
           })

           #replot FitComponent with changed parameters and the Fit
           FName[[SpectIndx]]@Components[[Nc1]] <<- Ycomponent(FName[[SpectIndx]]@Components[[Nc1]], x=FName[[SpectIndx]]@RegionToFit$x, y=FName[[SpectIndx]]@Baseline$y)
	        tmp <- sapply(FName[[SpectIndx]]@Components, function(z) matrix(data=z@ycoor))  #fit is the sum of fitting components
	        FName[[SpectIndx]]@Fit$y <<- ( colSums(t(tmp)) - length(FName[[SpectIndx]]@Components)*(FName[[SpectIndx]]@Baseline$y)) #substract NComp*Baseline
	        FName[[SpectIndx]] <<- sortComponents(FName[[SpectIndx]])
           plot(FName[[SpectIndx]])
           Saved<<-FALSE
   }


#---------- SetLinks ----------

   SetLinks <- function() {    #SetLink is executed when SAVE button is pressed
             #reset previous links
             for (ii in 1:NComp){
                 LL<-length(FName[[SpectIndx]]@Components[[ii]]@link)
                 if (LL > 0) { #there is a link set on one of the fitting parameters
                    VarLnkd <- NULL
                    for (jj in 1:LL){ #scan the link slot: if the link is on sigma link will be reset
                        VarLnkd <- c(VarLnkd, FName[[SpectIndx]]@Components[[ii]]@link[[jj]]$variable) #vector containing the name of variable linked
                    }
                    jj <- grep("sigma", VarLnkd)
                    if (length(jj) > 0) {
                       FName[[SpectIndx]]@Components[[ii]]@link[[jj]] <- NULL #delete links on sigma
                    }
                 }
             }
             SigmaCtrl<-LinkCtrl(SigmaCtrl)  #controls on sigmas conditions
             SigmaCtrl<-na.omit(SigmaCtrl)   #eliminates NA from SigmaCtrl
             LL<-length(SigmaCtrl$CompLnkd)  #number of links on sigma
             for (ii in 1:LL) {
                  operation<-NULL
                  value<-NULL
                  Nc1<-SigmaCtrl$CompLnkd[ii]
                  Nc2<-SigmaCtrl$ToComp[ii]
                  if (! is.na(Nc1)){
                      linkExpr<-SigmaCtrl$Expression[ii]
                      linkExpr<-paste("sigma",Nc2,linkExpr,sep="") #there is also an operation on the linked component
                      FName[[SpectIndx]]<-XPSconstrain(FName[[SpectIndx]],Nc1,action="link",variable="sigma",value=value,expr=linkExpr)
                  }
             }
             cat("\n ==> Constraints saved!\n")
             FName[[SpectIndx]]<<-FName[[SpectIndx]]
   }


#---------- LinkCtrl ----------

   LinkCtrl <- function(SigmaCtrl) {    #LinkCtrl made when SAVE button pressed
      LinkedComp<-NULL
      LinkedComp<-cbind(SigmaCtrl$CompLnkd, SigmaCtrl$ToComp, SigmaCtrl$FitComp)  #the third column represents component indexes
      LinkedComp<-na.omit(LinkedComp)
      LL<-length(LinkedComp[,1])

      NComp<-length(SigmaCtrl$FitComp)
      LWrng<-NComp  #set length of WrongLinks to NComp
      NWrng<-NComp
      while(LWrng>0) {
         LinkedComp<-cbind(SigmaCtrl$CompLnkd, SigmaCtrl$ToComp, SigmaCtrl$FitComp)
         LinkedComp<-na.omit(LinkedComp)
         LL<-length(LinkedComp[,1])
#identification of Reference components: NO links present for them
         RefComp<-NULL   #RefComp is a vector of the indexes of Reference components
         jj=1
         for(ii in 1:NComp){
            if(is.na(SigmaCtrl$CompLnkd[ii])) {
               RefComp[jj]<-ii  #RefComp non linked components => SigmaCtrl$CompLnkd[ii]=NA
               jj=jj+1
            }
         }
         txt<-paste(paste("C", as.character(RefComp), sep=""), collapse=" ")
         cat("\n Found Non-Linked Components: ",txt)

#drop correctly linked components from wrong links
         NRef<-length(RefComp) #runs on the NON-linked components
         NLnks<-length(LinkedComp[,1]) #Number of links
         indxOK<-NULL
         for(ii in 1:NRef){    #this for runs on all the NON-linked components
             for(jj in NLnks:1){
                if(LinkedComp[jj,2]==RefComp[ii]) {
                   indxOK<-c(indxOK,jj)   #this are the indexes of correctly linked components
                }
             }
         }
         NWrng<-NLnks-length(indxOK)     #NWrng = NLinks - Links OK
         WrongLnks<-matrix(LinkedComp[-indxOK,], nrow=NWrng, ncol=3)
         LWrng<-length(WrongLnks)
         if (LWrng==0) {
             SigmaCtrl$CompLnkd<-na.omit(SigmaCtrl$CompLnkd)
             SigmaCtrl$ToComp<-na.omit(SigmaCtrl$ToComp)
             SigmaCtrl$FitComp<-na.omit(SigmaCtrl$FitComp)
             SigmaCtrl$Expression<-na.omit(SigmaCtrl$Expression)
             cat("\n ==> Link Ctrl Done!")
             break    #break while loop
         }
#Now control elements of LinkedComp which are linked to a FitComp which is are in turn linked to anothed FitComponent
         for(ii in 1:NWrng){
             for(jj in 1:NLnks){
                if(WrongLnks[ii,2]==LinkedComp[jj,1]) {
                   idx1<-WrongLnks[ii,3]                       #position of WrongLnk in SigmaCtrl
                   idx2<-LinkedComp[jj,3]
                   SigmaCtrl$ToComp[idx1]<-LinkedComp[jj,2]    #change the link to the correct FitComponent
                   if (nchar(SigmaCtrl$Expression[idx2])>0 && is.na(SigmaCtrl$Expression[idx2])==FALSE){
                      SigmaCtrl$Expression[idx1]<-SigmaCtrl$Expression[idx2]  #copy the operation present on the reference FitComponent to the linked component
                   }
                }
             }
         }
         LinkedComp<-NULL
      }
      return(SigmaCtrl)
   }


#---------- FixCtrl ----------

   FixCtrl <- function(Object) {
#Let us suppose a FIX contraint on sigmaC1 = 1.9eV = sigma reference component 1 is set. FixCtrl checks that
#all the linked sigma to the reference Comp1 have the same amplitude
#Since it is unknown if first the sigmaC1 is set to 1.9eV and then the links are created,
#the control the apmplitude of sigma of the other components be == sigmaC2 is made either if FIX and LINK 
#constraints are set

      NComponents<-length(Object@Components)
      CompIndx<-as.integer(gsub("[^0-9]", "", component1))   #index of the linked component
      SigC1<-paste("sigma", as.character(CompIndx), sep="")  #a string made of "sigmaXX" where XX is the index of the ReferenceComp.
      SigC1Start<-Object@Components[[CompIndx]]@param$start[3]   #get the value of start, min, max to make them equal to those of ReferenceComp. C1
      SigC1Min<-Object@Components[[CompIndx]]@param$min[3]
      SigC1Max<-Object@Components[[CompIndx]]@param$max[3]
      for (ii in 1:NComponents){
          if (length(Object@Components[[ii]]@link)>0) {
              if (Object@Components[[ii]]@link[[1]]$expr == SigC1) { #here control if the link is to ReferenceComp. == C1
                  Object@Components[[ii]]@param$start[3]<-SigC1Start #make values equal to those of ReferenceComp.
                  Object@Components[[ii]]@param$min[3]<-SigC1Min
                  Object@Components[[ii]]@param$max[3]<-SigC1Max
              }
          }
     }
     return(Object)
}


#---------- editFitFrame ----------

   editFitFrame <- function(h,...){
      selectedComp<-svalue(T1obj1)
      CompIndx<-as.integer(gsub("[^0-9]", "", selectedComp))
      fitParam<-FName[[SpectIndx]]@Components[[CompIndx]]@param #load DataFrame relative to the selected component
      VarNames<-rownames(fitParam)
      fitParam<-as.matrix(fitParam) #this is needed to construct correctly the data.frame
      fitParam <- data.frame(cbind(VarNames,fitParam), stringsAsFactors=FALSE) #in the dataframe add a column with variable names
      newFitParam<<-fitParam
      Label=paste("C", CompIndx, "- COMPONENT FIT PARAMETERS")
      DFwin <- gwindow(title=Label, visible=FALSE) # open a window to edit the dataframe
      DFgroup <- ggroup(horizontal=FALSE, container=DFwin)
      glabel("                             EDIT FIT PARAMETERS                                     ", container=DFgroup)
      DFrame <- gdf(items=fitParam, container=DFgroup)
      size(DFrame)<-c(400,150)
      addHandlerChanged(DFrame, handler=function(h,...){ #addHandlerChanged dowload the dataFrame with modified parameters in NewFirParam (global variable)
         newFitParam <<- h$obj[]
      })

      gbutton("     SAVE AND EXIT      ", handler=function(h,...){

                newFP <- lapply(newFitParam[,2:ncol(newFitParam)], function(x) {as.numeric(x)} ) #the dataframe contais strings
                fitParam<-fitParam[,-1]   #remove labels introduced in the first column of the DataFrame
                fitParam[, 1:ncol(fitParam)]<-newFP   #this operation preserves the class(fitParam)=data.base nneded to save parameters in the relative slot of XPSSSample
                slot(FName[[SpectIndx]]@Components[[CompIndx]], "param")<-fitParam #save parameters in the slot of XPSSample

                FName[[SpectIndx]]<<-FName[[SpectIndx]]
                operation<<-"edit"
                component1<<-selectedComp
                parameter<<-NULL
                dispose(DFwin)
                setCommand()    #only to replot the new fit
                XPSSaveRetrieveBkp("save")
                return()
             }, container = DFgroup)
      visible(DFwin) <- TRUE
   }



#===== variabili =====
   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   ActiveFName <- activeFName #Save actual activeFName to save data in the correct XPSSample (activeFName could change)
   FName<-get(ActiveFName,envir=.GlobalEnv)
   SpectIndx<-get("activeSpectIndx", envir=.GlobalEnv)
   SpectName<-get("activeSpectName", envir=.GlobalEnv)
   SpectList<-XPSSpectList(ActiveFName)
   NComp<-length(FName[[SpectIndx]]@Components)

   FitComp1<-names(FName[[SpectIndx]]@Components)
   FitComp2<-""
   fitParam<-NULL
   newFitParam<-NULL
   ParamList<-""

   operation<-""
   parameter<-""
   linkExpression<-""
   setValue<-""
   component1<-""
   component2<-""
   NewParam<-""
   NewRSF<-NULL
   Saved<-FALSE
   SigmaCtrl<-list(FitComp=c(1:NComp), CompLnkd=NULL, ToComp=NULL, Expression=NULL)

   plot(FName[[SpectIndx]])


#---- Ctrl on the active coreline if fit is present or selection of a new core line.

     if (NComp ==0){
        WW <- gwindow("NO FIT FOUND", visible=FALSE)
 		  GG <- ggroup(horizontal=FALSE, container=WW)
        txt <- paste("ATTENTION: no fit found in ", activeSpectName, "Change core line please!")
        GL<-glabel(txt, container=GG)
        font(GL) <- list(weight="medium", size=6)
        gseparator(horizontal = TRUE, container = GG)
        GF <- gframe("SELECT CORE LINE", horizontal=FALSE, spacing=1, container=GG)
        CB<-gcombobox(SpectList, selected=-1, width=3, handler = function(h, ...){
            SourceCoreline<-svalue(CB)
            SourceCoreline<-unlist(strsplit(SourceCoreline, "\\."))
            SpectName<<-SourceCoreline[2]
            SpectIndx<<-as.integer(SourceCoreline[1])
            NComp<<-length(FName[[SpectIndx]]@Components)
            FitComp1<<-names(FName[[SpectIndx]]@Components)
            plot(FName[[SpectIndx]])
        }, container=GF)
		  gbutton("  OK  ", handler=function(...){
		      if (NComp==0){ return() }
		      SpectName<<-names(FName)[SpectIndx]
		      assign("activeSpectIndx", SpectIndx, envir=.GlobalEnv)
		      assign("activeSpectName", SpectName, envir=.GlobalEnv)
		      dispose(WW)
        }, container=GG)
        visible(WW)<-TRUE
        WW$set_modal(TRUE)  #wait until selection done
      }
      mainFCwin <- gwindow("FIT PARAMETER CONSTRAINTS", parent=c(30,30), visible=TRUE)
      size(mainFCwin)<- c(570,320)
      maingroup <- ggroup(horizontal=TRUE, container=mainFCwin)
      NBframe <- gframe(text=" CONSTRAINTS ", spacing=5,horizontal=FALSE, container=maingroup)

#----- NOTEBOOK PER LA GESTIONE DEI CONSTRAINTS -----

      nb <- gnotebook(expand=TRUE, container = NBframe)

# --- Tab1 - EDIT ---
      T1group1 <- ggroup(label="EDIT FIT PARAMETERS", horizontal=FALSE, container=nb)
      layoutT1 <- glayout(homogeneous=FALSE, spacing=3, container=T1group1)

      layoutT1[1,1] <- T1frame1 <- gframe(" SELECT COMPONENT TO EDIT", spacing=5, container=layoutT1)
      T1obj1 <- gcombobox(FitComp1, selected=-1, editable=FALSE, handler=editFitFrame, container=T1frame1)
      layoutT1[2,1] <- glabel("          ", spacing=5, container=layoutT1)

      layoutT1[3,1] <- gseparator(horizontal = TRUE, container=layoutT1)
      layoutT1[3,2] <- gseparator(horizontal = TRUE, container=layoutT1)
      layoutT1[4,1] <- T1frame2 <- gframe(" SELECT COMPONENT ", spacing=5, container=layoutT1)

      HndlrT1obj2<-function(){  #external handler because called also when notebook pages are reset
                       FitComp<-svalue(T1obj2)
                       CompIndx<-as.integer(gsub("[^0-9]", "", FitComp))
                       OldRSF<-FName[[SpectIndx]]@Components[[CompIndx]]@rsf
                       svalue(SetRSF) <- OldRSF
                       addHandlerChanged(SetRSF, handler=function(h,...){
                               NewRSF<<-svalue(SetRSF)
                               if (NewRSF != ""){
                                   NewRSF<<-as.numeric(NewRSF)
                                   svalue(SetRSF)<-""
                               }
                      })
                 }
      T1obj2 <- gcombobox(FitComp1, selected=-1, editable=FALSE, handler=function(h,...){HndlrT1obj2()}, container=T1frame2)

      layoutT1[4,2] <- T1frame3 <- gframe(" CHANGE RSF ", spacing=5, container=layoutT1)
      SetRSF<- gedit(selected=-1, handler=NULL, container=T1frame3)

      gbutton(" SAVE RSF ", handler=function(h,...){
                       FitComp<-svalue(T1obj2)
                       CompIndx<-as.integer(gsub("[^0-9]", "", FitComp))
                       slot(FName[[SpectIndx]]@Components[[CompIndx]], "rsf")<-NewRSF #load new parameter in the XPSSample slot
                       FName[[SpectIndx]]<<-FName[[SpectIndx]]
                 }, container=T1group1)

      gbutton(" REMOVE COMPONENT CONSTRAINTS ", handler=function(h,...){
                       operation<<-"remove"
                       component1<<-svalue(T2obj1)
                       setCommand()
                 }, container = T1group1)

      gbutton(" REMOVE ALL CONSTRAINTS ", handler=function(h,...){
                       SigmaCtrl<<-list(FitComp=c(1:NComp), CompLnkd=NULL, ToComp=NULL, Expression=NULL)
                       operation<<-"remove"
                       for (ii in 1:NComp){
                           component1<<-FitComp1[ii]
                           setCommand()
                       }
                 },container = T1group1)


# --- Tab2 - FIX ---

      T2group1 <- ggroup(label=" FIX / SET ", horizontal=FALSE, container=nb)
      layoutT2 <- glayout(homogeneous=FALSE, spacing=3, container=T2group1)

      layoutT2[1,1] <- T2frame1 <- gframe(text=" SELECT COMPONENT ", spacing=5, container=layoutT2)

      HndlrT2obj1 <- function(){ #external handler because called also when notebook pages are reset
                     component1<-svalue(T2obj1)   #componente scelta
                     CompIndx<-as.integer(gsub("[^0-9]", "", component1))  #indice della componente
                     ParamList<-rownames(FName[[SpectIndx]]@Components[[CompIndx]]@param) #carico il DataFrame relativo alla componente selezionata nel Radiobox

                     delete(T2frame2, T2obj2)   #per mantenere l'ordine degli oggetti devo cancellare anche il bottone
                     T2obj2 <<- gcombobox(ParamList, selected=-1, editable=FALSE, handler=NULL, container=T2frame2)
                     addHandlerChanged(T2obj2,handler=function(h,...){
                     #l'handler va definito qui poiche' T2obj2 viene rigenerato e non corrisponde a quello originario
                                        component1<<-svalue(T2obj1)   #componente scelta
                                        CompIndx<-as.integer(gsub("[^0-9]", "", component1))  #indice della componente
                                        parameter<-svalue(T2obj2)
                                        OldValue<-FName[[SpectIndx]]@Components[[CompIndx]]@param[parameter,"start"] # valore attuale del parametro
                                        NewParam<<-round(as.numeric(OldValue), digits=2)
                                        svalue(T2obj3)<-OldValue
                     })
      }

      T2obj1 <- gcombobox(FitComp1, selected=-1, editable=FALSE, handler=function(h,...){HndlrT2obj1()}, container=T2frame1)


      layoutT2[2,1] <- T2frame2 <- gframe(text=" PARAMETER TO FIX / SET ", spacing=5, container=layoutT2)
      T2obj2 <- gcombobox(ParamList, selected=-1, editable=FALSE, handler=NULL, container=T2frame2)

      layoutT2[3,1] <- T2frame3 <-gframe(text=" VALUE TO SET  ", spacing=5, container=layoutT2)
      T2obj3<- gedit(, selected=-1, handler=function(h,...){
                     NewParam<-svalue(T2obj3)
                     if (NewParam != ""){
                        NewParam<<-as.numeric(NewParam)
                     }
                  }, container=T2frame3)

      gbutton(" SET CONSTRAINT ", handler=function(h,...){
                       component1<<-svalue(T2obj1)
                       component2<<-"NULL"
                       CompIndx<-as.integer(CompIndx <- gsub("[^0-9]", "", component1))  #component index
                       parameter<-svalue(T2obj2)
                       ParamIndx<-svalue(T2obj2, index=TRUE)
                       operation<<-"fix"     #operation=set only when Gedit is used, otherwise it is forced to FIX when a parameter is selected (see handler T2obj1)
                       setValue<<-NewParam
                       parameter<<-parameter
                       linkExpression<<-""
                       component1<<-component1
                       component2<<-component2
                       setCommand()
                 }, container = T2group1)

      gbutton(" REMOVE COMPONENT CONSTRAINTS ", handler=function(h,...){
                       operation<<-"remove"
                       component1<<-svalue(T2obj1)
                       setCommand()
                 }, container = T2group1)

      gbutton(" REMOVE ALL CONSTRAINTS ", handler=function(h,...){
                       SigmaCtrl<<-list(FitComp=c(1:NComp), CompLnkd=NULL, ToComp=NULL, Expression=NULL)
                       operation<<-"remove"
                       for (ii in 1:NComp){
                           component1<<-FitComp1[ii]
                           setCommand()
                       }



                 },container = T2group1)

# --- Tab3 - LINK ---

      T3group1 <- ggroup(label=" LINK ", horizontal=FALSE, container=nb)
      layoutT3 <- glayout(homogeneous=FALSE, spacing=3, container=T3group1)

      layoutT3[1,1] <- T3frame1 <- gframe(" SELECT COMPONENT ", spacing=5, container=layoutT3)

      HndlrT3obj1 <- function(){ #external handler because called also when notebook pages are reset
                       linkExpression<<-""
                       component<-svalue(T3obj1)   #selected component
                       CompIndx<-as.integer(gsub("[^0-9]", "", component))  #component index
                       ParamList<-rownames(FName[[SpectIndx]]@Components[[CompIndx]]@param) #load the parameter dataframe of the selected component
                       delete(T3frame2, T3obj2)
                       T3obj2<<- gcombobox(ParamList, selected=-1, handler=function(h,...){
                                           svalue(T3obj4) <- ""    #when change parameter remove links
                                       }, container=T3frame2)

                       FitComp2<<-FitComp1[-CompIndx]
                       delete(T3frame3, T3obj3)
                       T3obj3 <<- gcombobox(FitComp2, selected=-1, handler=function(h,...){
                                           svalue(T3obj4) <- ""    #when change parameter remove links
                                       }, container=T3frame3)
                       svalue(T3obj4) <- ""
      }
      T3obj1 <- gcombobox(FitComp1, selected=-1, editable=FALSE, handler=function(h,...){HndlrT3obj1()}, container=T3frame1)

      layoutT3[2,1] <- T3frame2 <-gframe(text=" PARAMETER to link ", spacing=5, horizontal=TRUE, container=layoutT3)
      T3obj2<- gcombobox(ParamList<-"", selected=-1, editable=FALSE, handler=function(h,...){
                                    svalue(T3obj4) <- ""    #when change parameter remove links
                      }, container=T3frame2)

      layoutT3[3,1] <- T3frame3 <- gframe(text=" LINK TO COMPONENT", spacing=5, container=layoutT3)
      T3obj3 <- gcombobox(FitComp2, selected=-1, editable=FALSE, handler=function(h,...){
                                    svalue(T3obj4) <- ""    #when change parameter remove links
                      }, container=T3frame3)

      layoutT3[3,2] <- T3frame4 <-gframe(text="LINK EXPRESSION:  *,  -, +  x.xx", spacing=5, container=layoutT3)

      T3obj4<- gedit(selected=-1, container=T3frame4)

      gbutton(" SET CONSTRAINT ", handler=function(h,...){
                       operation<<-"link"
                       component1<<-svalue(T3obj1)
                       parameter<<-svalue(T3obj2)
                       component2<<-svalue(T3obj3)
                       linkExpression<<-svalue(T3obj4)
                       setCommand()
                 },container = T3group1)

      gbutton(" REMOVE COMPONENT CONSTRAINTS ", handler=function(h,...){
                       operation<<-"remove"
                       component1<<-svalue(T3obj1)
                       setCommand()
                 },container = T3group1)

      gbutton(" REMOVE ALL CONSTRAINTS ", handler=function(h,...){
                       SigmaCtrl<<-list(FitComp=c(1:NComp), CompLnkd=NULL, ToComp=NULL, Expression=NULL)
                       operation<<-"remove"
                       for (ii in 1:NComp){
                           component1<<-FitComp1[ii]
                           setCommand()
                       }
                 },container = T3group1)

# --- End of notebook ---


# --- Page in common ---
      Bframe <- gframe(text="OPTIONS", spacing=5,horizontal=FALSE, container=maingroup)

      CLframe<-gframe(text="SELECT CORELINE", spacing=5,horizontal=FALSE, container=Bframe)
      CLobj1 <- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h,...){
                          Saved<<-FALSE
                          XPSComponent<-svalue(CLobj1)
                          XPSComponent<-unlist(strsplit(XPSComponent, "\\."))   #remove number at CoreLine beginning
                          SpectIndx<<-as.numeric(XPSComponent[1])
                          SpectName<<-XPSComponent[2]
                          assign("activeSpectName", SpectName,envir=.GlobalEnv) #set the active XPSSample be the lasr loaded file
                          assign("activeSpectIndx", SpectIndx,envir=.GlobalEnv) #set the active spectrum index
                          FName<<-get(ActiveFName,envir=.GlobalEnv)
                          FitComp1<<-names(FName[[SpectIndx]]@Components)
                          NComp<<-length(FName[[SpectIndx]]@Components)
                          plot(FName[[SpectIndx]])
#--- Reset Notebook pages ---

#--- pag1
                          delete(T1frame1,T1obj1)
                          T1obj1 <<- gcombobox(FitComp1, selected=-1, editable=FALSE, handler=editFitFrame, container=T1frame1)
                          delete(T1frame2,T1obj2)
                          T1obj2 <<- gcombobox(FitComp1, selected=-1, editable=FALSE, handler=function(h,...){HndlrT1obj2()}, container=T1frame2)
                          svalue(SetRSF)<<-""
#--- pag2
                          delete(T2frame1,T2obj1)
                          T2obj1 <<- gcombobox(FitComp1, selected=-1, editable=FALSE, handler=function(h,...){HndlrT2obj1()}, container=T2frame1)
                          svalue(T2obj2)<<-""
                          svalue(T2obj3)<<-""
#--- pag3
                          delete(T3frame1,T3obj1)
                          T3obj1 <<- gcombobox(FitComp1, selected=-1, editable=FALSE, handler=function(h,...){HndlrT3obj1()}, container=T3frame1)
                          svalue(T3obj2)<<-""
                          svalue(T3obj3)<<-""
                          svalue(T3obj4)<<-""
#--- reset parameters
                          operation<<-""
                          parameter<<-""
                          linkExpression<<-""
                          setValue<<-""
                          component1<<-""
                          component2<<-""
                          NewParam<<-""
                          SigmaCtrl<<-list(FitComp=NULL, CompLnkd=NULL, ToComp=NULL, Expression=NULL)
                          fitParam<<-NULL
                          ParamList<<-""

                     }, container = CLframe)

      gbutton(" FIT Lev.Marq. ", handler=function(h,...){
                          if (Saved){
                             FName[[SpectIndx]]<<-XPSFitLM(FName[[SpectIndx]])
                             plot(FName[[SpectIndx]])
                          } else {
                             gmessage(msg="Please save the Constraints before running the fit", title="WARNING!", icon = "warning")
                          }
                     }, container = Bframe)

      gbutton(" FIT ModFit ", handler=function(h,...){
                          if (Saved){
                             FName[[SpectIndx]]<<-XPSModFit(FName[[SpectIndx]])
                             plot(FName[[SpectIndx]])
                             FName[[SpectIndx]]<<-FName[[SpectIndx]]
                          } else {
                             gmessage(msg="Please save the Constraints before running the fit", title="WARNING!", icon = "warning")
                          }
                     }, container = Bframe)

      gbutton(" SAVE ", handler=function(h,...){
                          if(length(SigmaCtrl$CompLnkd) > 0 && Saved == FALSE) { #there are links on sigma still not controlled
                             SetLinks()                        #first all the links have to be set then they can be controlled and set!!!
                          }
                          SpectName<<-names(FName)[SpectIndx]  #name of the active CoreLine
                          assign(ActiveFName, FName, envir=.GlobalEnv)
                          assign("activeSpectName", SpectName, envir=.GlobalEnv)
                          assign("activeSpectIndx", SpectIndx, envir=.GlobalEnv)
                          Saved<<-TRUE
                          plot(FName[[SpectIndx]])
                          XPSSaveRetrieveBkp("save")
                     },container=Bframe)

      gbutton(" RE-LOAD DATA ", handler=function(h,...){
                          FName<<-get(ActiveFName, envir=.GlobalEnv)
                          Saved<<-FALSE
                          plot(FName[[SpectIndx]])
                     },container=Bframe)

      gbutton(" EXIT ", handler=function(h,...){
                          if (Saved){
                             dispose(mainFCwin)
                          } else {
                             if (gconfirm(msg="Data NOT saved! Do you want to exit?", title="WARNING!", icon="warning",)) {
                                  dispose(mainFCwin)
                             }
                          }
                          XPSSaveRetrieveBkp("save")
                     },container=Bframe)

      visible(mainFCwin)<-TRUE
      svalue(nb) <- 3

} #end of constraints

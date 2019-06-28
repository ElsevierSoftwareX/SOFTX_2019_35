#function to save XPS dataframes analyzed by XPS program

#'To Save data in the Hard Disk
#'
#'Provides a userfriendly interface to select a FileName and Directory
#'where to save the analyzed XPS-Sample data.
#'Analyzed data by default have extension .Rdata.
#'No parameters are passed to this function
#'@examples
#'
#'\dontrun{
#'	XPSSaveData()
#'}
#'
#'@export
#'


XPSSaveData <- function() {

     ChDir<-function(){
          workingDir<-getwd()
          PathName <<- tk_choose.dir(default=workingDir)
          PathName <<- paste(dirname(PathName), "/", basename(PathName), sep="") #cambia i separatori da \\ a /
     }

     CutPathName<-function(PathName){
          if (nchar(PathName) > 40){
             splitPathName<-strsplit(PathName, "/")
             LL<-nchar(PathName[[1]])
             HeadPathName <<- paste(splitPathName[[1]][1],"/", splitPathName[[1]][2], "/ ... ", sep="") 
             ShortPathName<-paste(HeadPathName, substr(PathName, (LL-30), LL), sep="")
             return(ShortPathName)
          }
          return(PathName)

     }

     SaveSingle<- function(){
          PathName <<- paste(dirname(PathName), "/", basename(PathName), sep="") #change separators from \\ in /
          saveFName <<- unlist(strsplit(saveFName, "\\."))
          saveFName <<- paste(saveFName[1],".RData", sep="")  #Define the Filename to be used to save the XPSSample
          FName<-get(activeFName, envir=.GlobalEnv)
          FName@Filename<-saveFName #save the new FileName in the relative XPSSample slot
          PathFileName<-paste(PathName,"/",saveFName[1], sep="")

          assign(saveFName, FName, envir=.GlobalEnv)  #save the xxx.Rdata XPSSample in the .GlobalEnv
          save(list=saveFName, file=PathFileName, compress=TRUE)
          removeFName<-unlist(strsplit(activeFName, "\\."))   #in activeFName are initially .vms or .pxt or OldScienta fileNames
          if (removeFName[2] != "RData"){
             remove(list=activeFName,pos=1,envir=.GlobalEnv)  #xxx.Rdata is saved in .GlobalEnv Now remove xxx.vms, xxx.pxt
          }
          assign("activeFName", saveFName, envir=.GlobalEnv)  #change the activeFName in the .GlobalEnv
          ShortPathName <- CutPathName(PathFileName)
          txt<-paste("\n Analyzed Data saved in: ", ShortPathName, sep="")
          svalue(message)<-txt
          cat("\n", txt)
          XPSSaveRetrieveBkp("save")
     }

     SaveAll<- function(){
          PathName <<- paste(dirname(PathName), "/", basename(PathName), sep="") #change separators from \\ in /
          FNameList<-XPSFNameList()
          LL=length(FNameList)
          for(jj in 1:LL){
              saveFName <<- unlist(strsplit(FNameList[jj], "\\."))
              saveFName <<- paste(saveFName[1],".RData", sep="")  #Define the Filename to be used to save the XPSSample
              FName<-get(FNameList[jj], envir=.GlobalEnv)
              FName@Filename<-saveFName ##save the new FileName in the relative XPSSample slot
              PathFileName<-paste(PathName,"/",saveFName, sep="")

              assign(saveFName, FName, envir=.GlobalEnv)  #save the xxx.Rdata XPSSample in the .GlobalEnv
              save(list=saveFName, file=PathFileName, compress=TRUE)
              removeFName<-unlist(strsplit(FNameList[jj], "\\."))  #in FNameList are initially .vms or .pxt or OldScienta fileNames
              if (removeFName[2] != "RData"){
                remove(list=FNameList[jj],pos=1,envir=.GlobalEnv)  #xxx.Rdata is saved in .GlobalEnv Now remove xxx.vms, xxx.pxt
              }
              if (FNameList[jj] == activeFName){
                 assign("activeFName", saveFName, envir=.GlobalEnv)  #change the activeFName in the .GlobalEnv
              }
              ShortFName<-CutPathName(PathFileName)
              txt<-paste("\n Analyzed Data saved in: ", ShortFName, sep="")
              cat("\n", txt)
          }
          ShortPathName<-CutPathName(PathName)
          txt<-paste("\n Analyzed Data saved in directory: ", ShortPathName, sep="") 
          svalue(message)<-txt
          XPSSaveRetrieveBkp("save")
     }

     GroupAndSave<- function(){
          if (saveFName == "") {
             gmessage(msg="NO FILE NAME TO SAVE DATA" , title = "Saving Data",  icon = "warning")
             return()
          }
          saveFName <<- unlist(strsplit(saveFName, "\\."))
          saveFName <<- paste(PathName,"/",saveFName[1],".RData", sep="")
          FNameList<-XPSFNameList()
          save(list=FNameList, file=saveFName, compress=TRUE)
          ShortFName<-CutPathName(saveFName)
          txt<-paste("\n Analyzed Data saved in: ", ShortFName, sep="")
          svalue(message)<-txt
          cat("\n", txt)
          XPSSaveRetrieveBkp("save")
     }


#===== Setting Variables =====
   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   saveFName <- ""
   PathName <- ""
   if (exists("WDirPath", envir=.GlobalEnv) ){
      PathName<-get("WDirPath", envir=.GlobalEnv)   #get the last path used to load data
   } else {
      PathName<-choose.dir()
      setwd(PathName)
   }

#===== Command Window =====
   win <- gwindow("SAVE XPS-SAMPLE DATAFILE", visible=FALSE)

   group1 <- ggroup(label="SAVE DATA", horizontal=FALSE, container=win)
   frame1 <- gframe(" Change Directory ", spacing=5, horizontal=FALSE, container=group1)

   txt<-paste("Current Dir: ", PathName, sep="")
   obj1<-glabel(txt, container=frame1)
   gbutton("  Change Directory   ", handler=function(h,...){
                      ChDir()
                      svalue(obj1)<-PathName
                   },container=frame1)

   frame2 <- gframe("File Name (.RData)", spacing=5, horizontal=FALSE, container=group1)
   obj2<-gedit(text="", container=frame2)
   saveFName <- get("activeFName", envir=.GlobalEnv)
   saveFName <-unlist(strsplit(saveFName, "\\."))
   saveFName <- paste(saveFName[1], ".Rdata", sep="")  #Compose the new FileName
   svalue(obj2)<-saveFName
   addHandlerChanged(obj2, handler=function(h,...){
                     saveFName <<- svalue(obj2)  #the name can be edited and modified
                   })

   gbutton("   Save Active XPS-Sample   ", handler=function(h,...){ SaveSingle() },container=frame2)

   frame3 <- gframe(" All Analyzed Data Separated", spacing=5, horizontal=FALSE, container=group1)
   gbutton("   Save All XPS-Samples   ", handler=function(h,...){ SaveAll() },container=frame3)

   frame4 <- gframe(" Group Analyzed Data ", spacing=5, horizontal=FALSE, container=group1)
   obj4<-gedit(text="", container=frame4)
   addHandlerChanged(obj4, handler=function(h,...){
                     saveFName <<- svalue(obj4)
                   })
   gbutton(" Group XPS-SAmples and Save (.RData) ", handler=function(h,...){ GroupAndSave() },container=frame4)

   message <- glabel("", container=group1)
   assign("S.message", message, envir=MyEnv)

   gbutton("          Exit           ", handler=function(h,...){
                     rm(list = ls(envir = MyEnv, all.names = TRUE), envir = MyEnv)
                     dispose(win)
                     return(1)
          },container=group1)

   visible(win) <- TRUE
   win$set_modal(TRUE)

}

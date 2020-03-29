require(reader)
require(tcltk)
min_first=c()
max_first=c()
file_suffix= "_reversed.csv"



detect_version=function(fn)
{
  #  first, detect whether this is Mormyroscope version 1 file or version 2 by seeing if metadata is separated by "="
  firstline = read.csv(fn,header = FALSE,nrows = 1,sep = "=") # get first line of file using the "=" character as the separator
  if(length(firstline)==2) 
  {
    mscope_ver <- 1
  }
  else
  {
    mscope_ver <-  2
  }  # set Mscope_ver to 1 or 2 depending on presence of "=".
  mscope_ver
}

get_metadata=function(p_file, p_vers)
{
  if(p_vers==1)
  {
    skip <-  12
    meta_delim<-"="
  }
  else if(p_vers==2)
  {
    skip <-  14
    meta_delim<-"\t"
  }
  #read metadata
  meta<-read.csv(p_file,header=FALSE,nrows=skip,sep = meta_delim)  #read meta data 
  #lsView(meta)
  meta
  
}

get_signal=function(p_file, p_vers)
{
  if(p_vers==1)
  {
    skip_lines <-  12
    
  }
  else if(p_vers==2)
  {
    skip_lines <-  14
  }
  #read metadata
  decimal_sep <- get.delim(p_file,
                           skip=skip_lines,
                           delims=c(".",","))

  df=read.csv(p_file, sep='\t',
              skip=skip_lines, 
              header=TRUE,
              dec=decimal_sep,
              colClasses=c("numeric","numeric", "NULL"))
  colnames(df)<-c("time", "amplitude")
  df<-df[,1:2]
  df
  
}


test_peak=function(p_file)
{


  mscope_ver=detect_version(p_file)
  metadata <- get_metadata(p_file, mscope_ver)
  signal_frame <- get_signal(p_file, mscope_ver)
  #View(signal_frame)
  max_peak=which(signal_frame$amplitude==max(signal_frame$amplitude))
  max_peak<-max_peak[1]
  min_peak=which(signal_frame$amplitude==min(signal_frame$amplitude))
  min_peak<-min_peak[1]
   if(min_peak<max_peak)
  {
    min_first<<-c(min_first, p_file)
  }
  else
  {
    max_first<<-c(max_first, p_file)
  }
}

write_inversed_file=function(p_file, suffix)
{

  mscope_ver=detect_version(p_file)
  metadata <- get_metadata(p_file, mscope_ver)
  signal_frame <- get_signal(p_file, mscope_ver)
  signal_frame$amplitude<-(signal_frame$amplitude * -1)
  signal_frame$amplitude2<-0
  new_file=gsub(".csv$", paste0(suffix,".csv"), p_file)
  if(mscope_ver==1)
  {
    meta_delim<-"="
  }
  else if(mscope_ver==2)
  {
    meta_delim<-"\t"
  }
  write.table( metadata,  
              file=new_file, 
              sep=meta_delim,
              quote=FALSE,
              row.names=FALSE, 
              col.names=FALSE )
  write.table( signal_frame,  
               file=new_file, 
               sep="\t", 
               append=TRUE,
               quote=FALSE,
               row.names=FALSE, 
               col.names=c("Time [s]","Amplitude", "Amplitude") )
  print(paste0(new_file, " has been created"))
  
}

write_inversed_file_call=function(fn)
{
  write_inversed_file(fn, file_suffix)
}


tk_interface=function(p_min_first, p_max_first)
{
  list_selected=c()
  wind2 <- tktoplevel()
  tkwm.resizable(wind2, FALSE, FALSE)
  scr_width <- system("wmic desktopmonitor get screenwidth", intern=TRUE)

  
  scr_width=as.numeric(scr_width[2])
  tkwm.geometry(wind2, paste0(scr_width,"x300+0+0"))
  tkwm.title(wind2, "Sélection des Variables")
  
  labellistSel <- tklabel(wind2, text="Selected for reversion",fg="black")
  labellistMin <- tklabel(wind2, text="Min peak before max peak")
  labellistMax <- tklabel(wind2, text="Max peak before min peak",fg="black")
 
  scrlistSelX<-tkscrollbar(wind2, repeatinterval=5, orient="horizontal",width=90, command=function(...) tkxview(listSel,...))
  listSel <- tklistbox(wind2,selectmode="extended",height=5,width=90, xscrollcommand=function(...) tkset(scrlistSelX,...))
  scrlistMin<-tkscrollbar(wind2, repeatinterval=5, command=function(...) tkyview(listMin,...))
  scrlistMinX<-tkscrollbar(wind2, repeatinterval=5, orient="horizontal",width=90, command=function(...) tkxview(listMin,...))
  listMin <- tklistbox(wind2,selectmode="extended",height=5,width=90, yscrollcommand=function(...) tkset(scrlistMin,...), xscrollcommand=function(...) tkset(scrlistMinX,...))
  scrlistMax<-tkscrollbar(wind2, repeatinterval=5, command=function(...) tkyview(listMax,...))

  scrlistMaxX<-tkscrollbar(wind2, repeatinterval=5, orient="horizontal",width=90, command=function(...) tkxview(listMax,...))
  listMax <- tklistbox(wind2,selectmode="extended",height=5,width=90, yscrollcommand=function(...) tkset(scrlistMax,...), xscrollcommand=function(...) tkset(scrlistMaxX,...))
  for(i in 1:length(p_min_first))
  {
    tkinsert(listMin,"end",p_min_first[i])
  }
  for(i in 1:length(p_max_first))
  {
    tkinsert(listMax,"end",p_max_first[i])
  }

  
  boutonSelMin<-tkbutton(wind2,text="<",width=5,command=function(){
    if (tclvalue(tkcurselection(listMin))!="") {
    
      selection<-as.numeric(strsplit(tclvalue(tkcurselection(listMin))," ")[[1]])+1
      for(i in selection){
        if(!(p_min_first[i] %in% list_selected))
        {
          list_selected<<-c(list_selected,p_min_first[i])
          tkinsert(listSel,"end",p_min_first[i])
        }
      }
    }
  })
  boutonSelMax<-tkbutton(wind2,text="<",width=5,command=function(){
    if (tclvalue(tkcurselection(listMax))!="") {
      
      selection<-as.numeric(strsplit(tclvalue(tkcurselection(listMax))," ")[[1]])+1
        for(i in selection){
        if(!(p_max_first[i] %in% list_selected))
        {
          list_selected<<-c(list_selected,p_max_first[i])
          tkinsert(listSel,"end",p_max_first[i])
        }
      }
    }
  })
    
  boutonErase<-tkbutton(wind2,text="Erase",width=7, command=function() {
    if (tclvalue(tkcurselection(listSel))!="") {
      selection<-as.numeric(strsplit(tclvalue(tkcurselection(listSel))," ")[[1]])+1
      for(i in selection){
        tkdelete(listSel,i-1)
        
      }
      list_selected<-list_selected[-selection]
    }
    
  })
  boutonValidate<-tkbutton(wind2,text="Validate",width=7, command=function() 
    {
        lapply(list_selected,write_inversed_file_call)
    }
  )
  
  tkgrid(labellistSel,row=0,column=0)
  tkgrid(listSel,row=1,column=0,rowspan=2)
  tkgrid(scrlistSelX,row=3,column=0, sticky="ns")
  tkgrid(boutonErase,row=4,column=0)
  tkgrid(boutonSelMin,row=1,column=2)
  tkgrid(labellistMin,row=0,column=3)
  tkgrid(scrlistMin,row=1,column=4,rowspan=2, sticky="ns")
  tkgrid(listMin,row=1,column=3,rowspan=2)
  tkgrid(scrlistMinX,row=3,column=3, sticky="ns")

  tkgrid(boutonSelMax,row=1,column=5)
  tkgrid(labellistMax,row=0,column=6)
  tkgrid(scrlistMax,row=1,column=7,rowspan=2, sticky="ns")
  tkgrid(listMax,row=1,column=6,rowspan=2)
  tkgrid(scrlistMaxX,row=3,column=6, sticky="ns")

  tkgrid(boutonValidate,row=4,column=3)
  tkwait.window(wind2)
}

#test_peak("C:\\Users\\ftheeten\\Downloads\\EXAMPLE EOD FILES FROM MbisaCongo2018-20200328\\Pollimyrus cf. stappersii 1\\MbisaCongo_2018_MC1007_B.csv")
files=choose.files()
lapply(files, test_peak)
tk_interface(min_first, max_first)


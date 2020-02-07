require(reader)
require(ggplot2)
require(changepoint)
require(dplyr )

global_metadata_lines = 12
global_sampling_rate = 192000
global_bit_depth = 16
global_encoding="utf-8"
global_treshold <- 0.02
global_nb_peaks<-7
global_arbitrary_baseline <-40

global_baseline_done = FALSE

Eod <-setRefClass("Eod",
                     fields = list(
                       file = "character",
                       threshold= "numeric",
                       maxPeaks= "numeric",
                       specimenTag = "character",
                       provisionalId="character",
                       collectionEvent= "character",
                       recordingSoftware = "character",
                       recordingHardware = "character",
                       recordist = "character",
                       bitDepth = "numeric",
                       samplingRate = "numeric",
                       recordingDateTime="ANY",
                       recordingTemperature="numeric",
                       recordingConductivity="numeric",
                       projectName = "character", 
                       wave = "ANY",
                       metadata_lines = "numeric",
                       obj_creation_time="ANY",
                       file_metadata = "ANY",
                       encoding = "character",
                       file_wave="ANY",
                       decimal_sep = "character",
                       #SC treatements
                       o_changepoints_mean="ANY",
                       v_changepoints_mean = "vector",
                       o_changepoints_var="ANY",
                       v_changepoints_var = "vector",
                       o_changepoints_meanvar="ANY",
                       v_changepoints_meanvar = "vector",
                       mean_by_mean="numeric",
                       mean_by_var="numeric",
                       mean_by_meanvar="numeric",
                       mean_by_arbitrary_baseline="numeric",
                       last_by_mean="numeric",
                       last_by_var="numeric",
                       last_by_meanvar="numeric",
                       last_by_arbitrary_baseline="numeric",
                       arbitraryBaseline="numeric",
                       frame_baseline=="ANY"
                     )
                     
)

atts <- attributes(Eod$fields())$names
print(atts)
Eod$accessors(atts)
Eod$methods()

Eod$methods(
  initialize=
    function(specimenTag="N/A",
             samplingRate =  global_sampling_rate ,
             metadata_lines = global_metadata_lines, 
             provisionalId="N/A",
             collectionEvent="N/A",
             recordingSoftware ="N/A",
             recordingHardware="N/A" ,
             recordist="N/A",
             bitDepth =global_bit_depth ,
             recordingDateTime = "N/A",
             recordingTemperature = 0,
             recordingConductivity = 0,
             projectName = "N/A", 
             encoding = global_encoding,
             threshold = global_treshold,
             maxPeaks = global_nb_peaks,
             arbitraryBaseline= global_arbitrary_baseline,
             ...
             )
             {
              callSuper(...)
              specimenTag <<- specimenTag
              provisionalId <<- provisionalId
              collectionEvent <<- collectionEvent
              recordingSoftware <<- recordingSoftware
              recordingHardware <<- recordingHardware
              recordist <<- recordist
              bitDepth <<-  bitDepth
              samplingRate <<-  samplingRate
              recordingDateTime <<-  recordingDateTime
              recordingTemperature <<-  recordingTemperature
              recordingConductivity <<-  recordingConductivity
              threshold <<- threshold
              maxPeaks <<-maxPeaks
              #tech
              metadata_lines <<- metadata_lines
              projectName <<- projectName
              encoding <<-encoding
              obj_creation_time <<- Sys.time()
              #sc
              arbitraryBaseline<<-arbitraryBaseline
              v_changepoints_mean <<-c(0)
              v_changepoints_var <<-c(0)
              v_changepoints_meanvar <<-c(0)
              mean_by_mean<<-0
              mean_by_var<<-0
              mean_by_meanvar<<-0
              mean_by_arbitrary_baseline<<-0
              last_by_mean<<-0
              last_by_var<<-0
              last_by_meanvar<<-0
              last_by_arbitrary_baseline<<-0
            
               .self
             }
    ,
    readFile = function(file, encoding=global_encoding)
    {
      file <<- file
      encoding <<-encoding
      file_obj_tmp <- read.csv(file, 
                            header=FALSE, 
                            nrows=metadata_lines, 
                            sep="=", 
                            colClasses=c("character","character"),
                            fileEncoding = encoding)
 
      #remove extra columns
      if(ncol(file_obj_tmp)>2)
      {
        file_obj_tmp<-file_obj_tmp[,1:2]
      }
      
      colnames(file_obj_tmp) <- c("param", "value")
      file_metadata<<-file_obj_tmp
      initialize(
        specimenTag=file_metadata$value[1],
        provisionalId=file_metadata$value[2],
        collectionEvent=file_metadata$value[3],
        recordingSoftware=file_metadata$value[4],
        recordingHardware=file_metadata$value[5],
        recordist=file_metadata$value[6],
        bitDepth=as.numeric(file_metadata$value[7]),
        samplingRate = as.numeric(file_metadata$value[8]),
        recordingDateTime = file_metadata$value[9],
        recordingTemperature = as.numeric(file_metadata$value[10]),
        recordingConductivity =  as.numeric(file_metadata$value[11]),
        projectName =   file_metadata$value[12],
      )
      #read wave
      decimal_sep <<- get.delim(file,
                                skip=metadata_lines,
                                delims=c(".",".")
      )
      file_wave<<-read.csv(
        file,
        sep='\t',
        skip=metadata_lines, 
        header=TRUE,
        dec=decimal_sep,
        colClasses=c("numeric","numeric", "NULL")
      )
      if(ncol(file_wave)>2)
      {
        file_wave<<-file_wave[,1:2]
      }
      colnames(file_wave) <<- c("time", "amplitude")
    },
  
    getMetadata=function()
    {
      file_metadata
    },
  
    getWave=function()
    {
     file_wave
    },
  
    defineBaseline=function()
    {
      v_frame<-.self$getWave()
      
      o_changepoints_mean<<-cpt.mean(v_frame$amplitude, method="BinSeg",Q=5)
      v_changepoints_mean<<-cpts(o_changepoints_mean)
      
      if(length(v_changepoints_mean)>0)
      {  
        mean_by_mean <<- coalesce(mean(v_frame[1:v_changepoints_mean[1],1 ]),0L )
        last_by_mean <<- tail(v_changepoints_mean, n=1)
      }
      
      o_changepoints_var<<-cpt.var(v_frame$amplitude, method="BinSeg",Q=5)
      v_changepoints_var<<-cpts(o_changepoints_var)
      if(length(v_changepoints_var)>0)
      { 
        mean_by_var <<- mean(v_frame[1:v_changepoints_var[1],1 ])
        last_by_var <<- tail(v_changepoints_var, n=1)
      }
      
      o_changepoints_meanvar<<-cpt.meanvar(v_frame$amplitude, method="BinSeg",Q=5)
      v_changepoints_meanvar <<- cpts(o_changepoints_meanvar)
      if(length(v_changepoints_meanvar)>0)
      { 
        mean_by_meanvar <<- mean(v_frame[1:v_changepoints_meanvar[1],1 ])
        last_by_meanvar <<- tail(v_changepoints_meanvar, n=1)
      }
      
      mean_by_arbitrary_baseline <<- mean(v_frame[1:arbitraryBaseline, 1 ])
      last_by_arbitrary_baseline <<-nrow(v_frame)
        
      global_baseline_done = TRUE
      
    },
  
    getPossibleBaseline=function()
    {
      print( global_baseline_done)
      if(global_baseline_done==FALSE)
      {
        defineBaseline()
        print("done")
      }
      df<-data.frame(type=c("mean", "var", "meanvar","by_arbitrary_baseline" ), mean_value=c(mean_by_mean,mean_by_var,
                                                                             mean_by_meanvar,mean_by_arbitrary_baseline ))
     
      frame_baseline <<- data.frame(c(mean_by_mean,mean_by_var,mean_by_meanvar,mean_by_arbitrary_baseline ), 
                                   c(v_changepoints_mean[1],v_changepoints_var[1], v_changepoints_meanvar[1],40),
                                   c(last_by_mean, last_by_var, last_by_meanvar,last_by_arbitrary_baseline)
                                  )
      

      colnames(frame_baseline)<<-c("mean", "position", "last")
      df
    }
  
  )



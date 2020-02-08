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
                       frame_baseline="ANY",
                       frame_absolute="ANY",
                       chosen_baseline_index="numeric",
                       chosen_baseline="ANY",
                       chosen_baseline_position="numeric",
                       chosen_last_position="numeric"
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
      
      
      o_changepoints_mean<<-cpt.mean(file_wave$amplitude, method="BinSeg",Q=5)
      v_changepoints_mean<<-cpts(o_changepoints_mean)
      
      if(length(v_changepoints_mean)>0)
      {  
        mean_by_mean <<- coalesce(mean(file_wave[1:v_changepoints_mean[1],1 ]),0L )
      }
      else
      {
        v_changepoints_mean<<-c(1,length(file_wave$time))
      }
      last_by_mean <<- tail(v_changepoints_mean, n=1)
      
      o_changepoints_var<<-cpt.var(file_wave$amplitude, method="BinSeg",Q=5)
      v_changepoints_var<<-cpts(o_changepoints_var)
      if(length(v_changepoints_var)>0)
      { 
        mean_by_var <<- mean(file_wave[1:v_changepoints_var[1],1 ])
      }
      else
      {
        v_changepoints_var<<-c(1,length(file_wave$time))      
      }
      last_by_var <<- tail(v_changepoints_var, n=1)
      
      o_changepoints_meanvar<<-cpt.meanvar(file_wave$amplitude, method="BinSeg",Q=5)
      v_changepoints_meanvar <<- cpts(o_changepoints_meanvar)
      if(length(v_changepoints_meanvar)>0)
      { 
        mean_by_meanvar <<- mean(file_wave[1:v_changepoints_meanvar[1],1 ])
      }
      else
      {
        v_changepoints_meanvar<<-c(1,length(file_wave$time))
      }
      last_by_meanvar <<- tail(v_changepoints_meanvar, n=1)
      mean_by_arbitrary_baseline <<- mean(file_wave[1:arbitraryBaseline, 1 ])
      last_by_arbitrary_baseline <<-nrow(file_wave)
        
      global_baseline_done = TRUE
      
    },
    getAbsoluteValues=function()
    {
      
      frame_absolute <<- data.frame(
        entry=c("duration_sec", "nb_lines", "min_amplitude", "max_amplitude", "pos_min", "pos_max", "time_min", "time_max"),
        value=c(tail(file_wave$time,1), 
                format(length(file_wave$time), scientific=FALSE), 
                min(file_wave$amplitude), max(file_wave$amplitude),
                format(which(file_wave$amplitude==min(file_wave$amplitude)), scientific=FALSE),
                format(which(file_wave$amplitude==max(file_wave$amplitude)), scientific=FALSE),
                file_wave$time[which(file_wave$amplitude==min(file_wave$amplitude))],
                file_wave$time[which(file_wave$amplitude==max(file_wave$amplitude))]
        )
        
      )
      frame_absolute
    }
    ,
    getPossibleBaseline=function()
    {
     
      if(global_baseline_done==FALSE)
      {
        defineBaseline()
        
      }
     
      frame_baseline <<- data.frame(
                          type=c("mean", "var", "meanvar","by_arbitrary_baseline" ),
                          mean=c(mean_by_mean,mean_by_var,mean_by_meanvar,mean_by_arbitrary_baseline ), 
                          first_position=c(v_changepoints_mean[1],v_changepoints_var[1], v_changepoints_meanvar[1],arbitraryBaseline),
                          last_position=c(last_by_mean, last_by_var, last_by_meanvar,last_by_arbitrary_baseline),
                          first_position_time=c(file_wave$time[ v_changepoints_mean[1]],
                                                file_wave$time[v_changepoints_var[1]], 
                                                file_wave$time[v_changepoints_meanvar[1]],
                                                file_wave$time[arbitraryBaseline]),
                          last_position_time=c(file_wave$time[last_by_mean], file_wave$time[last_by_var], file_wave$time[last_by_meanvar],file_wave$time[last_by_arbitrary_baseline]),
                          inflexion_points=c(paste(v_changepoints_mean,collapse=" "),paste(v_changepoints_var,collapse=" "), paste(v_changepoints_meanvar,collapse=" "),paste(c(arbitraryBaseline,last_by_arbitrary_baseline),collapse=" "))
                                  )

      chooseBaselineAndNormalize("meanvar")
      frame_baseline
    },
    chooseBaselineAndNormalize = function(type) #type = "mean, meanvar, var, arbitrary
    {
      if(type=="mean")
      {
        chosen_baseline_position <<- 1
      }
      else if(type=="var")
      {
        chosen_baseline_position <<- 2
      }
      else if(type=="meanvar")
      {
        chosen_baseline_position <<- 3
      }
      else if(type=="arbitrary")
      {
        chosen_baseline_position <<-4
      }
      chosen_baseline<<-frame_baseline$mean[chosen_baseline_index]
      chosen_baseline_position<<-frame_baseline$first_position[chosen_baseline_index]
      chosen_last_position<<-frame_baseline$last_position[chosen_baseline_index]
      
    }
    
  
  )



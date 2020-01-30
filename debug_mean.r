require(changepoint)
require(ggplot2)


nb_metadata_rows<-12
global_treshold <- 0.02
global_plot<-NULL
global_frame<-NULL
#baseline

## Functions

base_and_norm_amplitude<-function( p_frame, mean_first_segment)
{
  p_frame$amplitude <- p_frame$amplitude - mean_first_segment
  #View(p_frame)
  p_frame$amplitude <- p_frame$amplitude/(max(p_frame$amplitude) - min(p_frame$amplitude))
  return(p_frame) #returned
}



start_end_signal<-function(p_frame, p_treshold)
{
  #v_diff <- diff(p_frame$amplitude)
  #v_diff <- abs(v_diff)
  p_frame$amplitude <- abs(p_frame$amplitude)
  signal <- which(p_frame$amplitude > p_treshold)
  c(min(signal), max(signal))
}

normalize_and_center_time<-function(p_frame)
{
  #normalize time
  p_frame$time<- p_frame$time / (max(p_frame$time) - min(p_frame$time))
  #find center
  max_pos = which(p_frame$amplitude ==(max(p_frame$amplitude)))
  min_pos = which(p_frame$amplitude ==(min(p_frame$amplitude)))
  interv<-c(max_pos, min_pos)
  
  print("Pos max and min")
  print(interv)
  
  #if min before amx
  interv_sorted<- sort(interv)
  
  print("Pos max and min - sorted")
  print(interv_sorted)
  print("p1 time")
  print(p_frame[interv_sorted[1],1])
  print("p2 time")
  print(p_frame[interv_sorted[2],1])
  
  time_center_value = (p_frame[interv_sorted[2],1]+p_frame[interv_sorted[1],1]) /2
  
  
  print("(p1 time + p2 time) / 2")
  print(time_center_value)
  
  #closest array point
  real_time_center_position = which(abs(p_frame$time - time_center_value)==min(abs(p_frame$time - time_center_value))) 
  print("REAL TIME CENTER POSITION")
  print(real_time_center_position)
  real_time_center_value = p_frame[real_time_center_position, 1]
  print("REAL TIME CENTER VALUE")
  print(real_time_center_value)
  
  #substract time baseline value
  p_frame$time <- p_frame$time - real_time_center_value
  #return
  print("return")
  return(p_frame)
  
}

plot_frame<-function(p_data, p_changepoints_mean, p_changepoints_var, p_changepoints_meanvar)
{
  ggplot(p_data,aes(x = time, y = amplitude)) + geom_line()+ 
    geom_vline(xintercept = p_data[p_changepoints_mean,1], 
                color = "red", size=0.5) +
    geom_vline(xintercept = p_data[p_changepoints_var,1], 
               color = "blue", size=0.5) +
    geom_vline(xintercept = p_data[p_changepoints_meanvar,1], 
               color = "green", size=0.5)
}

handle_file<-function(p_file)
{
  print("--------------------------------------------------------------------------")
   print(p_file)
  v_frame=read.csv(
    p_file,
    sep='\t',
    skip=nb_metadata_rows, 
    header=TRUE,
    colClasses=c("numeric","numeric", "NULL")
  )
  
  #remove extra columns
  if(ncol(v_frame)>2)
  {
    v_frame=v_frame[,1:2]
  }
  #rename columns
  colnames(v_frame) <- c("time", "amplitude")
  print("Summary")
  summary(v_frame)
  

  o_changepoints_mean=cpt.mean(v_frame$amplitude, method="BinSeg",Q=5)
  v_changepoints_mean=cpts(o_changepoints_mean)
  print("change points mean")
  print(v_changepoints_mean)
  
  
  o_changepoints_var=cpt.var(v_frame$amplitude, method="BinSeg",Q=5)
  v_changepoints_var=cpts(o_changepoints_var)
  print("change points var")
  print(v_changepoints_var)
  mean_by_var <- mean(v_frame[1:v_changepoints_var[1],1 ])
 
  

  o_changepoints_meanvar=cpt.meanvar(v_frame$amplitude, method="BinSeg",Q=5)
  v_changepoints_meanvar = cpts(o_changepoints_meanvar)
  print("change points meanvar")
  print(v_changepoints_meanvar)
  mean_by_meanvar <- mean(v_frame[1:v_changepoints_meanvar[1],1 ])
  
  

  mean_by_40 <- mean(v_frame[1:40, 1 ])
  
  print("BASELINE OF 1ST SEGMENT BY VAR")
  print(mean_by_var)
  print("BASELINE OF 1ST SEGMENT BY MEANVAR")
  print(mean_by_meanvar)
  print("BASELINE OF 1ST SEGMENT BY 40")
  print(mean_by_40)
  chosen_baseline <- min(c(mean_by_var, mean_by_meanvar, mean_by_40))
  print("Chosen base line")
  print(chosen_baseline)
  
  v_frame<-base_and_norm_amplitude(v_frame,chosen_baseline )
  v_frame<-normalize_and_center_time(v_frame)
  print("returned")
  v_frame_extrema  = v_frame[sort(c(min(which(v_frame$amplitude == min(v_frame$amplitude) )), min(which(v_frame$amplitude == max(v_frame$amplitude))) )), ]
  v_frame_extrema$labels = c("P0","P1")
  
  #USELESS IF NORMALIZED JUST USE GLOBAL TRESHOOLd VAR 
  #v_treshold = (max(v_frame$amplitude) - min(v_frame$amplitude)) * global_treshold
  #print("TRESHOLD =")
  #print(v_treshold)
  
  signal_limit = start_end_signal(v_frame, global_treshold)
  print("SIGNAL LIMIT")
  print(signal_limit)
  v_start_time_signal = v_frame$time[signal_limit[1]]
  print(v_start_time_signal)
  v_end_time_signal = v_frame$time[signal_limit[2]]
  print(v_end_time_signal)
  print("signal duration=")
  print(v_end_time_signal - v_start_time_signal)
  v_frame_start_end  = v_frame[signal_limit, ]
  v_frame_start_end$labels = c("T1","T2")
  
  tmp_plot<-ggplot(v_frame,aes(x = time, y = amplitude)) + geom_line() + ggtitle(p_file)+
    geom_point(shape=8, data= v_frame_extrema , fill="black", color="black", size=3) + #display peaks
    geom_text(data= v_frame_extrema, label=v_frame_extrema$labels,  hjust=0,vjust=0) + #peaks label
    geom_vline(xintercept = 0, linetype="dashed", color = "black", size=0.5) + # x=0 (halfway between peaks)
    #geom_vline(xintercept=v_frame_start_end$time,  color = "red", size=0.5)
    geom_point(shape=1,   data= v_frame_start_end ,  color="red", size=3) + #display start and stop
    geom_text(data= v_frame_start_end, label=v_frame_start_end$labels,  hjust=0,vjust=0)
  
  save_variable<-paste0(p_file, ".png")
  ggsave(save_variable)
  
  if(is.null(global_frame))
  {
    global_frame<<-v_frame
  }
  else
  {
    
    global_frame<<-merge(x = global_frame, y = v_frame, by = "time", all = TRUE)
  }  
  print("end caller functiuon")
}

##MAIN 

src_files<-choose.files(default = "", caption = "Select file", multi = TRUE)
#src_file<-"D:\\ftheeten\\BICS\\R_DEV_CORNELL\\TRAINING2020\\data\\MbisaCongo_MC-1013_A.csv"
print(src_files)
typeof(src_files)


lapply(src_files, function(x) { 
   
    handle_file(x)
  } )

#print(global_frame)


print("End")

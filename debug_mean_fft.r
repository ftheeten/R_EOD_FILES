require(ggplot2)
require(changepoint)
require(reader) #get.delim
require(pracma) #findPeaks https://www.rdocumentation.org/packages/pracma/versions/1.9.9/topics/findpeaks
require(phonTools)

nb_metadata_rows<-12
acq.freq <- 192000
global_decimal_sep<-'.'
global_treshold <- 0.02
global_treshold_peaks <- 0# 0.000025
global_nb_peaks<-7
global_plot<-NULL
global_frame<-NULL
go_freq <- TRUE
#baseline

## Functions

eod_spectrum <-function(p_frame)
{
 
  #var_pwelch<-pwelch(p_frame$amplitude,fs=acq.freq, plot=FALSE)
  #print(var_pwelch)
  #plot( var_pwelch$freq,  10*log10(var_pwelch$spec/max(var_pwelch$spec)) , type='l', log = "x")
  spec<-spectrum(p_frame$amplitude,fs=acq.freq, plot=FALSE)
  #plot( spec$freq,  10*log10(spec$spec/max(spec$spec)) , type='l', log = "x")
  tmp=data.frame(freq=spec$freq,spec=spec$spec )
  tmp$freq<-tmp$freq* acq.freq
  tmp_plot<-ggplot(tmp, aes(x=tmp$freq, y=10*log10(tmp$spec/max(tmp$spec)) )) + geom_line() + scale_x_log10()
  tmp_plot
  
}

pad_1_sec<-function(p_frame)
{
  v_diff_1sec=1-max(p_frame$time)
  print("diff 1 sec")
  print(v_diff_1sec)
  
  v_time_interv=diff(p_frame$time)
  v_mean_interval=mean(v_time_interv)
  print("mean interv")
  print(v_mean_interval)
  #View(v_time_interv)
  v_pad=seq(0,v_diff_1sec/2, by=v_mean_interval )
  #View(pad)
  #copy
  v_1sec_frame=data.frame(p_frame)
  v_1sec_frame$time<-v_1sec_frame$time + v_mean_interval+ (v_diff_1sec/2) 
  v_pad_frame=data.frame(time=v_pad, amplitude=rep(0, times=length(v_pad)))
  #View(v_pad_frame)
  v_1sec_frame<-rbind(v_pad_frame,v_1sec_frame )
  v_pad_frame$time=v_pad_frame$time + v_mean_interval +max(v_1sec_frame$time)
  v_1sec_frame<-rbind(v_1sec_frame, v_pad_frame)
  plot(v_1sec_frame, type='l')
  print("Min padded")
  print(min(v_1sec_frame$time))
  print("Max padded")
  print(max(v_1sec_frame$time))
  v_1sec_frame
  
  
}

fct_diff_deriv<-function(p_frame)
{
  v_returned=c()
  v_diff=diff(p_frame$amplitude)
  v_returned=c(v_returned,0 )
  for(i in 1:length(v_diff)-1)
  {
    v_returned=c(v_returned,v_diff[i+1]-v_diff[i] )
  }
  v_returned=c(v_returned,0 )
 
  p_frame$amplitude<-v_returned
  
  p_frame
  
  
}

fct_freq <- function(p_frame, p_file)
{
  v_1sec_frame<-pad_1_sec(p_frame)
  v_plot<-eod_spectrum(v_1sec_frame)  
}

#LANDMARKS
base_and_norm_amplitude<-function( p_frame, mean_first_segment)
{
  p_frame$amplitude <- p_frame$amplitude - mean_first_segment
  #View(p_frame)
  p_frame$amplitude <- p_frame$amplitude/(max(p_frame$amplitude) - min(p_frame$amplitude))
  return(p_frame) #returned
}



start_end_signal<-function(p_frame, p_threshold)
{
  #v_diff <- diff(p_frame$amplitude)
  #v_diff <- abs(v_diff)
  p_frame$amplitude <- abs(p_frame$amplitude)
  signal <- which(p_frame$amplitude > p_threshold)
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

detect_peak_plateau<-function(p_frame, current_position, last_position)
{
  returned<-current_position
  p_ref_val=p_frame$amplitude[current_position]
  for(i in current_position+1:last_position)
  {
    p_val<-p_frame$amplitude[i]
    if(p_val!=p_ref_val)
    {
      return(i-1)
    }
  }
  returned
}

peak_detection<-function(p_frame, start_index,last_position, threshold=NULL, diff_threshold=NULL)
{
  v_position=c()
  v_prominence=c()
  previous_peak=0
  #v_prominence <-c()
  for(i in start_index:last_position )
  {
    p_val=p_frame$amplitude[i]
    p_previous=p_frame$amplitude[i-1]
    p_next=p_frame$amplitude[i+1]
    if(p_val > p_previous && p_val == p_next)
    {
      i<-detect_peak_plateau(p_frame, i, nrow(p_frame)-1)
      p_next=p_frame$amplitude[i+1]
    }
    if(p_val > p_previous && p_val > p_next )
    {
      if(!is.null(threshold))
      {
        if(p_val<=threshold)
        {
          next
        }
      }
      if(!is.null(diff_threshold))
      {
        if((p_val-p_previous )<=diff_threshold)
        {
          next
        }
      }
      
      v_prominence<-c(v_prominence, abs(p_val-previous_peak)) 
      v_position=c(v_position, i)
      previous_peak=p_val
    }
    
  }
  v_returned<-data.frame(position=v_position, prominence=v_prominence)
  v_returned
}

find_peaks_and_valleys<-function(p_frame, p_silent_index, p_last_position, p_diff_threshold)
{

  o_peaks<-peak_detection(p_frame, p_silent_index,p_last_position, diff_threshold =p_diff_threshold )
  p_frame$amplitude=(p_frame$amplitude) * - 1
  o_peaks2<-peak_detection(p_frame, p_silent_index,p_last_position, threshold=0,diff_threshold =p_diff_threshold)
  
  v_all_submits=rbind(o_peaks, o_peaks2)

  v_all_submits<-v_all_submits[order(v_all_submits$prominence),]

  v_peaks=v_all_submits$position

  v_peaks<-unique(v_peaks)

  v_peaks<-tail(v_peaks, n=global_nb_peaks)
  v_peaks
}

save_plot<-function(p_frame, p_file, valleys_peaks, v_frame_zero, v_frame_start_end, chosen_baseline_position )
{
  tmp_plot<-ggplot(p_frame,aes(x = time, y = amplitude)) + geom_line() + ggtitle(p_file)+
    #geom_point(shape=8, data= v_frame_extrema , fill="black", color="black", size=3) + #display peaks
    geom_point(shape=8, data= p_frame[valleys_peaks,] , fill="black", color="black", size=3) + #display peaks
    #???geom_text(data= v_frame_extrema, label=v_frame_extrema$labels,  hjust=0,vjust=0) + #peaks label
    geom_point(shape=16,   data= v_frame_zero ,  color="red", size=3) + #x=0 (halfway between peaks)
    geom_point(shape=1,   data= v_frame_start_end ,  color="red", size=3) + #display start and stop
    geom_text(data= v_frame_start_end, label=v_frame_start_end$labels,  hjust=0,vjust=0)+
    geom_vline(aes(xintercept=p_frame$time[chosen_baseline_position]),
               color="red", linetype="dashed", size=1)
  
  save_variable<-paste0(p_file, ".png")
  ggsave(save_variable, tmp_plot)
}

handle_file<-function(p_file,
                      sep='\t',
                      skip=nb_metadata_rows )
{
  print("--------------------------------------------------------------------------")
   print(p_file)
   
  global_decimal_sep <<- get.delim(p_file,
                                   skip=nb_metadata_rows,
                                   delims=c(".",".")
                                   )
  print("DELIMITER IS :")
  print(global_decimal_sep)
  v_frame <- read.csv(
    p_file,
    sep='\t',
    skip=nb_metadata_rows, 
    header=TRUE,
    dec=global_decimal_sep,
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
  #chosen_baseline <- min(c(mean_by_var, mean_by_meanvar, mean_by_40))


  
  frame_baseline <- data.frame(c(mean_by_var,mean_by_meanvar,mean_by_40 ), 
                               c(v_changepoints_var[1], v_changepoints_meanvar[1],40),
                               c(tail(v_changepoints_var, n=1), tail(v_changepoints_meanvar, n=1),nrow(v_frame))
                               )
 
  colnames(frame_baseline)<-c("mean", "position", "last'")
  #chosen_baseline_index=min(which(frame_baseline$mean==min(frame_baseline$mean)))
  chosen_baseline_index<-min(which(frame_baseline$position==max(frame_baseline$position)))
  chosen_baseline<-frame_baseline$mean[chosen_baseline_index]
  chosen_baseline_position<-frame_baseline$position[chosen_baseline_index]
  chosen_last_position<-frame_baseline$last[chosen_baseline_index]
 
   print("Chosen base line")
  print(chosen_baseline)
  print("Chosen base position")
  print(chosen_baseline_position)
  print("Chosen last position")
  print(chosen_last_position)
  
  v_frame_normalize_time_only<-data.frame(v_frame)
  v_frame<-base_and_norm_amplitude(v_frame,chosen_baseline )
  #go_freq
  if(go_freq)
  {
    #COPY
    v_frame_freq<-data.frame(v_frame)
    tmp_plot_periodgram <- fct_freq(v_frame_freq)
    v_file_name_freq<-sub(".csv", '_freq.png', p_file )
    ggsave(v_file_name_freq , tmp_plot_periodgram + ggtitle(v_file_name_freq))
    
  }
  v_frame<-normalize_and_center_time(v_frame)
  v_frame_normalize_time_only<-normalize_and_center_time(v_frame)
  
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
  
  v_zero_point=which(abs(v_frame$time)==min(abs(v_frame$time)))
  v_frame_zero= v_frame[v_zero_point,]
  
  #find all peaks and valleys
  valleys_peaks <- find_peaks_and_valleys(v_frame,chosen_baseline_position,chosen_last_position-1, global_treshold_peaks )
  print(valleys_peaks)
  
  #main plot
  save_plot(v_frame, p_file, valleys_peaks, v_frame_zero, v_frame_start_end, chosen_baseline_position )
  
  #deriv plot
  tmp_frame_deriv<-fct_diff_deriv(v_frame_normalize_time_only)
  v_file_name_deriv<-sub(".csv", '_deriv.png', p_file )
  v_frame_deriv_start_end  = tmp_frame_deriv[signal_limit, ]
  v_frame_deriv_start_end$labels = c("T1","T2")
  v_frame_deriv_zero= tmp_frame_deriv[v_zero_point,]
  save_plot(tmp_frame_deriv, v_file_name_deriv, valleys_peaks, v_frame_deriv_zero, v_frame_deriv_start_end, chosen_baseline_position )
    

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

print(src_files)
typeof(src_files)


lapply(src_files, function(x) { 
   
    handle_file(x)
  } )

#print(global_frame)


print("End")

require(ggplot2)
require(reader)
require("funprog")

global_nb_peaks<-7
global_skip_lines<-12
sampling_rate <- 192000

array_normalized<-list()
array_file_names<-c()

####################################################################""

calculate_derivate = function(p_frame)
{
  div<-diff(p_frame$amplitude)
  div<-div * (1/sampling_rate)
  div=c(0,div)
  div
}

normalize_amplitude= function(p_frame, p_silent_segment)
{
  mean_silence=mean(p_frame$amplitude[1:p_silent_segment])
  #frame_noise_reduction
  p_frame$amplitude <- p_frame$amplitude- mean_silence
  p_frame$amplitude <- p_frame$amplitude/(max(p_frame$amplitude) - min(p_frame$amplitude))
  p_frame  
}

normalize_time_on_peak_distance = function(p_frame)
{
  pos_min=min(which(p_frame$amplitude==min(p_frame$amplitude)))
  pos_max=min(which(p_frame$amplitude==max(p_frame$amplitude)))
  time_min=p_frame$time[pos_min]
  time_max=p_frame$time[pos_max]
  time_base= abs(time_min-time_max)
  new_zero = (time_min + time_max) / 2
  print("new zero")
  print(new_zero)
  p_frame$time<-p_frame$time - new_zero
  p_frame$time<-p_frame$time / time_base
  p_frame
  
}

merge_eods<-function(array_eods, array_files)
{
  df=NULL
  i=1
  cols=c("time")
  for(eod in array_eods)
  {
   
    print("-----------------")
    View(eod)
    colnames(eod)<-c("time", array_files[i])
    if(i==1)
    {
      df<-data.frame(eod)
    }
    else
    {
      df<-merge(df, eod, by="time", all=TRUE )
    } 
   
    i <- i+1
  }
  df
}

plot_merged<-function(base_plot, base_frame,array_eods, array_files)
{
  i<-1
  for(eod in array_eods)
  {
    tmp_frame<-eod 
    tmp_frame$filename=array_files[i]
    colnames(tmp_frame)<-c("time", "amplitude", "filename")
    base_plot<- base_plot + geom_line(data = tmp_frame, aes(x=time, y=amplitude, color=filename) ) 
    base_plot<-base_plot + geom_vline(xintercept = min(tmp_frame$time) ,aes(color=filename))
    base_plot<-base_plot + geom_vline(xintercept = max(tmp_frame$time) ,aes(color=filename))
    i<-i+1
  }
 base_plot
  
}

peak_detection = function(p_frame, start_index,last_position, p_threshold=NULL, diff_threshold=NULL)
{
  v_position=c()
  v_prominence=c()
  previous_peak=0
  #v_prominence <-c()
  if(start_index==1)
  {
    start_index=2
  }
  if(last_position==nrow(p_frame))
  {
    last_position=last_position-1
  }
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
      if(!is.null(p_threshold))
      {
        if(p_val<=p_threshold)
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

find_peaks_and_valleys = function( p_frame, p_silent_index, p_last_position, p_diff_threshold=NULL, p_nb_peaks=global_nb_peaks)
{
  
  
  o_peaks<-peak_detection(p_frame, p_silent_index,p_last_position, diff_threshold =p_diff_threshold )
  p_frame$amplitude=(p_frame$amplitude) * - 1
  o_peaks2<-peak_detection(p_frame, p_silent_index,p_last_position, p_threshold=0,diff_threshold =p_diff_threshold)
  
  v_all_submits=rbind(o_peaks, o_peaks2)
  v_all_submits<-v_all_submits[order(v_all_submits$prominence),]
  
  v_peaks=v_all_submits$position
  
  v_peaks<-unique(v_peaks)
  
  v_peaks<-tail(v_peaks, n=p_nb_peaks)
  
  sort(v_peaks)
}

detect_peak_plateau = function(p_frame, current_position, last_position)
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


count_contiguous_positives<-function(param)
{
  serie_length<-1
  current_serie = 1
  length_biggest_serie = 0
  biggest_serie = 1
  for(i in 2:length(param))
  {
    current=param[i]
    previous=param[i-1]
    if(current-previous>=0)
    {
      serie_length<-serie_length+1
    }
    else
    {
      #print(paste("break at ", i))
      if(serie_length>length_biggest_serie)
      {
        biggest_serie=current_serie
        length_biggest_serie=serie_length
      }
      if(i<length(param))
      {
        current_serie=i
        serie_length=1
      }
    }
    #if(serie_length>length_biggest_serie)
    #{
    #  biggest_serie=current_serie
    #  length_biggest_serie=serie_length
    #}
    
  }
  #print("biggest serie")
  #print(biggest_serie)
  #print("length biggest serie")
  #print(length_biggest_serie)
  biggest_serie
  
}
####################################################################
files=choose.files()
i<-1
for(file in files)
{
  decimal_sep <- get.delim(file,
                           skip=global_skip_lines,
                           delims=c(".","."))
  df=read.csv(file, sep='\t',
              skip=global_skip_lines, 
              header=TRUE,
              dec=decimal_sep,
              colClasses=c("numeric","numeric", "NULL"))
  colnames(df)<-c("time", "amplitude")
  df<-df[,1:2]
  View(df)
  div<-calculate_derivate(df)
  df_div <- data.frame(df)
  df_div$amplitude<-div
  View(df_div)  
  
  
  
  
  
  peaks_deriv = find_peaks_and_valleys(df_div, 1, nrow(df_div),p_nb_peaks=global_nb_peaks)
  
  peaks=find_peaks_and_valleys(df, 1, nrow(df),p_nb_peaks=global_nb_peaks)
  print(peaks)
  plot(df, type="l", main="main plot")
  points(df[peaks,],col = "red")
  points(df[peaks_deriv,],col = "green")
  
  
  #meth1 min first
  df_div_first=data.frame(df[1:peaks[1],])
  pos_min=min(which(df_div_first$amplitude==min(df_div_first$amplitude)))
  print("pos_min")
  print(pos_min)
  abline(v=df$time[pos_min], col="blue")
  
  #meth2 max first
  pos_max=max(which(df_div_first$amplitude==max(df_div_first$amplitude)))
  abline(v=df$time[pos_max], col="green")
  

  
  #mth4 contiguous positives values
  time_begin<-Sys.time()
  div_left2=diff(df_div_first$amplitude)
  View(div_left2)
  pos4=count_contiguous_positives(div_left2)
  abline(v=df$time[pos4], col="brown", lty=2)
  print("duration fct")
  print(Sys.time()-time_begin)
  print(pos4)
 
  #plots
  p_normalized=normalize_amplitude(df, pos4)
  
  p_normalized <- normalize_time_on_peak_distance(p_normalized)
  
  tmp_plot<-ggplot(p_normalized,aes(x = time, y = amplitude)) + geom_line() + ggtitle(paste( basename(file), " normalized"))
  print(tmp_plot)
  array_normalized[[i]]<-p_normalized
  array_file_names<-c(array_file_names, basename(file))
  i<-i+1
  
}

merged<-merge_eods(array_normalized, array_file_names)
View(merged)

tmp_plot<-ggplot(merged, aes(x=time, y=amplitude))
tmp_plot<-plot_merged(tmp_plot, merged,array_normalized, array_file_names)
print(tmp_plot)
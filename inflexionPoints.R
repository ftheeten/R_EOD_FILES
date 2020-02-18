require(reader)
require("funprog")

global_nb_peaks<-7
global_skip_lines<-12
sampling_rate <- 192000


####################################################################""

calculate_derivate = function(p_frame)
{
  div<-diff(p_frame$amplitude)
  div<-div * (1/sampling_rate)
  div=c(0,div)
  div
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
        print(paste("break at ", i))
        if(serie_length>=length_biggest_serie)
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
  print("biggest serie")
  print(biggest_serie)
  print("length biggest serie")
  print(length_biggest_serie)
  biggest_serie
  
}
####################################################################
files=choose.files()

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
  
  #meth3 left max first
  df_div_first2=data.frame(df[1:pos_max,])
  pos_min2=min(which(df_div_first2$amplitude==min(df_div_first2$amplitude)))
  abline(v=df$time[pos_min2], col="red")
  #meth3 max diff left max first
  df_div_first3=data.frame(df[1:pos_max,])
  div_left=diff(df_div_first3$amplitude)
  pos_max3=min(which(div_left==max(abs(div_left))))
  abline(v=df$time[pos_max3], col="grey")   
  #mth4 contiguous positives values
  div_left2=diff(df_div_first$amplitude)
  View(div_left2)
  pos4=count_contiguous_positives(div_left2)
  abline(v=df$time[pos4], col="brown", lty=2) 
  
  #fun prog
  croiss <- funprog::group_if(div_left2, `<=`)
  longueurs <- lengths(croiss)
  tmp<-croiss[longueurs == max(longueurs)] # ajouter [[1]] pour avoir la première des plus longues séquences   
  print(tmp)
  
  plot(df_div, type="l", main="deriv")
  
 
 
}
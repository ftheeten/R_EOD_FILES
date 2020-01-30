require(changepoint)
require(ggplot2)


nb_metadata_rows<-12
global_plot
#baseline

## Functions

base_and_norm<-function(p_first_point, p_frame)
{
  first_segment <- p_frame[1:p_first_point,]
  mean_first_segment = mean(first_segment$amplitude)
  print("mean first segment = ")
  print(mean_first_segment)
  #baseline <<-mean_first_segment
  #View(p_frame)
  p_frame$amplitude <- p_frame$amplitude - mean_first_segment
  #View(p_frame)
  p_frame$amplitude <- p_frame$amplitude/(max(p_frame$amplitude) - min(p_frame$amplitude))
  p_frame #returned
}





center_time<-function(p_frame)
{
  
  
  v_main_peak <- which(p_frame$amplitude==max(p_frame$amplitude))
  print(v_main_peak)
  bin_seg1 <- p_frame[1:v_main_peak-1, ]
  v_previous_valley <- which(bin_seg1$amplitude==min(bin_seg1$amplitude))
  print(v_previous_valley)
  bin_seg2 <- p_frame[1:v_previous_valley-1, ]
  v_previous_peak <- which(bin_seg2$amplitude==max(bin_seg2$amplitude))
  print(v_previous_peak)
  global_plot <<- global_plot + geom_vline(xintercept = p_frame[c(v_previous_peak, v_main_peak),1], 
             color = "red", linetype="dashed", size=0.5)
  baseline_time <- (p_frame[v_main_peak,1] +  p_frame[v_previous_peak,1]) /2
  print(p_frame[v_main_peak,1])
  print(p_frame[v_previous_peak,1])
  print(baseline_time)
 
  new_time_center=which(abs(p_frame$time - baseline_time)==min(abs(p_frame$time - baseline_time))) 
  print(new_time_center)
  print(p_frame[new_time_center,1])
  new_time_center_val = p_frame[new_time_center,1]
  p_frame$time <- p_frame$time - new_time_center_val
  p_frame #returned
  
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

##MAIN 

src_files<-"C:\\R_DEV_MORMY\\TRAINING_2020\\MbisaCongo_MC-1006_A.csv"
print(src_file)


v_data=read.csv(
  src_files,
  sep='\t',
  skip=nb_metadata_rows, 
  header=TRUE,
  colClasses=c("numeric","numeric", "NULL")
)
print(typeof(v_data))
#View(v_data)
#ts => time serie
print(ncol(v_data))
if(ncol(v_data)>2)
{
  v_data=v_data[,1:2]
}
colnames(v_data) <- c("time", "amplitude")
summary(v_data)



o_changepoints_mean=cpt.mean(v_data$amplitude, method="BinSeg",Q=5)
print(o_changepoints_mean)
v_changepoints_mean=cpts(o_changepoints_mean)

o_changepoints_var=cpt.var(v_data$amplitude, method="BinSeg",Q=5)
print(o_changepoints_var)
v_changepoints_var=cpts(o_changepoints_var)

o_changepoints_meanvar=cpt.meanvar(v_data$amplitude, method="BinSeg",Q=5)
print(o_changepoints_meanvar)
v_changepoints_meanvar=cpts(o_changepoints_meanvar)

print(v_changepoints_mean)
#View(v_data)

#plot_frame(v_data,v_changepoints_mean, v_changepoints_var, v_changepoints_meanvar )

v_data_norm<-base_and_norm(v_changepoints_meanvar[1], v_data)
summary(v_data_norm)
#View(v_data_norm)

global_plot<-plot_frame(v_data_norm,v_changepoints_mean, v_changepoints_var, v_changepoints_meanvar )

p_frame_centered <- center_time(v_data_norm)

print(global_plot) 

ggplot(p_frame_centered,aes(x = time, y = amplitude)) + geom_line()


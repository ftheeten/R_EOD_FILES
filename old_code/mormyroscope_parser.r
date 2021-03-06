require(changepoint)
require(quantmod)
require(rlist)     
require("ggplot2")      
require("tractor.base")

nb_metadata_rows<-12
nb_harmonics<-3

#spline : 
#http://www.sthda.com/english/articles/40-regression-analysis/162-nonlinear-regression-essentials-in-r-polynomial-and-spline-regression-models/#polynomial-regression
#linear: 
#"https://rstudio-pubs-static.s3.amazonaws.com/183882_8ee72a6acd0f41f8b4e80d82687dcd1c.html
#see also: https://towardsdatascience.com/unraveling-spline-regression-in-r-937626bc3d96

# cs is the vector of complex points to convert
convert.fft <- function(cs, sample.rate=1) {
  cs <- cs / length(cs) # normalize
  
  distance.center <- function(c)signif( Mod(c),        4)
  angle           <- function(c)signif( 180*Arg(c)/pi, 3)
  
  df <- data.frame(cycle    = 0:(length(cs)-1),
                   freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                   strength = sapply(cs, distance.center),
                   delay    = sapply(cs, angle))
  print(df)
  return(df)
}


#http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html
# returns the x.n time series for a given time sequence (ts) and
# a vector with the amount of frequencies k in the signal (X.k)
get.trajectory <- function(X.k,ts,acq.freq) {
  
  N   <- length(ts)
  i   <- complex(real = 0, imaginary = 1)
  x.n <- rep(0,N)           # create vector to keep the trajectory
  ks  <- 0:(length(X.k)-1)
  
  for(n in 0:(N-1)) {       # compute each time point x_n based on freqs X.k
    x.n[n+1] <- sum(X.k * exp(i*2*pi*ks*n/N)) / N
  }
  traj<- (x.n * acq.freq)
  convert.fft(traj,acq.freq)  
  return(traj) 
}

#http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html
plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

# Plot the i-th harmonic
# Xk: the frequencies computed by the FFt
#  i: which harmonic
# ts: the sampling time points
# acq.freq: the acquisition rate
get.harmonic <- function(Xk, i, ts, acq.freq) {
  Xk.h <- rep(0,length(Xk))
  Xk.h[i+1] <- Xk[i+1] # i-th harmonic
  harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
  return(harmonic.trajectory)

}

frame_mormi_file<-function(p_file)
{
  my_frame=read.csv(
    p_file,
    sep='\t',
    skip=nb_metadata_rows, 
    header=TRUE,
    colClasses=c("numeric","numeric", "NULL")
  )
  #skip metadata columns
  print("init_frame")
  #skip 2nd channl (if any) 
  if(length(my_frame)>2)
  {
    my_frame <- my_frame[1:2]
  }
  #rename columns
  colnames(my_frame)[1] <- 'time'
  colnames(my_frame)[2] <- 'amplitude'
  return(my_frame)
}

mean_silence<-function(p_frame, p_begin, p_end)
{
  #isolate silent part 
  segment<-p_frame[c(p_begin:p_end-1),]
  
  v_mean<-mean(segment$amplitude)
  
  return(v_mean)
  
}

get_modulus<-function(x)
{
  print(typeof(x))
  print(typeof(x[1]))
  return(sapply(x,
                  function(x)
                  {
                    return(Mod(x))
                  }
                ))
}


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}



detect_wave<-function(p_normalized, inflexion_points)
{
  inflexion_points=c(1,inflexion_points)
  
  ampl=c()
  
  for(i in 1:(length(inflexion_points)-1))
  {
    
    next_i <- i + 1
    
    start<-inflexion_points[i]
    end<- inflexion_points[next_i]
    segment<-p_normalized[c(start:end),]
    
    
    tmp_ampl<-max(segment$amplitude)-min(segment$amplitude)
    
    ampl=c(ampl,tmp_ampl)
  }
  
  return(which(ampl==max(ampl)))
}

generate_wave_segments<-function(p_wave, knots)
{
  returned=list()
  for(i in 1:(length(knots)-1))
  {
    
    next_i <- i + 1
    
    start <- knots[i]
    end   <-   knots[next_i]
    end_start<-p_wave[end,1]
    #print("tend")
    #print(end_start)
    segment<-p_wave[c(start:end),]
    
    returned<-list.append(returned, segment)
  }
  return(returned)
  
}

linear_regression_for_segment<-function(segment)
{
  
  model <- lm(segment$amplitude ~ segment$time, data = segment)
  
  return(model)
  
}

analyze_mormyfile<-function(p_frame, p_title)
{ 
  print(p_title)
  file_result<-paste0(tools::file_path_sans_ext(p_title),"_output_r.csv")
  
  #find inflexion points by binary segmentation (Max 5 values)
  
  meanvar=cpt.meanvar(p_frame$amplitude, method="BinSeg",Q=5)
  plot(meanvar)
  inflexions=cpts(meanvar)
  #print(inflexions)
  
  time_infl <-sapply(inflexions, function(x) p_frame[x,1])
  write(implode(c("inflection_points", time_infl), sep = "\t"),file=file_result)
  #mean of silence part
  mean_silence<-mean_silence(p_frame,1,inflexions[1])
  
  #vector with original amplitude - mean_first_part
  subst_silence<-sapply(p_frame$amplitude,   
                        function(x) 
                        {
                          x - mean_silence
                        }
  )
  
  
  #replace original amplitude by original_value - mean_first_segment
  p_frame$amplitude <- subst_silence
  
  
  #normalize 
  p_frame$amplitude<-normalize(p_frame$amplitude)
  #plot(p_frame)
  
  #detect wave 
  wave_index<-detect_wave(p_frame, inflexions)
  
  #isolate wave
  wave<-p_frame[c(inflexions[wave_index-1]:inflexions[wave_index]),]
  
  peaks<-findPeaks(wave$amplitude)
  valleys<-findValleys(wave$amplitude)
  
  wave_knots<-c(1,sort(c(peaks, valleys)),length(wave$amplitude))
  
  
  #print global signal
  tmp_plot_inf<-ggplot(data=p_frame,mapping=aes(x = time, y = amplitude))+ geom_line() +ggtitle(paste0(p_title,"\r\nInflection points, peaks and valleys"))
  
  for (pt in inflexions)
  {
    
    tmp_plot_inf<-tmp_plot_inf + geom_vline(xintercept = p_frame[pt,1], 
                                            color = "red", size=0.5)
  }
  
  
  ggsave(paste0(tools::file_path_sans_ext(p_title),"_inflexion_points.png"), plot=tmp_plot_inf, device="png")
  
  #print isolated wave
  tmp_plot_wave<-ggplot(data=wave,mapping=aes(x = time, y = amplitude))+ geom_line() +ggtitle(paste0(p_title,"\r\nWave"))
  print(wave_knots)
  for (pt in wave_knots)
  {
    
    
    tmp_plot_wave<-tmp_plot_wave + geom_vline(xintercept = wave[pt,1], 
                                              color = "green", size=0.5)
  }
  ggsave(paste0(tools::file_path_sans_ext(p_title),"_wave.png"), plot=tmp_plot_wave, device="png")
  #time_peaks <-sapply(wave_knots, function(x) p_frame[x,1])
  #write(implode(c("cut_wave", time_peaks), sep = "\t"),file=file_result, append=TRUE)
  
  #analyze wave
  wave_segments<-generate_wave_segments(wave, wave_knots)
  
  write(implode(c("segment", "start","end", "intercepts", "slope", "r.squared", "p.value"), sep = "\t"),file=file_result, append=TRUE)
  i=1
  for(seg in wave_segments)
  {
    #call linear model
    wave_model<-linear_regression_for_segment(seg)
    #print(summary(wave_model))
    coeff_seg=coef(wave_model)
    #print(coeff_seg)
    # Equation de la droite de regression :
    # src :http://www.sthda.com/french/wiki/ggplot2-ajouter-une-ligne-droite-a-un-graphe-ligne-horizontale-verticale-et-droite-de-regression
    eq = paste0("y = ", coeff_seg[2], "*x + ", coeff_seg[1])
    
    
    ab_name <- paste0("Regression segment ", i)
    tmp_plot<-ggplot(data=p_frame,mapping=aes(x = time, y = amplitude))+ geom_line()+ggtitle(paste0(p_title,"\r\n", ab_name))
    #print("-----------------")
    #print(seg)
    
    
    
    
    tmp_plot=tmp_plot + geom_abline(linetype ="dashed", size=1, aes(intercept=coeff_seg[1], slope=coeff_seg[2], colour=eq)) +
      scale_fill_manual(name=ab_name, values=c(eq))  + theme(legend.position="bottom")+
      scale_color_manual(name=ab_name, values=c("red")) 
    
    ggsave(paste0(tools::file_path_sans_ext(p_title),"_seg_",i,".png"), plot=tmp_plot, device="png")
    write(implode(c(i, seg[1,1],seg[length(seg$time),1], coeff_seg[1], coeff_seg[2], summary(wave_model)$r.squared, summary(wave_model)$coefficients[2,4]), sep = "\t"),file=file_result, append=TRUE)
    
    i<-i+1  
  }
  fourier_var=fft(p_frame$amplitude)
  time <- tail(p_frame$time,1)
  print(time)
  acq.freq <- 192000
  ts  <- p_frame$time #seq(0,time-1/acq.freq,1/acq.freq)
  print(ts)
  #plot(fourier_var, type='l')
  print("CALL TRAJ")

  x.n <- get.trajectory(fourier_var,ts,acq.freq)   # create time wave
  
  print(typeof(x.n))
 
  mod_fft<-get_modulus(x.n)
  normalized_fft<-normalize(mod_fft)
  plot(ts,normalized_fft, type="l")
 
  colors = rainbow(nb_harmonics, alpha = 0.3)
  for(i in 1:nb_harmonics)
  {
    print("CALL HARM")
    print(i)
    harm_var<-get.harmonic(fourier_var,i,ts,acq.freq)
    harm_var_mod<-get_modulus(harm_var)
    harm_var_norm<-normalize(harm_var_mod)
    points(ts, harm_var_norm, type="l", col=colors[i])
  }
}


#main
src_files<-choose.files(default = "", caption = "Select file", multi = TRUE)
#src_files<-"C:\\R_DEV_MORMY\\data_source\\MbisaCongo_MC-1006_A.csv"


print(src_files)
for(src in src_files)
{
  print("loop")
  print(src)
  mormy_frame<-frame_mormi_file(src)
  print("call parser")
  analyze_mormyfile(mormy_frame, src)
}


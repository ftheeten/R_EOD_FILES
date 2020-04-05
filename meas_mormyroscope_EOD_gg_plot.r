# meas_mormyroscope_EOD.r  v.4
# A tutorial for reading and measuring EOD waveforms from Mormyroscope.
# This program opens one or more Mormyroscope EODs, finds selected landmarks on each EOD
# and makes measurements of voltages, times, and slopes on these landmarks.  It also calculates
# the derivative the Fourier transform of each EOD and measures additional landmarks on those.
# 
# Designed to analyze  Mormyroscope version 1.47 revision 1 (2018) and revision 1.47.2 (2019)
# C. Hopkins, F. Theeten, Mukweze Mulelennu Feb 13-26, 2020
# C. Hopkins 14 March 2020
# This simple program can serve as a tutorial on how to input, analyze, and plot mormyroscope EODs stored on .csv files, 
# The example marks out landmarks: T1,T2,P0,P1,P2,P3,s0,s1,s2,s3, and on the FFT, f_low, fftmax, f_high
####################################################################

require("pracma")  # a "practical math functions" package.
# Each user will need to use "install.packages("name of package")" command
# that is:  >  install.packages("pracma") on their computer.  You will only do this once.
require("reader")  # the "reader" package is required to read use the get.delim() function 
require("tools")  # the tools package is needed to copy the graphic images to a .pdf file (see dev.copy) in the "dev.copy(cairo_pdf,paste(woext,".pdf",sep = ""))" command below 
require("ggplot2") #added ftheeten 2020 04 04
#library("scales") #added ftheeten 2020 04 04 to ease log10 scale in fft


###############################################################################################################
# CONSTANTS used in this program
th = 0.02  # the threshold expressed as percent of peak-to-peak voltage. This is used for estimating the beginning and end of an EOD waveform. 
P0th = 0.0002  # the threshold voltage (again as percent of peak-peak amplitude) for measuring the duration of peak, P0. In order to qualify as P0, peak must have absolute value greater than this value 
multiplier = 20. # plots of EODs will also include an enlarged plot in order to show presence of small P0 peaks. This is the multiplier value
def.timebase = 6.0  # default timebase for EOD and derivative plots, i.e. xlim = c(-timebase/2,timebase/2)
timebases = c(0.25,0.5,1.,2.,4.,6.,10.,14.,18.,22.,26.,30.,40.) # possible full scale timebases in milliseconds (if autotbscale is TRUE)
autotbscale = TRUE  # if TRUE, then measure the total duration and set the time base from closest "timebases" otherwise use def.timebase
q10 = 1.6 # Q10 of temperature effect on EOD duration
temperaturecorrection = TRUE  # if TRUE, use Q10 to adjust EOD sampling rate to def.Temperature 
def.Temperature = 25.  # default temperature (if temperature is not provided in metadata, or is outside range)
corrected_to_temp = def.Temperature  #temperature is corrected to this value
auto_polarity = TRUE  # if TRUE invert EOD waveform if polarity is -1. polarity will be automatically determined using timing of P1 vs P2
# if FALSE, the polarity will remain as recorded

###############################################################################################################
# ggplot functions added ftheeten 2020 04 04

#custom function to add text label aside of point
add_annotation=function(plot, frame, suffix, p_color, offsetx=0.01, offsety=0.01, root_incr=1)
{
  i<-root_incr
  i_data<-1
  apply(frame, 1, 
        function(x)
        {
          ptname<-paste0(suffix,i)
          #justify to avoid text cropped by top border, check_overlap = TRUE for sharper font
          plot<<- plot + geom_text(x=frame[i_data,1] + offsetx, y=frame[i_data,2] + offsety , label=ptname, color=p_color,vjust="inward",hjust="inward", check_overlap = TRUE)
          i<<-i+1
          i_data<<-i_data+1
          
        }
  )
  plot
 
}

main_plot=function(title, full_wave_frame, big_wave_frame, timebase, tps, pks, slopes, p_th)
{
  
  main_plot<-ggplot(full_wave_frame)+geom_line( aes(x=time, y=amplitude), color="red") 
  main_plot<- main_plot + geom_line(data = big_wave_frame, aes(x=time, y=amplitude ), color="grey")
  
  frame_tps<-full_wave_frame[tps,]
  main_plot<- main_plot + geom_point(data=frame_tps,aes(x = time, y = amplitude), color="black")
  main_plot<- add_annotation(main_plot, frame_tps, "T", "black")
  
  frame_pks<-full_wave_frame[pks,]
  main_plot<- main_plot + geom_point(data=frame_pks,aes(x = time, y = amplitude), color="red")
  main_plot<-add_annotation(main_plot, frame_pks, "P", "red", 0.001)
  
  frame_slope<-full_wave_frame[slopes,]
  main_plot<- main_plot + geom_point(data=frame_slope,aes(x = time, y = amplitude), color="black")
  main_plot<- add_annotation(main_plot, frame_slope, "S", "black",0.001,0.001, 0)
  
  main_plot <-main_plot + geom_hline(yintercept = c(-p_th, p_th),color="grey")
  
  main_plot<- main_plot +ylim(range(full_wave_frame$amplitude))+ xlim(c(-timebase/2,timebase/2))
  main_plot<- main_plot + xlab("time (ms)") + ylab("V (norm)")
  main_plot<- main_plot +  theme_classic() #white background
  main_plot<- main_plot +  ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  main_plot
}

derivate_plot=function(title, full_wave_frame,  timebase, tps,  slopes)
{
  
  derivate_plot<-ggplot(full_wave_frame)+geom_line( aes(x=time, y=amplitude), color="red") 
 
  frame_tps<-full_wave_frame[tps,]
  derivate_plot<- derivate_plot + geom_point(data=frame_tps,aes(x = time, y = amplitude), color="black")
  derivate_plot<- add_annotation(derivate_plot, frame_tps, "T", "black")
  

  frame_slope<-full_wave_frame[slopes,]
  derivate_plot<- derivate_plot + geom_point(data=frame_slope,aes(x = time, y = amplitude), color="black")
  derivate_plot<- add_annotation(derivate_plot, frame_slope, "S", "black", 0.01,0.01, 0)
  

  
  derivate_plot<- derivate_plot +ylim(range(full_wave_frame$amplitude))+ xlim(c(-timebase/2,timebase/2))
  derivate_plot<- derivate_plot + xlab("time (ms)") + ylab("dV/dT (V/ms)")
  derivate_plot<- derivate_plot +  theme_classic() #white background
  derivate_plot<- derivate_plot +  ggtitle(title)+ theme(plot.title = element_text(hjust = 0.5))
  derivate_plot
}

fft_plot=function(title, fft_frame, iPmax, ibandwidth)
{
  fft_plot<-ggplot(fft_frame)+geom_line( aes(x=freq, y=power), color="red")
  point_max <- fft_frame[iPmax,]
  fft_plot<- fft_plot + geom_point(data=point_max,aes(x = freq, y = power), color="black")
  fft_plot<- fft_plot + annotate(geom="text", x=fft_frame[iPmax,1] , y=fft_frame[iPmax,2] -1 , label="f_max", color="black")
 
  points_bwdth<-fft_frame[ibandwidth,]
  fft_plot<- fft_plot + geom_point(data=points_bwdth,aes(x = freq, y = power), color="black")
  fft_plot<- fft_plot + annotate(geom="text", x=points_bwdth[1,1] , y=points_bwdth[1,2] -1 , label="f_low", color="black")
  fft_plot<- fft_plot + annotate(geom="text", x=points_bwdth[2,1] , y=points_bwdth[2,2] -1 , label="f_hi", color="black")
  
  fft_plot<- fft_plot + ylim(c(-60,0))
  fft_plot<- fft_plot +  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x))) 
  fft_plot<-fft_plot+ theme(
    panel.grid.major = element_line(color="grey"),
    panel.grid.minor = element_line(color="grey"),
    panel.background = element_rect(fill="white")
  )
  fft_plot<- fft_plot + xlab("Freq (Hz)") + ylab("20*log(dB/dBref)")
  fft_plot<- fft_plot +  ggtitle(title)+ theme(plot.title = element_text(hjust = 0.5))
  fft_plot
}




###############################################################################################################

# SELECT INPUT EOD FILES:   choose one or more Mormyroscope .csv files to analyze and plot
file_array <-choose.files(default = "*.csv", caption="Select one or more Mormyroscope csv files to analyze and plot")
file_array <- sort(file_array)  # arrange the files in alphabetical order 
print(dirname(file_array[1]))
setwd(dirname(file_array[1]))  # set the working directory to the folder containing the first of these input files
wd = getwd()
setwd(wd)
print(wd) # print the working directory location on the console


############################################################################################
# SELECT FILES for OUTPUT. EODMeasurements.txt)
output_file<-file.choose(new = TRUE)
output_file<-gsub("\\\\","/", output_file )  #if you are working in Windows, find the "\\" characters in the output_file and replace thenm with "/"  

# prepare a header row to identify the columns of output data
header<- c("File","ID","Tag_num","Temp_C","corrected_Temp",
           "Cond_uS%cm","Field_num","Hardware","Software",
           "Recordist","Bits","Fs_temp_corrected","Recorded_Date","Analysis_Date", "vP0", "vP1",
           "vP2", "vP3","ss0","ss1","ss2", "ss3","vs0","vs1",
           "vs2","vs3",
           "tP2-ts0","tP2-ts1","tP2-ts2","tP2-ts3","tP2-tP0","tP2-tP1","tP2-tP1",
           "f_low","fftmax","f_high","bandwidth","TD")
# write the header to the output file
write(paste(header, collapse = "\t"),file=output_file)  # write the header to the output file

###ASK USER IF THEY WANT TO SAVE PLOTS AS .PDF
if(interactive())
  saveaspdf=askYesNo("Do you want to save the plots as .PDF's and .PNG's",default = TRUE)



####MAIN LOOP OF PROGRAM (for each file selected do this...) ##############################
i <- 1
for(fn in file_array)   # go through each file in the sequence of files in file_array
{

    #  first, detect whether this is Mormyroscope version 1 file or version 2 by seeing if metadata is separated by "="
    firstline = read.csv(fn,header = FALSE,nrows = 1,sep = "=") # get first line of file using the "=" character as the separator
    if(length(firstline)==2) {Mscope_ver = 1}else{Mscope_ver = 2}  # set Mscope_ver to 1 or 2 depending on presence of "=".  

    # do this if your file was recorded with version 1 or 2 of Mormyroscope
    if (Mscope_ver==1)  # in version 1 (Mormyroscope ver 1.47) there are 12 lines of metadata in two columns separated by equal sign ("=") followed by time and amplitude data
        {
        #detect delimiter for decimals point ("," or ".") by scanning 10 lines data in the  file starting on the 17th line
        delim <- get.delim(fn, skip=16, 
                           #to skip detection of \t
                           delims=c(".",","))
        #read lines and create dataframe for time and wave (tw)
        tw<-read.csv(fn,skip = 12, sep = "\t", dec = delim)
        
        #read metadata
        meta<-read.csv(fn,header=FALSE,nrows=12,sep = "=")  #read meta data first 12 lines
        AnalysisDate = paste(Sys.time())
        tagno = paste(meta$V2[1])
        species = paste(meta$V2[2])
        CollectionEvent=paste(meta$V2[3])  # get the string in metadata row 3, column  V2 (variable 2)rang
        Software=paste(meta$V2[4])
        Hardware=paste(meta$V2[5])
        Recordist=paste(meta$V2[6])
        BitDepth=as.numeric(paste(meta$V2[7]))
        Fs=as.numeric(as.character(meta$V2[8]))
        DateTime=paste(meta$V2[9])
        Temperature=(gsub("[^0-9.,-]","",meta$V2[10])) #remove non-numeric characters from temperature
        Temperature = as.numeric(gsub(",",".",Temperature))  #substitute commas with periods
        if(is.na(Temperature) | Temperature >40 | Temperature < 10) {
          Temperature = def.Temperature
          warning(paste("Warning: Temperature set to", def.Temperature, " deg. C for file ",basename(fn)))
          }  # set Temperature to def.Temperature if it is missing or >40 or < 10       
        conductivity = gsub("[^0-9.,-]","",meta$V2[11]) #remove non-numeric characters in case user entered some non numbers.
        conductivity = as.numeric(gsub(",",".",conductivity)) #replace "," with "."
        ProjectName=paste(meta$V2[12])
        SpecimenComment = NULL
        RecordingComment = NULL
        }

      # else, do this for version 2 of Mormyroscope
      else  # else, DO THIS FOR version Mormyroscope ver. 1.47.2
        {
        delim <- get.delim(fn, skip=16, 
                           #to skip detection of \t
                           delims=c(".",","))
        #read data and create dataframe for time and wave (tw)
        tw<-read.csv(fn,skip = 14, sep = "\t", dec = delim)
        #read metadata and create a data frame for metadata
        meta<-read.csv(fn,header=FALSE,nrows=14,sep = "\t")  #read meta data first 14 lines
        # convert the metadata into strings or numbers
        AnalysisDate =    paste(Sys.time())
        tagno   =         paste(meta$V2[1])
        species =         paste(meta$V2[2])
        SpecimenComment=  paste(meta$V2[3])
        CollectionEvent=  paste(meta$V2[4])
        Software=         paste(meta$V2[5])
        Hardware=         paste(meta$V2[6])
        Recordist=        paste(meta$V2[7])
        BitDepth=as.numeric(paste(meta$V2[8]))
        Fs=              as.numeric(as.character(meta$V2[9]))
        DateTime=       meta$V2[10]
        Temperature=(gsub("[^0-9.,-]","",meta$V2[11])) #remove non-numeric characters from temperature
        Temperature = as.numeric(gsub(",",".",Temperature))  #substitute commas with periods
        if(is.na(Temperature) | Temperature >40 | Temperature < 10) {
          Temperature = def.Temperature
          warning(paste("Warning: Temperature set to", def.Temperature, " deg. C for file ",basename(fn)))
        }  # set Temperature to def.Temperature if it is missing or >40 or < 10       
        conductivity = as.numeric(gsub("[^0-9.-]","",meta$V2[12])) #remove all but numeric values in case user entered any letters.
        RecordingComment=paste(meta$V2[13])
        ProjectName=paste(meta$V2[14])
        }  #end of if statement for version number
    
        Fscorr = Fs*q10^((corrected_to_temp-Temperature)/10) # calculate the revised sampling rate if not at standard (i.e. 25 deg) temperature
        if (temperaturecorrection){
          Fs = Fscorr    # if temperaturecorrection  is true, then use Fscorr in place of Fs
          }
        wave = tw[,2] #use only the second column of the time & wave matrix, time variable comes from Fs
        time = seq(0,length(wave)-1)/Fs   # for simplicity in typing the variables, create two vectors, one for time one for wave
        tw = data.frame(time,wave)

  ######################################## POLARITY CHECK:  CHECK THAT THE EOD WAS RECORDED WITH THE CORRECT POLARITY.  IF NOT FIX IT.
      # This part of the program may be problematic for some species of fish that have unusual EODs.  A waveform is considered to have a 
      # polarity of +1 if the maximum of the wave comes before the minimum.  For some species (i.e. Polimyrus cf. stappersii sp. 2) this is never the case and the program will get the 
      # wrong answer.  To fix this, the user may have to change this piece of the program, or ask the user to confirm that the polarity is correct.
      # find the max and min and polarity of the wave and the position
      maxw = max(wave)
      minw = min(wave)
      imax=min(which(wave==max(wave)))
      imin=min(which(wave==min(wave)))    
      polarity = 1;  #assume that polaraity is 1 (OK) at start
      if (auto_polarity){
    
        # for Cyphomyrus, Marcusenius, Paramormyrops,and Pollimyrus stappersii sp. 1 use this test
         if (imax<imin){
         polarity = 1}
         else  {
         polarity = -1}
      
        # For Pollimyrus cf. stappersii sp. 2 use this test
        # if (abs(maxw)<abs(minw)) {
        #  polarity = 1}
        #  else{polarity = -1}
      
        # 
        } #end auto_polarity
    
    
    # NORMALIZE THE EOD  and center the time axis, inverting the waveform if the polarity is -1
    baseline = mean(wave[1:30])
    wave = (wave - baseline)/(wave[imax]-wave[imin])
    wave = polarity * wave  #multiply by polarity to invert the wave if the polarity is -1
    icenter = round((imax+imin)/2)
    time = (time - time[icenter])*1000 # convert to milliseconds
    maxw=max(wave)
    minw=min(wave)
    imax = min(which(wave == maxw))
    imin = min(which(wave == minw))
    
    # find the peaks P1 and P2
    iP1 = imax
    iP2=imin
    iT1 = min(which(abs(wave)>=th))
    iT2 = max(which(abs(wave)>=th))
    TD=(time[iT2]-time[iT1])
    # if autoscale is TRUE, then automatically scale the time base to be three times the Total Duration, if FALSE use default
    if (autotbscale){
          itb = min(which((timebases-3*TD)>=0))  #select the timebase from the timebases vector so that timebase is at least 3 times the total duration of the EOD
          timebase = timebases[itb]
    }

    # search for P0
    # P0 is the minimum point between the beginning of the EOD and P1
    vP1 = wave[imax]
    tP1 = time[imax]
    vP2 = wave[imin]
    tP2= time[imin]
      
    vP0 = min(wave[1:imax]) # voltage at P0
    iP0 = min(which(wave[1:imax] == vP0))
    tP0 = time[iP0]
    if(abs(vP0)<P0th){  #check to see if vP0 is sufficiently large
      vP0 = NULL
      iP0 = NULL
      tP0 = NULL
      }
    else
      {
    iP0 = min(which(wave == vP0))
    tP0 = time[iP0]
      }
    # search for Peak 3
    vP3 = max(wave[iP2:length(wave)])   #voltage at P3
    iP3 = min(which(wave[iP2:length(wave)] == vP3))
    if(abs(vP3)<P0th)
      {
      vP3 = NULL
      iP3 = NULL
      tP3 = NULL
      }
    # calculate the derivative of the waveform    
    dvdt = (diff(smooth(wave))*Fs/1000) # smooth wave before taking deriv. units are Volts per millisecond
    dvdt = c(0,dvdt)

    ss2 = min(dvdt)
    is2 = min(which(dvdt == ss2))
    ts2 = time[is2]
    vs2 = wave [is2]
    ss1 = max(dvdt[1:is2])
    is1 = min(which(dvdt == ss1))
    ts1 = time[is1]
    vs1 = wave[is1]
    ss0 = min(dvdt[1:is1])
    is0 = min(which(dvdt == ss0))
    ts0 = time[is0]
    vs0 = wave[is0]
    ss3 = max(dvdt[is2:length(dvdt)])
    is3 = max(which(dvdt == ss3)) # max of all of the dvdt point that equal ss3
    ts3 = time[is3]
    vs3 = wave[is3]
    
    # calculate the FFT of the EOD and find the landmarks on the FFT
    Y = fft(wave)
    L = length(wave)
    P2 = abs(Y/L) #two sided spectrum of wave
    P1 = P2[1:(L/2+1)]
    lp1=length(P1)
    P1[2:lp1-1]=2*P1[2:lp1-1]
    f=Fs*(0:(L/2))/L
    Pmax = max(P1)
    iPmax = which(P1 == Pmax)
    fftmax = f[iPmax]
    Prel = 10*log10(P1/Pmax)  # multiply log Voltage ratio because the amplitude is measured in volts, and we want power we need to square volts
    # or multiply the 10 dB by 2 hence 20 x log10(P1/Pmax) 
    ihigh = min(which(Prel[iPmax:length(Prel)]<=-6))
    ibandwidth = range(which(Prel >=-6))
    f_low = f[ibandwidth[1]]   # find the f_high and f_low where the power drops by 6 dB 
    f_high = f[ibandwidth[2]]
    bandwidth = f[ibandwidth[2]]-f[ibandwidth[1]]
    
    data_row <- c(basename(fn),species, tagno, Temperature,corrected_to_temp,
                  conductivity, CollectionEvent, Hardware,
                  Software, Recordist, BitDepth, Fs, DateTime, AnalysisDate,
                  vP0, vP1, vP2, vP3, ss0,ss1,ss2, ss3, vs0,vs1,vs2,vs3,
                  tP2-ts0,tP2-ts1,tP2-ts2,tP2-ts3,tP2-tP0,tP2-tP1,tP2-tP1,
                  f_low,fftmax,f_high,bandwidth,TD)
    
    write(paste(data_row, collapse = "\t"),file=output_file, append=TRUE)
    
    # plot the data, both wave and derivative

    #op <- par() # query graphical parameters and save them in op
    #par(mar = c(3,3,3,.5))
    #par(mfcol=c(2,2))   # set up to plot 2 columns and 2 row of plots (first column for EOD and dvdt, second column for FFT)
    
    bigwave = multiplier*wave
    ilimit = min(which(bigwave >vP1))
    
    #MAIN wave plot
    #ftheeten 2020 04 04
    #(ggplot)
    full_wave_frame=data.frame(time,wave)
    colnames(full_wave_frame)<-c("time", "amplitude")
    big_wave_frame=data.frame(time[1:ilimit],bigwave[1:ilimit])
    colnames(big_wave_frame)<-c("time", "amplitude")
    tps= c(iT1, iT2)
    pks = c(iP0,iP1,iP2)
    slopes = c(is0,is1,is2,is3)
    wave_plot<-main_plot(paste0(species, " ", tagno), full_wave_frame, big_wave_frame, timebase, tps, pks, slopes, th)
    
   
    
    # PLOT the DERIVATIVE of the WAVE
    #ftheeten 2020 04 04
    #(ggplot)
    derivate_frame=data.frame(time,dvdt)
    colnames(derivate_frame)<-c("time", "amplitude")
    deriv_plot<-derivate_plot(paste0(species," ",tagno,"\r\n", "derivate") , derivate_frame, timebase, tps, slopes)
   
    
    # PLOT the FFT
    #ftheeten 2020 04 04
    #(ggplot)
    fft_frame=data.frame(f,Prel)
    colnames(fft_frame)<-c("freq", "power")
    freq_plot<-fft_plot(paste0(species," ",tagno,"\r\n", "Power Spectrum of EOD"),fft_frame, iPmax, ibandwidth)
    
    #extract folder of output file (to save the plot at the same location)
    print(output_file)
    base_path<-dirname(output_file)
    print(base_path)
   
    if(saveaspdf)
    {
      print(basename(fn))
      name_main_plot_png=paste0(base_path,"/",basename(fn),"_main_plot.png")
      name_main_plot_pdf=paste0(base_path,"/",basename(fn),"_main_plot.pdf")
      
      name_derivate_plot_png=paste0(base_path,"/",basename(fn),"_derivate_plot.png")
      name_derivate_plot_pdf=paste0(base_path,"/",basename(fn),"_derivate_plot.pdf")
      
      name_fft_plot_png=paste0(base_path,"/",basename(fn),"_freq_spectrum_plot.png")
      name_fft_plot_pdf=paste0(base_path,"/",basename(fn),"_fres_spectrum_plot.pdf")
      
      ggsave(name_main_plot_png, plot=wave_plot)
      ggsave(name_main_plot_pdf, plot=wave_plot)
      
      ggsave(name_derivate_plot_png, plot=deriv_plot)
      ggsave(name_derivate_plot_png, plot=deriv_plot)
      
      ggsave(name_fft_plot_png, plot=freq_plot)
      ggsave(name_fft_plot_pdf, plot=freq_plot)
    }
 
   
    i <-i+1
    #  print("END LOOP")

    }
print("EOF")



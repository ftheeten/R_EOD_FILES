# EOD Mormyroscope parser

This project aims to provide a R script to parse the metadata and wave information contained in the Mormyroscope files, whose format has been defined by Professor Carl Hopkins from Cornell University.

Its core component is the **eod_class.R** file , which contains two classes, implemented with the **RC** paradigm of R :

- Eod
- EodCluster

"**Eod**" maps the access to a sinfle Mormyroscope file and :

- extracts its metadata (first 12 lines) as a dataframe
- extracts the original wave as a dataframe with 2 columns (time and amplitude)
- normalizes the time and amplitude on a 1-basis
- finds the landmarks (T1, T2, peaks and valleys). Two parameters (that can be passed to the constructor) define the maximal number of peaks to detect (by default 7) and the treshold to detect T1 and T2 (default value : 2pc)
- detects the baseline level (mean level of the first silent part) by using the **changepoint** package. Fort methods are possible : "mean", "var", meanvar", "abitrary length" (in Mormyroscope time slots). Default value is "meanvar" which seems the most efficient (longer silent part)
- creates a periodgram (densities of frequencies) by using the **spectrum** function of the core R library 
- creates a plot (based of ggplot) the normalized wave with its landmarks
- creates a plot (based of ggplot) the normalized derivate (amplitude difference between a time slot and its precedessor)
- it can also reverse the phase and renormalized (in case of phase shifted at recording)

"**EodCluster**" allows accessing several EOD files at once and contains an array of EOD objects, that can be publicly accessed.
It alse contains 2 specific functions to :
- plot all the tiles with the three diagrams (normalized wave, derivate and periodgram)  as files in a given folder. The name of the file depends from the original CSV file
- create and save in a folder a single plot that merges (superimposes) all the wave plot (centered on 0) in a given folder

## 1 **EOD** API

### Constructor and default parameters

```
#Create an object without parameter
v <- Eod$new()
 
TO_BE_DOCUMENTED FOR PARAMETERS
```

### 1.1 Load an EOD object from a file

```
v$readFile("C:\\R_DEV_MORMY\\data_source\\MbisaCongo_MC-1013_A.csv")
#or with a Microsoft Windows file chooser
v$readFile(file.choose())
```

**Note: 
	- the object must be created with "new()" before calling "readFile()"
    - readFile uses the contructor to create the object
**

### 1.2 Get metadata (first 12 lines)

```
v$getMetadata()
#result is a dataframe
```

### 1.3 Get/set file metadata individually

Get parameters :
```
v$getSpecimenTag()
v$getProvisionalId()
v$getCollectionEvent()
v$getRecordingSoftware()
v$getRecordingHardware()
v$getRecordist()
v$getBitDepth()
v$getSamplingRate()
v$getRecordingDateTime()
v$getRecordingTemperature()
v$getRecordingConductivity()
v$getProjectName()
```

Modify parameters :
```
v$setSpecimenTag(<NEW_VALUE>)
v$setProvisionalId(<NEW_VALUE>)
v$setCollectionEvent(<NEW_VALUE>)
v$setRecordingSoftware(<NEW_VALUE>)
v$setRecordingHardware(<NEW_VALUE>)
v$setRecordist(<NEW_VALUE>)
v$setBitDepth(<NEW_VALUE>)
v$setSamplingRate(<NEW_VALUE>)
v$setRecordingDateTime(<NEW_VALUE>)
v$setRecordingTemperature(<NEW_VALUE>)
v$setRecordingConductivity(<NEW_VALUE>)
v$setProjectName(<NEW_VALUE>)
```

### 1.4 Get wave (unnomalized)

```
v$getWave()
```

### 1.5 Normalize

Calculate baseline and inflexion points and view the result in a frame :
```
getPossibleBaseline(<type>)
```
**type** parameter has possible values (linked to the "changepoint" package) :
- "mean" 
- "var"
- "meanvar" 
- "arbitrary"

**"meanvar"** is the default parameter. 
If **arbitrary** is chosen, the length (expressed in wave rows) of the first silent part has to be given by the user.
This can be defined by :
```
v$setArbitraryBaseLine(<integer>)
```
If not provided, the default value is 40.
This can also be accessed by : 
```
v$getArbitraryBaseLine()
```
Normalize with the chosen baseline :
```
v$chooseBaselineAndNormalize(type)
```
Default value of type is the previously chosen baseline (that can be also defined in the class constructor)

### 1.6 Get signal data

These functions returns data frame with two columns :
- position (row indexer)
- normalized time 
- normalized amplitude

```
v$getNormalizedWave()
v$getLandmarks()
v$getT1T2()
```

### 1.7 Inverse wave

```
v$inversePhase()
```

**Note** : this function inverse the amplitude of the original wave and performs a new normalization.

### 1.8 Get normalized wave diagram

```
v$getMainPlot()
```

### 1.9 Get derivate diagrams

```
v$getDerivatePlot()
```

### 1.10 Get periodgram

```
v$getPeriodgramPlot()
```

### 1.11 Save all the three diagrams

```
v$savePlots(<PATH_OF_DIRECTORY>)
```

With directory chooser on MS Windows :

```
v$savePlots(choose.dir())
```

**Note**
- the plot are generated with ggplot
- pattern
  - basename\_of\_csv\_file*_main_plot.png*
  - basename\_of\_csv\_file*_derivate_plot.png*
  - basename\_of\_csv\_file*_periodgram_plot.png*
  
## ** 2 EODCluster** API

**EODCluster** is a class gathering together a cluster of files. It can also generate a wave plot superimposing the normalized waves. 

### 2.1 Initialize
```
ec<-EodCluster$new(<ARRAY OF FILES>)
``` 
With Windows file chooser :

```
ec<-EodCluster$new(choose.files())
```
### 2.2 Get an EOD object inside of the cluster
```
ec$getEODS(<INDEX position>)
```
E.g.  get EOD object corresponding to the second file
```
ec$getEODS(2)
```

The resulting variables is an EOD object whose methods are available (see documentation above) :
```
obj<-ec$getEODS(2)
View(obj$getMetadata())
obj$getPossibleBaseline()
obj$getPeriodgram()
```

Shorter syntax :
```
View(ec$getEODS(2)$getMetadata())
```

### 2.3 Save all plots
```
ec$saveAllPlots(<DIRECTORY_PATH>)
```

With Windows directory chooser

```
ec$saveAllPlots(choose.dir())
```

### 2.4 Create plot merging normalized waves
```
ec$superimposePlots(<DIRECTORY_PATH>)
```

With Windows directory chooser

```
ec$superimposePlots(choose.dir())
```

**Author : Franck Theeten (Royal Museum for Central Africa) franck.theeten@africamuseum.be ** 
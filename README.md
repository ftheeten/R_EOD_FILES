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

## **EOD** API

### Constructor and default parameters

```
#Create an object without parameter
v <- Eod$new()
 
TO_BE_DOCUMENTED FOR PARAMETERS
```

### Load an EOD object from a file

```
v$readFile("C:\\R_DEV_MORMY\\data_source\\MbisaCongo_MC-1013_A.csv")
#or with a Microsoft Windows file chooser
v$readFile(file.choose())
```

**Note: 
	- the object must be created with "new()" before calling "readFile()"
    - readFile uses the contructor to create the object
**

### Get metadata (first 12 lines)

```
v$getMetadata()
#result is a dataframe
```

### Get/Set file metadata individually

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

### Get wave (unnomalized)

```
v$getWave()
```

### Normalize

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

### Get signal data

These functions returns data frame with two columns :
- position (row indexer)
- normalized time 
- normalized amplitude

```
v$getNormalizedWave()
v$getLandmarks()
v$getT1T2()
```

### Reverse phase

```
v$inversePhase()
```

**Note** : this function inverse the amplitude of the original wave and performs a new normalization.

### get normalized wave diagram

```
v$getMainPlot()
```

### get derivate diagrams

```
v$getDerivatePlot()
```

### get periodgram

```
v$getPeriodgramPlot()
```

### save all the three diagrams

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
  - <basename of csv file>*_main_plot.png*
  - <basename of csv file>*_derivate_plot.png*
  - <basename of csv file>*_periodgram_plot.png*
  
## **EODCluster** API

 

**Author Franck Theeten (Royal Museum for Central Africa)** 
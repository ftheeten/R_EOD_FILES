#EOD Mormyroscope parser

This project aims to provide a R script to parse the metadata and wave information contained in the Mormyroscope files, whose format has been defined by Professor Carl Hopkins from Cornell University.

Its core component is the **eod_class.R** file , which contains two classes, implemented with the **RC** paradigm of R :

- Eod
- EodCluster

"**Eod**" maps the access to a sinfle Mormyroscope file and :

- extracts its metadata (first 12 lines) as a dataframe
- extracts the original wave as a dataframe with 2 columns (time and amplitude)
- normalize the time and amplitude on a 1-basis
- finds the landmarks (T0, T1, peaks and valleys). A parameter (that can be passed to the constructor) fixes the maximal number of peaks to detect (by default 7) and the treshold to detect T0 and T1 (default value : 2pc)



```
```
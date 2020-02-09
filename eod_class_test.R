source("eod_class.R")
#print(EodObj$fields)
"
v <- Eod$new(specimenTag=\"test\")

print(v$getSpecimenTag())

v$readFile(\"C:\\R_DEV_MORMY\\data_source\\MbisaCongo_MC-1013_A.csv\")
print(v$getSpecimenTag())
View(v$getMetadata())
View(v$getWave())
#inverse
#v$inversePhase()
View(v$getWave())
View(v$getAbsoluteValues())
View(v$getPossibleBaseline())
print(v$getMainPlot())
print(v$getDerivatePlot())
View(v$getLandmarks())
View(v$getT1T2())
print(v$getPeriodgramPlot())
v$savePlots(choose.dir())
"
v<-EodCluster$new(choose.files())
tmp<-v$getEODS(1)
View(tmp$getMetadata)
#v$saveAllPlots(choose.dir())
v$superimposePlots(choose.dir())
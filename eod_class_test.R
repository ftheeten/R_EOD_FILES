source("eod_class.R")
#print(EodObj$fields)

v <- Eod$new(specimenTag="test")

print(v$getSpecimenTag())

v$readFile("C:\\R_DEV_MORMY\\data_source\\MbisaCongo_MC-1013_A.csv")
print(v$getSpecimenTag())
View(v$getMetadata())
View(v$getWave())
View(v$getAbsoluteValues())
View(v$getPossibleBaseline())
print(v$getMainPlot())
print(v$getDerivatePlot())
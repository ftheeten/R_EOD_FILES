source("eod_class.R")
#print(EodObj$fields)

v <- Eod$new(specimenTag="test")

print(v$getSpecimenTag())

v$readFile("D:\\ftheeten\\BICS\\R_DEV_CORNELL\\TRAINING2020\\data\\MbisaCongo_MC-1006_A.csv")
print(v$getSpecimenTag())
View(v$getMetadata())
View(v$getWave())
View(v$getPossibleBaseline())
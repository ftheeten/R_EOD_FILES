source("eod_class.R")


v <- Eod$new(specimenTag="")

print(v$getSpecimenTag())

v$readFile(file.choose())
print(v$getSpecimenTag())
View(v$getMetadata())
View(v$getWave())
#inverse
v$inversePhase()
View(v$getWave())
View(v$getAbsoluteValues())
View(v$getPossibleBaseline())
print(v$getMainPlot())
print(v$getDerivatePlot())
View(v$getLandmarks())
View(v$getT1T2())
print(v$getPeriodgramPlot())
v$savePlots(choose.dir())

v<-EodCluster$new(choose.files())
tmp<-v$getEODS(1)
View(tmp$getMetadata())
v_dir<-choose.dir()
v$saveAllPlots(v_dir)
v$superimposePlots(v_dir)

<<<<<<< HEAD
v$getSpecimenList()
v$superimposePlots(choose.dir(), "Mbisa Congo_HC001_MC-1013")
v$superimposePlots(choose.dir(), "Mbisa Congo_HC002_MC-1006")
=======
#change normalization
tmp<-v$getEODS(1)
tmp$setArbitraryBaseline(60)
tmp$getPossibleBaseline(type="arbitrary")
print(tmp$getMainPlot())
>>>>>>> e2f5207b2a89d57a0b0e56f1b523ee35913bd8ea


# ECSASatlas

*Building pelagic bird density maps using ECSAS data.*


### MODELS

- Add variance on p (though it's negligeable compared to var on density...)
- Add covariates
- Add plane surveys

### TODO list on maps

- Add 2012-2013 QC data
- There is a hack in the building of classes of density and CV to make sure the number of classes is always the same despite the fact that there could be fewer values than the number of classes. Otherwise, classIntervals gives non-sense or there is a bug in the layout of the map


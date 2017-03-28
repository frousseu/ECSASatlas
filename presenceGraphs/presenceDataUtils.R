
# GROUP_NAMES_FR <-
#   as.list(enc2utf8(
#     c(
#       Terns = "sternes",
#       Fulmars = "fulmars",
#       Shearwaters = "puffins",
#       "Storm-Petrels" = "océanites",
#       "Diving Waterfowl" = "canards plongeurs",
#       Phalaropes = "phalaropes",
#       Jaegers = "petits labbes",
#       Skuas = "grands labbes",
#       Alcids = "alcidés",
#       Gulls = "goélands et mouettes",
#       Gannets = "fous de bassan",
#       Murres = "guillemots marmette et marmettes"
#     )
#   ))

### get data for french names from EC's official list on my github
SPECIES_NAMES <- fread("presenceCalendar_groups.csv", encoding = "UTF-8")

SPECIES_NAMES$display_en <- tolower(SPECIES_NAMES$group_atlas)
SPECIES_NAMES$display_fr <- tolower(SPECIES_NAMES$group_atlas_fr)

SPECIES_NAMES[Alpha %in% c("NOFU", "NOGA"), display_en := c("Nothern Fulmars", "Nothern Gannets")]
SPECIES_NAMES[Alpha %in% c("NOFU", "NOGA"), display_fr := c("Fulmars boréaux", "Fous de Bassan")]

  # read.csv(
  #   file.path("EC_AVIAN_CORE_20161216.csv"),
  #   header = TRUE,
  #   stringsAsFactors = FALSE
  # )[, c("Species_ID", "English_Name", "French_Name")]

## Add exclusions


getPresenceGroups <-
  function(extract = FALSE,
           loadNew = FALSE,
           presenceData = "",
           outputFile = "",
           pathSOMEC = "",
           fileSOMEC ="",
           pathECSAS = "",
           fileECSAS = "",
           groupsFile = "", 
           cols = c(
             "Date",
             "Year",
             "Month",
             "Week",
             "Alpha",
             "Count",
             "group_detection",
             "group_atlas",
             "WatchLenKm",
             "English",
             "French",
             "WatchID"
           )) {
     
    if (!extract) {
      load(presenceData)
    } else {
        ecsas <-
          ECSAS.extract(
            lat = c(39.33489, 74.65058),
            long = c(-90.50775, -38.75887),
            ecsas.drive = pathECSAS,
            ecsas.file = fileECSAS
          )
        addQC <-
          SOMEC2ECSAS(
            input = file.path(pathSOMEC, fileSOMEC),
            output = paste0(pathSOMEC, "/ECSASexport.csv"),
            date = "1011-11-02",
            typeSaisie = "ordi",
            step = "5 min",
            spNA = FALSE
          )
        groups <- read.csv(
          groupsFile,
          header = TRUE,
          stringsAsFactors = FALSE
        )
        
        names(addQC) <- gsub("Orig", "", names(addQC))
        
        # For now, elements without WatchID are scrapped,
        # it may be due to a bug in the SOMEC2ECSAS function
        # or missing data in the original files. Check for that
        # and add a comment in the TODO list of ECSASconnect
        addQC <- addQC[!is.na(addQC$WatchID), ]
        
        ### Replace french names and wrong names in Quebec data
        ### There are about 15 observations without a count number
        # table(addQC$Alpha[is.na(addQC$Count)], useNA = "always")
        db <-
          odbcConnectAccess2007(file.path(pathSOMEC, fileSOMEC))
        qc <- sqlFetch(db, "Code espèces", as.is = TRUE)
        odbcClose(db)
        
        badNames <-
          c(
            "foba",
            "fubo",
            "fulm",
            "GOAC",
            "goar",
            "guma",
            "LALQ",
            "LIMICOLESP",
            "OCWL",
            "PATC",
            "PLON",
            "RAZO",
            "rien",
            "SCSP"
          )
        goodNames <-
          c(
            "FOBA",
            "FUBO",
            "FUBO",
            "GOAC",
            "GOAR",
            "GUMA",
            "LALQ",
            "LIMICOLESP",
            "OCWI",
            "PATC",
            "PLON",
            "PEPI",
            "RIEN",
            "SCSP"
          )
        
        m <- match(addQC$Alpha, badNames)
        mna <- which(!is.na(m))
        addQC$Alpha[mna] <- goodNames[m[mna]]
        
        m <- match(addQC$Alpha, qc$CodeFR)
        mna <- which(!is.na(m))
        addQC$Alpha[mna] <- qc$CodeAN[m]
        
        # pour verifier les lignes restantes mult_list la fin
        addQC$add <- 1
        nad <- which(is.na(addQC$Date))
        addQC$Date[nad] <- format(addQC$StartTime[nad], "%Y-%m-%d")
        
        m <- match(addQC$Alpha, ecsas$Alpha)
        addQC$English <- ""
        mna <- which(!is.na(m))
        addQC$English[mna] <- ecsas$English[m[mna]]
        
        d <- join(ecsas, addQC, type = "full")
        
        # Remove all observations without birds
        d <- d[]
        d$Alpha[is.na(d$Alpha) |
                  d$Alpha %in% c("RIEN", "NOBI")] <- ""
        d$English <- gsub("Storm Petrel", "Storm-Petrel", d$English)
        d$English[is.na(d$English)] <- ""
        # d$French <-
        #   SPECIES_NAMES$French_Name[match(d$English, SPECIES_NAMES$English_Name)]
        d$Distance[is.na(d$Distance)] <- ""
        
        # on enleve la croisiere vers l'europe pour eviter des distortions dans les projections
        d <- d[which(d$LongStart > (-150) & d$LongStart < (-18)), ]
        d <- d[which(d$LatStart > (0) & d$LatStart < (90)), ]
        
        d <-
          mcds.filter(d, dist2m = FALSE, distanceLabel.field = "DistanceCode")
        
        # d$Date <- as.Date(d$Date, format = "%Y-%m-%d")
        d <-
          d[order(d$CruiseID,
                  d$WatchID,
                  d$Date,
                  substr(d$StartTime, 12, 19)), ]
        d$Month <- substr(d$Date, 6, 7)
        month_comb <- c("12010203", "04050607", "08091011")
        d$MonthC <- month_comb[sapply(d$Month, function(i) {
          g <- grep(i, month_comb)
          if (length(g) > 1) {
            if (i == "01") {
              g <- g[1]
            }
            if (i == "10") {
              g <- g[2]
            }
          }
          g
        })]
        
        # d$English[grep("Genus: Gulls", d$English)] <- "Genus: Gulls"
        
        
        m <- match(d$English, groups$sp)
        # If no species are in English because of an empty transect, we don't want ;a group name
        d$group_detection <-
          ifelse(!d$English %in% c("", NA), groups$group_detection[m], "")
        d$group_atlas <-
          ifelse(!d$English %in% c("", NA), groups$group_atlas[m], "")
        
        # Check what does not have a group name to make sure it is not an important species
        empty <- is.na(d$group_detection) | d$group_detection == ""
        sort(table(d$English[empty]))
        empty <- is.na(d$group_atlas) | d$group_detection == ""
        sort(table(d$English[empty]))
        
        # Turn what does not have a group to empty values in species, distance, groups and count
        k1 <-
          !d$Alpha %in% c("", NA) & d$group_detection %in% c("", NA)
        k2 <- !d$Alpha %in% c("", NA) & d$group_atlas %in% c("", NA)
        
        d$Alpha <- ifelse(k1 & k2, "", d$Alpha)
        d$English <- ifelse(k1 & k2, "", d$English)
        d$Count <- ifelse(k1 & k2, "", d$Count)
        d$Distance <- ifelse(k1 & k2, "", d$Distance)
        d$group_detection <- ifelse(k1 & k2, "", d$group_detection)
        d$group_atlas <- ifelse(k1 & k2, "", d$group_atlas)
        
        # Keep only data with species
        
        presenceData <- d
        
        save(presenceData, file = outputFile)
    }
    
    presenceData$Count <- as.numeric(presenceData$Count)
    return(presenceData[, cols])
  }


prepareData <-
  function(data,
           groupBy,
           subset = NULL,
           speciesColumn = "Alpha",
           countColumn = "Count",
           effortColumn = "WatchLenKm",
           effort = NULL,
           emptyEffort = NA,
           allowedGroups = c(
             "Alpha",
             "group_atlas",
             "group_detection",
             "Year",
             "Month",
             "Week",
             "English",
             "French"
           ),
           ...) {
    # Keep only species we are interested in
    spData <- data.table(data)
    
    spData[, Date := as.Date(Date, format = "%Y-%m-%d")]
    # Remove lines without observations
    if (!is.null(spData$Date)) {
      spData <- spData[!is.na(Date)]
    }
    # Subset data based on the subset list
    if (!is.null(subset)) {
      setkeyv(spData, names(subset))
      query <-
        paste0(lapply(seq_along(subset), function(i, subset) {
          var <- names(subset)[i]
          value <- paste0("\"", subset[[i]], "\"", collapse = ",")
          query <- paste0(var, " %in% c(", value, ")")
        }, subset), collapse = " & ")
      # str(query)
      query <- parse(text = query)
      
      spData <- spData[eval(query)]
    }
    
    # Restrict variables with which we can group
    groupBy <-
      groupBy[groupBy %in% allowedGroups]
    setkeyv(spData, groupBy)
    
    # Make sure there is at least one row for each combination
    fillRowsBy <- list()
    for (var in groupBy) {
      fillRowsBy <- c(fillRowsBy, list(switch(
        var,
        Alpha = unique(spData$Alpha),
        Month = 1:12,
        Week = 1:52,
        Year = min(spData$Year):max(spData$Year),
        group_atlas = unique(spData$group_atlas),
        group_detection = unique(spData$group_detection),
        English = unique(spData$English),
        French = unique(spData$French)
      )))
    }
    fullRows <- do.call(CJ, fillRowsBy)
    spData <- spData[fullRows]
    setkeyv(spData, groupBy)
    
    # Get columns that concern time only
    timeGroup <- groupBy[!groupBy %in% speciesColumn]
    # Create a unique id column
    spData[, timeseq := do.call(paste, c(mget(timeGroup), sep = "_"))]
    
    # Convert Efforts to numeric and replace NA with 0
    spData <-
      spData[, c(effortColumn) := as.numeric(get(effortColumn))][is.na(get(effortColumn)), c(effortColumn) := 0]
    # Convert Count to numeric and replace NA with 0
    spData <-
      spData[, c(countColumn) := as.numeric(get(countColumn))][is.na(get(countColumn)), c(countColumn) := 0]
    
    # Agreggation variables
    countBy <- "timeseq"
    if (speciesColumn %in% groupBy) {
      countBy <- c(speciesColumn, countBy)
    }
    
    if (is.null(effort)) {
      # Calculate effort if needed
      effort <-
        spData[, .(effort = sum(get(effortColumn))), by = timeseq]
    } else {
      effort[, timeseq := do.call(paste, c(mget(timeGroup), sep = "_"))]
    }
    
    ## Create the final aggregated dataset
    res <- spData[, .(nobs = sum(get(countColumn))), by = countBy]
    # Add effort column to dataset
    setkey(effort, timeseq)
    setkey(res, timeseq)
    res <- merge(res, effort, all.x = TRUE)
    
    # Replace empty effort by provided value
    res[effort == 0 | is.na(effort), effort := emptyEffort]
    
    # Calculate density
    res[, density := nobs / effort]
    res[is.nan(density), density := 0]
    
    ## If the time sequence is created from more than one colum, create a unique
    ## id for each row
    if (length(timeGroup) > 1) {
      res[, idx := 1:.N]
    } else {
      ## Else convert the time sequence as numeric
      res[, idx := as.numeric(timeseq)]
    }
    res
  }


getSpeciesData <-
  function(data,
           speciesList,
           speciesColumn,
           effort,
           nmin) {
    # Iterate on each species to create presence graph
    speciesData <- lapply(speciesList, function(species, data) {
      subset <- species
      names(subset) <- speciesColumn
      graphData <- prepareData(
        data,
        subset = subset,
        groupBy = c(speciesColumn, "Week"),
        speciesColumn = speciesColumn,
        effort = effort
      )
    }, data)
    
    speciesData <- do.call(rbind, speciesData)
    speciesData
  }

getGroupData <-
  function(group,
           data,
           speciesColumn,
           groupColumn,
           effort,
           nmin,
           bySpecies) {
    data <- data[get(groupColumn) %in% group]
    # Keep only species with more than nmin observations
    if (bySpecies) {
      species <-
        data[, .(obs = base::sum(as.numeric(.N), na.rm = TRUE)), by = speciesColumn]
      speciesList <- species[species$obs > nmin, get(speciesColumn)]
      
      speciesData <-
        getSpeciesData(data, speciesList, speciesColumn, effort, nmin)
      list(species = speciesList,
           data = speciesData,
           speciesCount = species)
    } else {
      groupData <- prepareData(
        data,
        groupBy = c(speciesColumn, "Week"),
        speciesColumn = speciesColumn,
        effort = effort
      )
      list(data = groupData)
    }
  }

cleanData <- function(presenceData, effortBy) {
  presenceData <- data.table(presenceData)
  
  # Make sure date columns are ok
  presenceData[, Date := as.Date(Date, format = "%Y-%m-%d")]
  presenceData[, Year := as.numeric(format(Date, format = "%Y"))]
  presenceData[, Week := as.numeric(format(Date, format = "%W"))]
  
  # Get combined effort for all data
  watches <- unique(presenceData, by = "WatchID")
  effort <- watches[, .(effort = sum(WatchLenKm)), by = effortBy]
  setkey(effort, Week)
  setkey(presenceData, Week)
  
  # Remove unwanted groups or species
  presenceData <-
    presenceData[-grep("Genus:|Family:| or ", presenceData$English), ]
  
  
  if (exists("EXCLUDE_GROUPS")) {
    presenceData <- presenceData[!group_atlas %in% EXCLUDE_GROUPS]
  }
  
  if (exists("DISPLAY_GROUP")) {
    lapply(seq_along(DISPLAY_GROUP), function(i, presenceData) {
      presenceData[group_atlas %in% DISPLAY_GROUP[[i]], display_group := i]
    }, presenceData)
  }
  
  list(data = presenceData, effort = effort)
}

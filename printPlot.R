print_atlas <- function(densities, dl) {
  for (i in seq_along(month_comb)) {
    group <- month_comb[i]
    png(
      paste0(pathMAPS, "/global_", group, ".png"),
      width = 6,
      height = 4.8,
      units = "in",
      res = 600
    )
    
    dat <- dl[[group]]
    dens <- densities[Month == group]
    grid$val <- dens$Estimates[match(grid$id, dens$Region)]
    # grid$u <- dens$"95% Upper"[match(grid$id, dens$Region)]
    # grid$l <- dens$"95% Lower"[match(grid$id, dens$Region)]
    # grid$u <-
    # ifelse(grid$u > (2 * max(grid$val, na.rm = TRUE)), 2 * max(grid$val, na.rm =
    # TRUE), grid$u)
    grid$cv <- dens$cv[match(grid$id, dens$Region)]
    # grid$diff <- grid$u - grid$l
    count <-
      ddply(dl[[group]], .(cell), function(i) {
        round(sum(as.numeric(i$Count), na.rm = TRUE) / sum(i$WatchLenKm[!duplicated(i$WatchID)], na.rm =
                                                             TRUE),
              1)
      })
    count <-
      ddply(dl[[group]], .(cell), function(i) {
        sum(as.numeric(i$Count), na.rm = TRUE)
      })
    count <-
      ddply(dl[[group]], .(cell), function(i) {
        paste(sum(as.numeric(i$Count), na.rm = TRUE), round(sum(i$WatchLenKm[!duplicated(i$WatchID)], na.rm =
                                                                  TRUE), 0), collapse = "\n")
      })
    grid$count <- count$V1[match(grid$id, count$cell)]
    grid$nbsamp <- dens$"nbsamp"[match(grid$id, dens$Region)]
    
    r <- range(c(grid$val), na.rm = TRUE)
    
    ### CREATE BRAKES FOR DENSITIES
    val <- grid$val
    val <- val[!is.na(val)]
    if (length(unique(val)) < length(cols)) {
      # if the number of values is below the length of cols, classIntervals returns non-sense
      val <- sort(c(val, seq(
        min(val), max(val), length.out = length(cols)
      )))
    }
    br <-
      suppressWarnings(classIntervals(
        val,
        n = length(cols),
        style = "kmeans",
        rtimes = 1
      )$brks)
    br <- ifelse(br < 0, 0, br)
    
    ### ADD VALUES OF COLORS TO GRID
    grid$col <- colo.scale(c(r, grid$val), cols = cols, breaks = br)[-(1:2)]
    grid$col <- alpha(ifelse(is.na(grid$cv), NA, grid$col), trans)
    # grid$colu <- colo.scale(c(r, grid$u), cols = cols, breaks = br)[-(1:2)]
    # grid$colu <- alpha(grid$colu, ifelse(is.na(grid$colu), 1, trans))
    # grid$coll <- colo.scale(c(r, grid$l), cols = cols, breaks = br)[-(1:2)]
    # grid$coll <- alpha(grid$coll, ifelse(is.na(grid$coll), 1, trans))
    
    ### CREATE BREAKS FOR CV (brcv is global and produced before the loop)
    ncv <- length(brcv) - 1
    cexcv <- seq(0.35, 1, length.out = 4)
    cutcv <- cut(grid$cv, breaks = brcv)
    grid$cutcv <- cutcv
    grid$cex <- cexcv[as.numeric(cutcv)] * mag
    
    ### BUILD GRID WITH HOLES
    holes <-
      gBuffer(
        SpatialPoints(coordinates(grid), proj4string = CRS(proj4string(grid))),
        width = ifelse(is.na(grid$cex), 0, grid$cex * 40000),
        byid = TRUE
      ) # the buffer width value needs to be adjusted manually with the cex of the legend
    gholes <- gIntersection(gDifference(grid, holes), grid, byid = TRUE)
    
    
    ### PLOT
    par(mar = c(0, 0, 0, 0), mgp = c(0.5, 0.1, 0))
    k <- !is.na(grid$val)
    #plot(grid,bg="#7AAFD1",border="#7AAFD1",xaxs="i",yaxs="i",xlim=c(-1848241,3712932),ylim=c(-1413652,3035287))
    plot(
      1,
      1,
      type = "n",
      xaxs = "i",
      yaxs = "i",
      xlim = c(-1848241, 3712932),
      ylim = c(-1413652, 3035287)
    )
    boxcut <- par("usr")
    
    
    ### plot bathymetry shading
    lb <- c(75, 72, 69, 66, 63, 60, 57, 54) - 5 #c(70,65,60,55,50,45,42,39)
    plot(b0,
         col = hcl(240, 50, lb[1]),
         border = NA,
         add = TRUE)
    plot(b200,
         col = hcl(240, 50, lb[2]),
         border = NA,
         add = TRUE)
    plot(b1000,
         col = hcl(240, 50, lb[3]),
         border = NA,
         add = TRUE)
    plot(b2000,
         col = hcl(240, 50, lb[4]),
         border = NA,
         add = TRUE)
    plot(b3000,
         col = hcl(240, 50, lb[5]),
         border = NA,
         add = TRUE)
    plot(b4000,
         col = hcl(240, 50, lb[6]),
         border = NA,
         add = TRUE)
    plot(b5000,
         col = hcl(240, 50, lb[7]),
         border = NA,
         add = TRUE)
    plot(b6000,
         col = hcl(240, 50, lb[8]),
         border = NA,
         add = TRUE)
    pb <- hcl(240, 50, lb[1])
    db <- hcl(240, 50, lb[7])
    
    
    ### draw latitudes and longitudes
    plot(
      lat,
      add = TRUE,
      col = "grey20",
      pch = 16,
      cex = 0.01
    )
    plot(
      lon,
      add = TRUE,
      col = "grey20",
      pch = 16,
      cex = 0.01
    )
    
    
    ### plot grid and values
    plot(
      eu,
      add = TRUE,
      lwd = 0.1,
      border = NA,
      col = "grey75"
    )
    plot(
      na,
      add = TRUE,
      lwd = 0.1,
      border = NA,
      col = "grey75"
    )
    
    ### PLOT GRID WITH HOLES OR NOT
    plot(
      gholes[k],
      col = grid$col[k],
      border = ifelse(is.na(grid$col[k]), alpha("lightblue", trans), NA),
      add = TRUE,
      lwd = 0.5
    )
    
    ### PLOT unvisited holes in grid
    grid[k, ] %>%
      gUnaryUnion %>%
      slot("polygons") %>%
      extract2(1) %>%
      slot("Polygons") %>%
      sapply(function(i) {
        !slot(i, "hole")
      }) ->
      keep
    
    test <- grid[k, ] %>%
      gUnaryUnion %>%
      slot("polygons") %>%
      extract2(1) %>%
      slot("Polygons") %>%
      magrittr:::extract(keep) %>%
      Polygons(ID = 1) %>%
      list %>%
      SpatialPolygons ->
      chgrid
    
    proj4string(chgrid) <- proj4string(grid)
    pp <- SpatialPoints(coordinates(grid2), CRS(proj4string(grid2)))
    k1 <- !is.na(over(pp, chgrid))
    k2 <- over(pp, grid[k, ])
    k2 <- apply(k2, 1, function(j) {
      all(is.na(j))
    })
    wh <- which(k1 & k2)
    plot(
      pp[wh],
      add = TRUE,
      col = "lightblue",
      cex = 0.35,
      pch = 4
    )
    
    
    ### plot shapefiles
    plot(
      eu,
      add = TRUE,
      lwd = 0.5,
      border = "grey55",
      col = alpha("grey75", 0.65)
    )
    plot(
      na,
      add = TRUE,
      lwd = 0.5,
      border = "grey55",
      col = alpha("grey75", 0.65)
    )
    plot(
      gl,
      col = pb,
      lwd = 0.5,
      border = "grey55",
      add = TRUE
    )
    
    ### plot bathymetry lines to go over cells
    plot(
      gUnionCascaded(b200),
      border = alpha("black", 0.2),
      add = TRUE,
      lwd = 0.5
    )
    plot(
      gUnionCascaded(b1000),
      border = alpha("black", 0.2),
      add = TRUE,
      lwd = 0.5
    )
    plot(b2000,
         border = alpha("black", 0.2),
         add = TRUE,
         lwd = 0.5)
    plot(b3000,
         border = alpha("black", 0.2),
         add = TRUE,
         lwd = 0.5)
    plot(b4000,
         border = alpha("black", 0.2),
         add = TRUE,
         lwd = 0.5)
    plot(b5000,
         border = alpha("black", 0.2),
         add = TRUE,
         lwd = 0.5)
    plot(b6000,
         border = alpha("black", 0.2),
         add = TRUE,
         lwd = 0.5)
    
    ### SPECIES INFO BOX
    rect(
      1100000,
      1560000,
      3800000,
      3100000,
      col = alpha("white", 0.4),
      border = NA
    )
    rect(
      1100000,
      2680000,
      3800000,
      3100000,
      col = alpha("white", 0.25),
      border = NA
    )
    rect(
      1100000,
      2240000,
      3800000,
      2520000,
      col = alpha("white", 0.25),
      border = NA
    )
    rect(
      2450000,
      -1000000,
      3650000,
      1225000,
      col = alpha("white", 0.4),
      border = NA
    )
    
    ### add this to keep lines in the east of greenland, but not overwrite cells in the west
    hide <-
      gIntersection(gr, bbox2pol(c(1100000, 3800000, 1560000, 3100000), proj4string =
                                   laea)) # plot over the topright box to hide it but not hide cells in the west of greenland
    plot(hide,
         add = TRUE,
         border = NA,
         col = "grey75")
    plot(gr,
         add = TRUE,
         lwd = 0.5,
         border = "grey55")
    
    ### replot lat and lon over land to make them darker on land
    plot(
      lat[olat],
      add = TRUE,
      col = "grey30",
      pch = 16,
      cex = 0.01
    )
    plot(
      lon[olon],
      add = TRUE,
      col = "grey30",
      pch = 16,
      cex = 0.01
    )
    
    ### PLOT DENSITY LEGEND
    se <-
      paste(format(round(br[-length(br)], 1), nsmall = 1, digits = 0),
            format(round(br[-1], 1), nsmall = 1, digits = 0),
            sep = " - ")
    lcols <- alpha(cols, trans)
    deleg <-
      c("0", paste0(c(">", rep("", length(
        se
      ) - 1)), if (is.numeric(se)) {
        round(se, 0)
      } else{
        se
      }))
    deleg <- paste(deleg, c("/ not visited (  )", rep("", length(deleg) - 1)))
    l <-
      legend(
        2500000,
        1100000,
        adj = c(0, 0.5),
        title.adj = 0,
        legend = rep("", length(deleg)),
        y.intersp = 1.2,
        bty = "n",
        title = "Density (birds / km\U00B2)\nDensité (oiseaux / km\U00B2)",
        cex = tex * 1
      )
    for (j in seq_along(l$text$x)) {
      X <- l$text$x[j]
      Y <- l$text$y[j]
      shift <-
        c(X - coordinates(hex)[1, 1], Y - coordinates(hex)[1, 2]) - c(400000, -7000)
      col <- c(NA, lcols)[j]
      bord <- c("lightblue", rep(NA, length(lcols)))[j]
      e <-
        elide(hex, shift = shift, rotate = 0) #la valeur de rotate doit être ajustée à la mitaine en fonction de la cellule choisie hex
      plot(
        e,
        col = col,
        add = TRUE,
        border = bord,
        lwd = 0.5
      )
      text(
        coordinates(e)[, 1] + 100000,
        coordinates(e)[, 2],
        label = deleg[j],
        cex = tex * 1,
        adj = c(0, 0.5)
      )
      if (j == 1) {
        width <- strwidth(deleg[j], cex = tex * 1)
        points(
          coordinates(e)[, 1] + 100000 + width - 56000,
          coordinates(e)[, 2] - 7000,
          pch = 4,
          cex = 0.35,
          col = "lightblue"
        )
      }
    }
    sca <- bbox(e) # needed for scale position
    
    
    ### PLOT CV LEGEND
    cvleg <-
      strsplit(gsub("\\)|\\(|\\]|\\[", "", gsub(",", " - ", levels(cutcv))), " - ")
    cvleg <-
      c("N/A (n = 1)", sapply(cvleg, function(k) {
        paste(gsub(" ", "", k), collapse = " - ")
      }))
    l <-
      legend(
        2500000,
        -110000,
        adj = c(1, 0.5),
        title.adj = 0,
        y.intersp = 1.2,
        legend = rep("", length(cvleg)),
        bty = "n",
        title = "Coefficient of variation (%)\nCoefficient de variation (%)",
        cex = tex * 1
      )
    for (j in seq_along(l$text$x)) {
      Y <- l$text$y[j]
      shift <-
        c(X - coordinates(hex)[1, 1], Y - coordinates(hex)[1, 2]) - c(400000, -7000)
      bord <- c("lightblue", rep(NA, length(lcols)))[j]
      width <-
        c(0.001, cexcv)[j] * 40000 # on ajoute une explication et un "fake" buffer de 0.001 pour illustrer l'absence de trous et sa signification
      hhex <-
        gBuffer(
          SpatialPoints(coordinates(hex), proj4string = CRS(proj4string(grid))),
          width = width,
          byid = TRUE
        ) # the buffer width value needs to be adjusted manually with the cex of the legend
      hexholes <- gIntersection(gDifference(hex, hhex), hex, byid = TRUE)
      e <- elide(hexholes, shift = shift, rotate = 0)
      plot(
        e,
        col = alpha("white", trans),
        add = TRUE,
        border = NA,
        lwd = 1
      )
      text(
        coordinates(e)[, 1] + 100000,
        coordinates(e)[, 2],
        label = cvleg[j],
        cex = tex * 1,
        adj = c(0, 0.5)
      )
    }
    
    
    ### GET THE DIFFERENT NAMES
    gg <- unlist(strsplit(group, "\\."))
    if (nchar(gg[1]) == 4) {
      span <- d$English[match(gg[1], d$Alpha)]
      splat <- as.character(d$Latin[match(gg[1], d$Alpha)])
      spfr <- d$French[match(gg[1], d$Alpha)]
    } else{
      span <- unname(groupn[[match(gg[1], names(groupn))]][1])
      splat <- "  "
      spfr <- unname(groupn[[match(gg[1], names(groupn))]][2])
    }
    # mmonth <- match(strsplit(group, "\\.")[[1]][2], month_comb)
    mmonth <- match(group, month_comb)
    
    text(
      1600000,
      2940000,
      span,
      font = 2,
      adj = c(0, 0.5),
      cex = tex * 1.4
    )
    text(
      1600000,
      2790000,
      spfr,
      font = 2,
      adj = c(0, 0.5),
      cex = tex * 1.4
    )
    text(
      3600000,
      2940000,
      unlist(strsplit(splat, " "))[[1]],
      font = 3,
      adj = c(1, 0.5),
      cex = tex * 1.1
    )
    text(
      3600000,
      2790000,
      unlist(strsplit(splat, " "))[[2]],
      font = 3,
      adj = c(1, 0.5),
      cex = tex * 1.1
    )
    
    text(
      1600000,
      2440000,
      "No. of records / Nb. de mentions :",
      adj = c(0, 0.5),
      cex = tex
    ) #diff de 120000
    text(
      1600000,
      2320000,
      "Sample size / Taille d'échantillon :",
      adj = c(0, 0.5),
      cex = tex
    )
    text(
      3600000,
      2440000,
      sum(dat$Count != "", na.rm = TRUE),
      adj = c(1, 0.5),
      cex = tex
    )
    text(3600000,
         2320000,
         length(unique(dat$SMP_LABEL)),
         adj = c(1, 0.5),
         cex = tex)
    
    wmonthEN <-
      c("Jan",
        "Feb",
        "Mar",
        "Apr",
        "May",
        "Jun",
        "Jul",
        "Aug",
        "Sep",
        "Oct",
        "Nov",
        "Dec")
    wmonthFR <-
      c("Jan",
        "Fév",
        "Mar",
        "Avr",
        "Mai",
        "Jun",
        "Jul",
        "Aoû",
        "Sep",
        "Oct",
        "Nov",
        "Déc")
    nmonth <- 1:12
    se <- seq(1600000, 3500000, length.out = 12)
    text(
      se,
      2630000,
      wmonthEN,
      col = ifelse(nmonth %in% monthNB[[mmonth]], cols[length(cols)], "grey55"),
      font = ifelse(nmonth %in% monthNB[[mmonth]], 2, 1),
      cex = 0.35,
      adj = c(0, 0.5)
    )
    text(
      se,
      2580000,
      wmonthFR,
      col = ifelse(nmonth %in% monthNB[[mmonth]], cols[length(cols)], "grey55"),
      font = ifelse(nmonth %in% monthNB[[mmonth]], 2, 1),
      cex = 0.35,
      adj = c(0, 0.5)
    )
    
    
    ### BARPLOT
    s <- unique(dl[[group]][, .(Date, SMP_LABEL, SMP_EFFORT)])
    s$Date2 <- substr(s$Date, 6, 10)
    tab <- unlist(dlply(s, .(Date2), function(x) {
      sum(x$SMP_EFFORT)
    }))
    #tab<-table(substr(s$Date,6,10))
    sc <-
      substr(seq.Date(as.Date(paste0(
        "2008-", min(substr(s$Date, 6, 10))
      )), as.Date(paste0(
        "2008-", max(substr(s$Date, 6, 10))
      )), by = 1), 6, 10)
    miss <- setdiff(sc, names(tab))
    add <- rep(0, length(miss))
    names(add) <- miss
    tab <- c(tab, add)
    tab <- tab[order(names(tab))]
    mm <-
      substr(seq.Date(as.Date("2007-12-01"), as.Date("2008-11-30"), by = 1), 6, 10)
    tab <- tab[order(match(names(tab), mm))]
    tab <-
      tab[1:(length(tab) - min(which(!rev(tab) == 0)) + 1)] # tricks pour le 1 janvier ? v?rifier si p?riodes c
    
    
    s <- dl[[group]]
    s$Date2 <- substr(s$Date, 6, 10)
    s <-
      ddply(s, .(Date2), function(k) {
        sum(as.numeric(k$Count), na.rm = TRUE)
      })
    #tab2<-s$V1
    #names(tab2)<-s$Date2
    
    temp <- data.frame(Date2 = names(tab),
                       eff = tab,
                       stringsAsFactors = FALSE)
    temp <- join(temp, s, type = "full")
    
    
    names(tab)[setdiff(seq_along(tab), seq(1, length(tab), by = 10))] <-
      ""
    subplot({
      barplot(
        tab,
        las = 2,
        cex.names = 0.3,
        cex.lab = 0.3,
        cex.axis = 0.3,
        yaxt = "n",
        border = NA,
        ylab = "",
        col = db
      )
      
      axis(
        2,
        cex.axis = 0.3,
        cex.lab = 0.3,
        tcl = -0.1,
        lwd = 0.1,
        las = 2,
        col.axis = db
      )
      
      par(new = TRUE)
      
      barplot(
        temp$V1 / temp$eff,
        las = 2,
        cex.names = 0.3,
        cex.lab = 0.3,
        yaxt = "n",
        cex.axis = 0.3,
        border = NA,
        col = cols[length(cols)]
      )
      
      axis(
        4,
        cex.axis = 0.3,
        cex.lab = 0.3,
        tcl = -0.1,
        lwd = 0.1,
        las = 2,
        col.axis = cols[length(cols)]
      )
      
      mtext("Effort (km)",
            side = 2,
            cex = 0.3,
            line = 0.6)
      
      mtext(
        "Birds / km\nOiseaux / km",
        side = 4,
        cex = 0.3,
        line = 0.5
      )
      
      mtext(
        "Daily Effort (km) and Raw Linear Bird Densities\nEffort journalier (km) et densités linéaires brutes d'oiseaux",
        side = 1,
        cex = 0.35,
        line = 0.7
      )
    }
    , x = c(1600000, 3350000), y = c(1860000, 2160000)) #c(-1125000, -750000)
    
    
    ### LATITUDES numbers
    text(
      xxlat,
      yylat[-1],
      paste0(selat[-1], "°N"),
      xpd = TRUE,
      cex = tex * 0.5,
      adj = c(-0.15, -1)
    )
    text(
      xxlon[-1],
      yylon,
      gsub("-", "", paste0(selon[-1], "°W")),
      xpd = TRUE,
      cex = tex * 0.5,
      adj = c(-0.15, -1.25)
    )
    
    off <- 50000
    lines(sca[1, ], rep(sca[2, 1], 2) - off, lwd = 1)
    lines(rep(sca[1, 1], 2), sca[2, 1] - off + c(-10000, 10000), lwd = 1)
    lines(rep(sca[1, 2], 2), sca[2, 1] - off + c(-10000, 10000), lwd = 1)
    text(((sca[1, 2] - sca[1, 1]) / 2) + sca[1, 1],
         sca[2, 1] - off - 40000,
         paste((sca[1, 2] - sca[1, 1]) / 1000, "km"),
         adj = c(0.5, 0.5),
         cex = tex * 0.5
    )
    
    rect(
      -60001100000,
      -70000000000,
      6000000000,
      -1290000,
      col = alpha("white", 0.4),
      border = NA
    )
    #rect(-60001100000,2960000,6000000000,3000000000,col=alpha("white",0.4),border=NA)
    text(
      par("usr")[1],
      -1320000,
      "Predicted densities are derived from distance sampling models using Distance 6.0 and the GeoAviR R package with the Eastern Canadian Seabirds-at-Sea database. The sample size corresponds to the number of cruiseID/date/cell combinations.",
      cex = tex * 0.4,
      adj = c(-0.01, 0.5)
    )
    text(
      par("usr")[1],
      -1370000,
      "Ces densités proviennent de modèles d'échantillonnage par distance basés sur Distance 6.0 et le package R GeoAviR et utilisant les données du Suivi des oiseaux en mer de l'est du Canada. La taille d'échantillon correspond au nombre de combinaisons croisière/date/cellule.",
      cex = tex * 0.4,
      adj = c(-0.01, 0.5)
    )
    
    
    # PLOT EC LOGOS
    logo1 <-
      readJPEG(paste0(pathMAPSRData, "/ECCC_FIP_FRA_COUL.jpg"), native = TRUE)
    logo2 <- readPNG(paste0(pathMAPSRData, "/Huard.png"), native = TRUE)
    
    rect(
      boxcut[1],
      boxcut[4] - 90000,
      boxcut[1] + 1000000 + 175000,
      boxcut[4],
      col = "white",
      border = NA
    )
    rasterImage(logo1, boxcut[1], boxcut[4] - 80000, boxcut[1] + 1000000, boxcut[4])
    rasterImage(logo2,
                boxcut[1] + 1000000 + 50000,
                boxcut[4] - 80000,
                boxcut[1] + 1000000 + 150000,
                boxcut[4])
    
    dev.off()
    
    ### understand CI
    temp <- ml[[1]]$input_data$observations
    names(temp)[1] <- "Region"
    temp <- join(temp, dens)
    temp <- ddply(temp, .(Region), function(i) {
      i$nbsample <- length(unique(i$SMP_LABEL))
      i
    })
    
  }
  
}

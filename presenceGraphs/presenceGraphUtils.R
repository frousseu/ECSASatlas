library(GeoAviR)



fitModel <- function(data, formula) {
  data <- data[!is.na(y)]
  xrange <- range(data$x)
  model <- lm(formula, data = data)
  xseq <- seq(from = xrange[1],
              to = xrange[2],
              length = 80)
  pred <- predict(model, newdata = data.frame(x = xseq),  se = TRUE)
  y = pred$fit
  ci <- pred$se.fit * qt(0.95 / 2 + .5, pred$df)
  ymin = y - ci
  ymax = y + ci
  data.table(x = xseq, y, ymin, ymax, se = pred$se.fit)
}

presenceGraph <-
  function(data,
           x,
           y,
           group,
           colour = group,
           smoothData = TRUE,
           displayPoints = FALSE,
           displayLines = FALSE,
           xpadding = 1,
           method = "auto",
           xbounds = NULL,
           ybounds = c(0, NA),
           xlab = "",
           ylab = "",
           title = "",
           mult = 1,
           xticks = waiver(),
           xbreaks = NULL,
           se = FALSE,
           legend = TRUE,
           addSeasons = TRUE,
           expandy = c(0, 0),
           groupingFUN = NULL,
           ...) {
    # data <- data.table(x = d[[x]], y = d[[y]], group = d[[group]])
    
    setnames(data, c(x, y, group), c("x", "y", "group"))
    
    
    
    if (is.null(xbounds)) {
      xbounds = c(min(data$x), max(data$x) * xpadding)
    }
    if (is.null(xbreaks)) {
      xbreaks <- seq(xbounds[1], xbounds[2])
    }
    
    
    gplot <-  ggplot() +
      scale_x_continuous(
        limits = xbounds,
        breaks = xbreaks,
        labels = xticks,
        expand = c(0, 0)
      ) +
      labs(
        title = title,
        y = ylab,
        x = xlab,
        colour = NULL
      ) +
      theme(axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 9,
        colour = "black"
      ))  +
      theme(
        axis.text.y = element_text(size = 9, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        plot.title = element_text(size = 12, colour = "black"),
        strip.text = element_text(size = 8)
      ) 
    if (!legend) {
      gplot <- gplot + guides(colour = FALSE)
    } else {
      if (group == "Alpha") {
        species <- sort(unique(data$group))
        frenchNames <-
          SPECIES_NAMES$French_Name[match(species, SPECIES_NAMES$Species_ID)]
        englishNames <-
          SPECIES_NAMES$English_Name[match(species, SPECIES_NAMES$Species_ID)]
        labels <- sprintf("%s / %s", englishNames, frenchNames)
        gplot <- gplot + scale_color_discrete(labels = labels)
      }
    }
    
    ## Add Data
    
    # Add seasons delimiters
    if (addSeasons) {
      spring <-
        as.numeric(format(strptime("04-01", format = "%m-%d"), format = "%W"))
      autumn <-
        as.numeric(format(strptime("08-01", format = "%m-%d"), format = "%W"))
      winter <-
        as.numeric(format(strptime("12-01", format = "%m-%d"), format = "%W"))
      seasons <- c(spring, autumn, winter)
      for (i in seasons) {
        gplot <-
          gplot + geom_vline(
            xintercept = i,
            size = 0.5,
            linetype = "dashed",
            color = "darkgrey"
          )
      }
    }
    
    # Smooth data
    if (smoothData) {
      args <- list(...)
      # Get smoothing formula
      formula <- if (is.null(args$formula)) {
        y ~ x
      } else {
        args$formula
      }
      
      fit <- data[, fitModel(.SD, formula), by = group]
      # Add multiplier for display purpose
      fit$y <- fit$y * mult
      
      # Add grouping column
      fit <- merge(fit, SPECIES_NAMES, by.x = "group", by.y = group)
      if (is.function(groupingFUN)) {
        fit <- groupingFUN(fit)
      }

      # plot the smoothing
      gplot <-
        gplot + geom_smooth(aes(x, y, colour = get(colour)), data = fit, stat = "identity") +
        scale_y_continuous(limits = c(0, NA),
                           oob = squish,
                           expand = expandy) +
        scale_color_manual(values = COLOR_GROUPS[as.character(unique(fit$group_atlas))])
    }
    
    # Display points
    if (displayPoints) {
      gplot <- gplot + geom_point(data = data,
                                  aes(x, y, colour = get(colour)))
    }
    
    # Display lines
    if (displayLines) {
      gplot <- gplot + geom_line(data = data,
                                 aes(x, y, colour = colour))
    }
    
    gplot
  }





#' Title
#'
#' @param data the raw dataset
#' @param subset List of filters to apply to the dataset. The names of the items
#' must match the names of columns in data
#' @param groupBy Character vector of columns names with which the data will be grouped
#' @param speciesColumn the name of the column with species. Defaults to Alpha
#' @param countColumn name of the column with observation counts. Defaults to Count
#' @param effortColum name of the column with sample effort. Defaults to WatchLenKm
#' @param ... Optional parameters to either prepareData or presenceGraph. Common
#' options are:
#' smoothData : Boolean: should the data be smoothed. Defaults to TRUE
#' smoothMethod : Smoothing method for stat_smooth. Defaults to auto
#' displayPoints : Boolean: should the data points be displayed. Defaults to FALSE
#' displayLines : Boolean: should the data lines be displayed. Defaults to FALSE
#' xbounds, ybounds : numeric vector of length 2 of display limits respectively on x and
#'                    y axis
#' ... : Optional arguments to ggplot, stat_smooth, etc.
#'
#' @return the presence graph
#' @export
#'
#' @examples
displayPresence <-
  function(data,
           subset = NULL,
           groupBy = NULL,
           speciesColumn = "Alpha",
           prepareData = TRUE,
           ...) {
    if (prepareData) {
      presenceData <-
        prepareData(data,
                    subset = subset,
                    groupBy = groupBy,
                    speciesColumn,
                    ...)
    } else {
      presenceData <- data
    }
    g <- presenceGraph(presenceData,
                       x = "idx",
                       y = "density",
                       group = speciesColumn,
                       ...)
    g
  }


## Add number of observations to label
makeLabels <- function(n, speciesColumn) {
  setkeyv(n, speciesColumn)
  n <- merge(n, SPECIES_NAMES, by = speciesColumn)
  setnames(n, speciesColumn, "group")
  f <- function(labels) {
    labels[, 1] <- gsub("\\d+_", "", labels[, 1])
    if (names(labels)[1] != "group") {
      names(labels)[1] <- "group"
    }
    r <- join(labels, n, by = "group")
    newLabs <-
      sprintf("%s / %s (n = %d)", r$English, r$French, r$obs)
    data.frame(newLabs, stringsAsFactors = FALSE)
  }
  class(f) <- append(class(f), "labeller")
  f
}


# getGroups <- function(presenceData, groupsColumn = "group_atlas", excludeGroups) {
#   # Get groups
#   groups <- unique(presenceData$group_atlas)
#   groups <- groups[!groups %in% excludeGroups]
#
# }

pasteGroups <- function(groups, lang) {
  if (lang == "en") {
    sepm <- ", "
    sepend <- " and "
  } else {
    sepm <- ", des "
    sepend <- " et des "
  }
  res <- ""
  for (i in seq_along(groups)) {
    if (i == 1) {
      sep <- ""
    } else if (i == length(groups)) {
      sep <- sepend
    } else {
      sep <- sepm
    }
    res <- paste0(res, sep, groups[i])
  }
  res
}

makeTitle <- function(groups, groupTitle, lang) {
  group <- pasteGroups(groups, lang)
  title <- gsub("%GROUP%", group, groupTitle[lang])
  title <- capitalize(title)
  title <- paste(strwrap(title, width = 70), collapse = "\n")
}

addGroupingColumn <- function(groupOrder, speciesColumn) {
  function(data) {
    data[, group_atlas := factor(group_atlas, levels = groupOrder)]
    setkeyv(data, c("group_atlas", "group"))
    data[, grouping := paste0(as.numeric(group_atlas), "_", group)]
  }
}

# Create the graph for the group
groupGraph <-
  function(group,
           groupData,
           speciesColumn,
           groupColumn,
           groupTitle = "",
           separateSpecies = FALSE,
           ...) {
    # get data and species list
    data <- groupData$data
    speciesList <- groupData$species
    
    # retrive species names
    # data <- merge(data, SPECIES_NAMES, by = speciesColumn)
    
    # retrive the varaible for coloring
    colour <- speciesColumn
    # If our grouping column is display
    if (groupColumn == "display_group") {
      # grouping column is group_atlas
      colour <- "group_atlas"
      # english names are found in global variable
      groups <- DISPLAY_GROUP[[group]]
      
    } else {
      # otherwise the group name is the group variable
      groups <- group
    }
    # Get english names from global variable
    groupsen <-
      SPECIES_NAMES[match(groups, group_atlas), display_en]
    # Get french names from global variable
    groupsfr <-
      SPECIES_NAMES[match(groups, group_atlas), display_fr]
    
    # Create titles for both languages
    titleen <- makeTitle(groupsen, groupTitle, "en")
    titlefr <- makeTitle(groupsfr, groupTitle, "fr")
    title <- paste0(titleen, "\n", titlefr)
    
    
    # Create graph
    g <-
      presenceGraph(
        data,
        x = "idx",
        y = "density",
        group = speciesColumn,
        title = title,
        groupingFUN = addGroupingColumn(groups, speciesColumn),
        ...
      )
    
    if (!is.null(speciesList)) {
      # Calculate how many columns to use
      ncol <- 1
      if (length(speciesList) > 5) {
        ncol <- 2
      }
      nSpecies <- groupData$speciesCount
      # Separate graphs by species
      if (separateSpecies) {
        g <-
          g + facet_wrap(
            ~  grouping,
            scales = "free_y",
            dir = "v",
            ncol = ncol,
            labeller = makeLabels(nSpecies, speciesColumn)
          )
      }
    }
    g
  }

# Create presence graph for a given group
createGroupGraph <-
  function(group,
           presenceData,
           speciesColumn,
           groupColumn,
           effort,
           nmin,
           bySpecies,
           ...) {
    # First compute statistics for all groups
    groupData <-
      getGroupData(group,
                   presenceData,
                   speciesColumn,
                   groupColumn,
                   effort,
                   nmin,
                   bySpecies)
    # Then create graphs based on new data
    groupGraph(group, groupData, speciesColumn, groupColumn, ...)
  }



## Main function to create graphs by group
graphsByGroup <- function(presenceData,
                          speciesColumn = "Alpha",
                          groupColumn = "group_atlas",
                          nmin = 10,
                          effortBy = "Week",
                          bySpecies = TRUE,
                          ...) {
  # Clean the raw data and prepare it for analysis
  cleaned <- cleanData(presenceData, effortBy)
  
  # Retrieve cleaned data and total effort
  presenceData <- cleaned$data
  effort <- cleaned$effort
  
  # Retrieve groups
  groups <- unique(presenceData[[groupColumn]])
  
  # Iterate on all groups
  graphs <-
    lapply(
      groups,
      createGroupGraph,
      presenceData,
      speciesColumn,
      groupColumn,
      effort,
      nmin,
      bySpecies,
      ...
    )
  names(graphs) <- groups
  graphs
}


## Create pdf and pngs based on graphs
outputGraphs <- function(graphs, type, path) {
  destDir <- file.path(path, type)
  if (!dir.exists(destDir)) {
    dir.create(destDir)
  }
  
  filename <- paste0("presenceCalendar_", type)
  pdf(file.path(destDir, paste0(filename, ".pdf")),
      width = 10,
      height = 7)
  for (i in graphs) {
    print(i)
  }
  dev.off()
  
  for (group in names(graphs)) {
    png(
      file.path(destDir, paste0(group, "_", filename, ".png")),
      width = 7,
      height = 7,
      units = "in",
      res = 600
    )
    print(graphs[[group]])
    dev.off()
  }
}

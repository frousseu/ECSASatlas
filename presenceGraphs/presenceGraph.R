require(ggplot2)
library(splines)
library(data.table)
library(scales)
library(Hmisc)
library(RColorBrewer)

rm(list = ls())

setwd("C:/dev/atlas/presence")
ROOT_DIR <- "C:/dev/atlas/presence"
OUTPUT_DIR <- file.path(ROOT_DIR, "output")

source("presenceDataUtils.R", encoding = "UTF-8")
source("presenceGraphUtils.R", encoding = "UTF-8")

presenceData <- getPresenceGroups()

EXCLUDE_GROUPS <- c("", NA, "Diving Waterfowl", "Dabbling Waterfowl")

DISPLAY_GROUP <- list(c("Gulls", "Terns", "Gannets"), 
                      c("Alcids", "Phalaropes", "Storm-Petrels"),
                      c("Jaegers", "Skuas", "Shearwaters", "Fulmars"))
colgroups <- unlist(DISPLAY_GROUP)
COLOR_GROUPS <-  c("red3", "royalblue", "orange",
                   "deepskyblue3", "violetred", "goldenrod", 
                   "darkolivegreen", "tomato", "magenta3", "steelblue")
names(COLOR_GROUPS) <-  colgroups

    
graphs_species <- graphsByGroup(
  presenceData,
  size = 1,
  formula = y ~ ns(x, 4),
  se = FALSE,
  nmin = 20,
  expandy = c(0.05, 0),
  separateSpecies = TRUE,
  legend = FALSE,
  mult = 1000,
  speciesColumn = "Alpha",
  groupColumn = "display_group",
  colour = "group_atlas",
  groupTitle = c(en = "%GROUP% presence calendar in eastern Canada",
                 fr = "Calendrier de présence des %GROUP% dans l'est du Canada"),
  xlab = "Week / Semaine",
  ylab = "Birds/linear km travelled / Oiseaux/km linéaire parcouru x1000",
  xbreaks = axisTicks(c(0, 52), log = FALSE, nint = 12)
)


pdf("presenceGraphs2_species.pdf", width = 7, height = 9.5)
for (i in graphs_species) {
  print(i)
}
dev.off()


outputGraphs(graphs_species, "total", OUTPUT_DIR)




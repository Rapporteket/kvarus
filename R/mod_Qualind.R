

# UI-part


# Server-part


library(ggplot2)
library(dplyr)

### Load in data ###
punktData <- getTimepointData()

basisData <- getBasisData()

### Clean and tidy data ###
punktData <- prePros(punktData)

# Count quality indicator:
kval <- kval_count(punktData, "behandlingsstatus")

# Make labs for ggplot:
ggData <- makeGGdata("behandlingsstatus", "kval")

# Make plot:
kval_plot <- kval_plot(kval, ggData)


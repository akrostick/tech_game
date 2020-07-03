# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tech: Game Board
# Author: Aiken Ord 
# Date: 30 June 2020
# License: MIT
#
# Game board design for card game for tech project.  
#
# Drawn with axidraw plotter.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # 1. Global variables ----
# A3 <- c(420, 297) # page dimensions in mm
# bg = "#000000" # black
# fg <- "#ffffff" # white
# margin <- 15 # margin in mm
# card_length <- 40 # length of one card in mm

# 2. Plot function ----
createPlot <- function(A3 = c(420, 297), margin = 15, card_length = 40, bg = "#000000", fg = "#ffffff"){

  # 2.1 Setup ----
  
  # The limits (range) of the axes in the x-y plane
  xlims <- c(-A3[1]/2, A3[1]/2) 
  ylims <- c(-A3[2]/2, A3[2]/2)
  
  # The graphical parameters...
  # bg: background colour
  # mar: all margins (bottom, left, top right) are set to 0... i.e. no margins
  # bty: the type of box to draw around the boundaries of the graph... set to "n" -- no box
  par(bg = bg, mar=c(0,0,0,0), bty="n")

  # Create the plot area
  # x: 0 -- no data
  # type: "n" -- no plot
  # xlim: x-axis limits (range)
  # ylim: y-axis limits (range)
  # axes: F -- no axes
  # asp: x-y aspect ratio = 1:1
  plot(0, type = "n", xlim = xlims, ylim = ylims, axes = F)#, asp=1)

  
  # 2.2. Draw the boundary box ----
    
  # Left line
  x <- xlims[1] + margin
  y1 <- ylims[1] + margin
  y2 <- ylims[2] - margin
  
  lines(c(x, x), c(y1, y2), type = 'l', lty = 1, col = fg)
  
  # Right line
  x <- xlims[2] - margin
  y1 <- ylims[1] + margin
  y2 <- ylims[2] - margin
  
  lines(c(x, x), c(y1, y2), type = 'l', lty = 1, col = fg)
  
  # Bottom line
  x1 <- xlims[1] + margin
  x2 <- xlims[2] - margin
  y <-  ylims[1] + margin
  
  lines(c(x1, x2), c(y, y), type = 'l', lty = 1, col = fg)
  
  # Top line
  x1 <- xlims[1] + margin
  x2 <- xlims[2] - margin
  y <-  ylims[2] - margin
  
  lines(c(x1, x2), c(y, y), type = 'l', lty = 1, col = fg)
  
  # 2.3. Draw the start and end card positions -----
  
  # Vertical centre line
  x <- 0
  y1 <- ylims[1] + margin
  y2 <- ylims[2] - margin
  
  lines(c(x, x), c(y1, y2), type = 'l', lty = 1, col = fg)
  
  # Start card A
  x <- xlims[1] + card_length + margin
  y1 <- ylims[1] + margin
  y2 <- ylims[2] - margin
  
  lines(c(x, x), c(y1, y2), type = 'l', lty = 1, col = fg)
  
  # Start card B
  x <- xlims[2] - card_length - margin
  y1 <- ylims[1] + margin
  y2 <- ylims[2] - margin
  
  lines(c(x, x), c(y1, y2), type = 'l', lty = 1, col = fg)
  
  # End card A
  x <- card_length
  y1 <- ylims[1] + margin
  y2 <- ylims[2] - margin
  
  lines(c(x, x), c(y1, y2), type = 'l', lty = 1, col = fg)
  
  # End card B
  x <- -card_length
  y1 <- ylims[1] + margin
  y2 <- ylims[2] - margin  
  
  lines(c(x, x), c(y1, y2), type = 'l', lty = 1, col = fg)

  # 2.4. Loop over moving card positions A and B

  # Card spacing for card A
  carddif <- (xlims[2] - margin - 2*card_length)/6 # Spacing for each step

  # Card positions for A  
  for (i in 1:5) {
    x <- card_length + i*carddif
    y1 <- ylims[1] + margin
    y2 <- ylims[2] - margin  
    
    lines(c(x, x), c(y1, y2), type = 'l', lty = 1, col = fg)
  }
  
  # Card spacing for card B
  for (i in 1:5) {
    x <- -card_length - i*carddif
    y1 <- ylims[1] + margin
    y2 <- ylims[2] - margin  
    
    lines(c(x, x), c(y1, y2), type = 'l', lty = 1, col = fg)
  }
  
}


# 3. Card track functions ----

# 3.1. Top track ----
track1 <- function(A3 = c(420, 297), margin = 15, card_length = 40, bg = "#000000", fg = "#1bd1d1") {
  # The limits (range) of the axes in the x-y plane
  xlims <- c(-A3[1]/2, A3[1]/2) 
  ylims <- c(-A3[2]/2, A3[2]/2)
  
  # The graphical parameters...
  # bg: background colour
  # mar: all margins (bottom, left, top right) are set to 0... i.e. no margins
  # bty: the type of box to draw around the boundaries of the graph... set to "n" -- no box
  par(bg = bg, mar=c(0,0,0,0), bty="n")
  
  # Create the plot area
  # x: 0 -- no data
  # type: "n" -- no plot
  # xlim: x-axis limits (range)
  # ylim: y-axis limits (range)
  # axes: F -- no axes
  # asp: x-y aspect ratio = 1:1
  plot(0, type = "n", xlim = xlims, ylim = ylims, axes = F)#, asp=1)

  # Draw track  
  x1 <- xlims[1] + margin
  x2 <- xlims[2] - margin
  y <-  (ylims[2] - margin)/2
  
  lines(c(x1, x2), c(y, y), type = 'l', lty = 1, col = fg)
}


# 3.2. Middle track ----
track2 <- function(A3 = c(420, 297), margin = 15, card_length = 40, bg = "#000000", fg = "#00a47a") {
  # The limits (range) of the axes in the x-y plane
  xlims <- c(-A3[1]/2, A3[1]/2) 
  ylims <- c(-A3[2]/2, A3[2]/2)
  
  # The graphical parameters...
  # bg: background colour
  # mar: all margins (bottom, left, top right) are set to 0... i.e. no margins
  # bty: the type of box to draw around the boundaries of the graph... set to "n" -- no box
  par(bg = bg, mar=c(0,0,0,0), bty="n")
  
  # Create the plot area
  # x: 0 -- no data
  # type: "n" -- no plot
  # xlim: x-axis limits (range)
  # ylim: y-axis limits (range)
  # axes: F -- no axes
  # asp: x-y aspect ratio = 1:1
  plot(0, type = "n", xlim = xlims, ylim = ylims, axes = F)#, asp=1)
  
  # Draw track  
  x1 <- xlims[1] + margin
  x2 <- xlims[2] - margin
  y <-  0
  
  lines(c(x1, x2), c(y, y), type = 'l', lty = 1, col = fg)
}

# 3.3. Bottom track ----
track3 <- function(A3 = c(420, 297), margin = 15, card_length = 40, bg = "#000000", fg = "#ffca00") {
  # The limits (range) of the axes in the x-y plane
  xlims <- c(-A3[1]/2, A3[1]/2) 
  ylims <- c(-A3[2]/2, A3[2]/2)
  
  # The graphical parameters...
  # bg: background colour
  # mar: all margins (bottom, left, top right) are set to 0... i.e. no margins
  # bty: the type of box to draw around the boundaries of the graph... set to "n" -- no box
  par(bg = bg, mar=c(0,0,0,0), bty="n")
  
  # Create the plot area
  # x: 0 -- no data
  # type: "n" -- no plot
  # xlim: x-axis limits (range)
  # ylim: y-axis limits (range)
  # axes: F -- no axes
  # asp: x-y aspect ratio = 1:1
  plot(0, type = "n", xlim = xlims, ylim = ylims, axes = F)#, asp=1)
  
  # Draw track  
  x1 <- xlims[1] + margin
  x2 <- xlims[2] - margin
  y <-  (ylims[1] + margin)/2
  
  lines(c(x1, x2), c(y, y), type = 'l', lty = 1, col = fg)
}

# # Test functions
# createPlot()
# track1()
# track2()
# track3()

# 4. Draw game board with plotter ----
library(fawkes)
fawkes::axi_version(options = axi_options())
fawkes::axi_toggle_pen(options = axi_options())

axi_options <- fawkes::axi_options(speed_down = 1,  speed_up = 2, model="A3", const_speed = T)
#axi_options <- fawkes::axi_options(speed_down = 10,  speed_up = 20, model="A3", const_speed = T)

# 4.1. Frame ---
plot.new()
ad <- axi_dev("A3", portrait = FALSE, margins = 0, ignore_color = TRUE, options = axi_options)
createPlot()
invisible(dev.off())
ad 

# 4.2. Track 1---
ad <- axi_dev("A3", portrait = FALSE, margins = 0, ignore_color = TRUE, options = axi_options)
track1()
invisible(dev.off())
ad 

# 4.3. Track 2 ---
ad <- axi_dev("A3", portrait = FALSE, margins = 0, ignore_color = TRUE, options = axi_options)
track2()
invisible(dev.off())
ad 

# 4.4. Track 3 ---
ad <- axi_dev("A3", portrait = FALSE, margins = 0, ignore_color = TRUE, options = axi_options)
track3()
invisible(dev.off())
ad 

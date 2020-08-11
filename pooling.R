###########################################################################
# Microeconomics Essay on Screening - Pooling Equilibrium Graphs
###########################################################################


# Install libraries 
if (!require('tidyverse')) install.packages("tidyverse")
if (!require('ggrepel')) install.packages("ggrepel")

# Clear workspace 
rm(list=ls())


# Load libraries 
library(tidyverse)
library(ggrepel)
source('intersect_func.R')


# Pooling Equilibrium Graph 

# Define the certainty line 
certainty <- data.frame(Hmisc::bezier(x = c(0, 6, 12), y = c(0, 6, 12)))
label_certainty <- data.frame(x=10,y=10)

# Define the risky type line
risky <- data.frame(Hmisc::bezier(x = c(2, 4.5, 11.5),y = c(7, 3, 3.5)))
label_risky <- data.frame(x=11.5,y=3.5)

# Define the safe type line
safe <- data.frame(Hmisc::bezier(x = c(4, 4.5, 11.5), y = c(9, 2.3, 2.8)))
label_safe <- data.frame(x=11.5,y=2.8)

# Define the 45 degree semi circle 
circle45 <- data.frame(Hmisc::bezier(x = c(1,1.7, 2), y = c(1,0.7, 0)))

# Find the intersection
intersect_point <- data.frame(curve_intersect(risky, safe))

# Create some points for connection
intercept <- data.frame(x = 12, y = 0)
safe_point <- data.frame(x = 4.41, y = 8.25) 
risky_point <- data.frame(x = 1.025, y = 4.476)
both_point <- data.frame(x = 2.6625, y = 6.363)
circle_point <- data.frame(x = 1, y = 1)
# Create the zero profit linnes
risky_line <- rbind(intercept, risky_point)
safe_line <- rbind(intercept, safe_point)
both_line <- rbind(intercept, both_point)


# Graph of pooling equilibrium 
# Add point for profitable deviation
profit_point <- data.frame(x=8.5, y=3.3)

Plot2 <- ggplot(mapping = aes(x = x, y = y)) + 
  geom_path(data = certainty, color = 'Black', size = 1) + 
  geom_path(data = risky, color = 'Red', size = 1) + 
  geom_path(data = safe, color = 'Green', size = 1) +
  geom_point(data = intersect_point, color = 'Blue', size = 3) +
  geom_label(data = intersect_point, label = expression(paste(C[3])), nudge_y = 0.5, label.size = 0.5, size = 5) +
  geom_line(data = risky_line, linetype = 'dotted', color = 'Red', size = 1.3 ) +
  geom_line(data = safe_line, linetype = 'dotted', color = 'Green', size = 1.3 ) +
  geom_line(data = both_line, linetype = 'dotted', color = 'Blue', size = 1.3 ) +
  geom_point(data = risky_point, color = 'Red', size = 0.5) + 
  geom_text(data = risky_point, label = 'R', nudge_x = -0.5, nudge_y = 0.3) +
  geom_point(data = safe_point, color = 'Green', size = 0.5) + 
  geom_text(data = safe_point, label = 'S', nudge_y = 0.4) +
  geom_point(data = both_point, color = 'Blue', size = 0.5) + 
  geom_text(data = both_point, label = 'B', nudge_y = 0.3) +
  geom_path(data = circle45, size = 0.7) + 
  geom_text(data = circle_point, label = '45º', nudge_x = 1, size = 4) + 
  geom_text_repel(data = intercept, label = 'E', nudge_y = 1, size = 4) +
  geom_point(data = profit_point, color = 'Black', size = 2) +
  geom_label(data = profit_point, label = expression(paste(C[4])), nudge_y = 0.5, label.size = 0.5, size = 5) +
  geom_point(data = label_risky, size = 1, color = 'Red') +
  geom_text(data = label_risky, label = 'R', nudge_y = 0.3) +
  geom_point(data = label_safe, size = 1, color = 'Green') +
  geom_text(data = label_safe, label = 'S', nudge_y = 0.3) +
  geom_point(data = label_certainty, size = 0.5, color = 'Black') +
  geom_text_repel(data = label_certainty, label = 'Certainty', nudge_x = 0.5, nudge_y = -0.5) +
  theme_classic() + 
  ylab(expression(paste('Income if loss (W'[1],')'))) +
  xlab(expression(paste('Income if no loss (W'[0],')'))) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 6, 12)) + 
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 6, 12)) + 
  theme(axis.title=element_text(size=14),
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.line = element_line(arrow = arrow())) +
  coord_equal()
Plot2

  
###########################################################################
# Microeconomics Essay on Screening - Perfect Information Graphs 
###########################################################################

# Load libraries 
library(tidyverse)
library(ggrepel)
source('intersect_func.R')

# Full Information Graph 

# Define the certainty line 
certainty <- data.frame(Hmisc::bezier(x = c(0, 6, 12), y = c(0, 6, 12)))
label_certainty <- data.frame(x=10,y=10)

# Define the risky type line
risky <- data.frame(Hmisc::bezier(x = c(1, 3, 9),y = c(6, 2, 3)))
label_risky <- data.frame(x=9,y=3)

# Define the safe type line
safe <- data.frame(Hmisc::bezier(x = c(4, 5, 11), y = c(11, 5, 4)))
label_safe <- data.frame(x=11,y=4)

# Define the 45 degree semi circle 
circle45 <- data.frame(Hmisc::bezier(x = c(1,1.7, 2), y = c(1,0.7, 0)))

# Define the x intercept point at 12 (could just do this with data.frame)
intercept <- data.frame(x = 12, y = 0)

# Find the intersects between the lines 
intersection_safe_df <- data.frame(curve_intersect(certainty, safe))
intersection_risky_df <- data.frame(curve_intersect(certainty, risky))


both_df <- rbind(intersection_risky_df, intersection_safe_df)
both_df   # slope = -0.46 for risky and slope = -1.4 for safe
new_df = data.frame(x = 0, y = 0)
new_df[1, 1] = (both_df[1, 1] + both_df[2, 1])/2
new_df[1, 2] = (both_df[1, 2] + both_df[2, 2])/2
point1 <- data.frame(x = 4.41, y = 8.25) 
point2 <- data.frame(x = 1.025, y = 4.476)
point3 <- data.frame(x = 2.6625, y = 6.363)
# Find intersections with intecept 
intersection_intercept_risky <- curve_intersect(risky, intercept)
intersection_intercept_safe <- curve_intersect(safe, intercept)
intersection_intercept_safe

# Temporary names for various points  
# Eh, graph works so I won't change the variable names. Will make it more clear in the following two graphs though. 
y = intersection_safe_df
w = intersection_risky_df
z = rbind(intercept,y)
x = rbind(intercept, w)
int_mid = rbind(intercept, new_df)
a = rbind(w, point2)
b = rbind(y, point1)
c = rbind(int_mid, point3)
circle_1 = data.frame(x = 1, y =  1)
risky_point = data.frame(x=1, y=6)




###########################################################################
# Graphs 
###########################################################################

# Create the plot with average line 
Plot1 <- ggplot(mapping = aes(x = x, y = y)) + 
  geom_path(data = certainty, color = 'Black', size = 1) + 
  geom_path(data = risky, color = 'Red', size = 1) + 
  geom_path(data = safe, color = 'Green', size = 1) +
  geom_point(data = intersection_risky_df, size = 3, color = 'Red') +
  geom_label(data = intersection_risky_df, label = expression(paste(C[1])), nudge_y = 0.5, label.size = 0.5, size = 5) +
  geom_point(data = intersection_safe_df, size = 3, color = 'Green') +
  geom_label(data = intersection_safe_df, label = expression(paste(C[2])), nudge_y = 0.5, label.size = 0.5, size = 5) +
  geom_point(data = intercept, size = 3, color = 'Orange') +
  geom_point(data = point1, size = 1, color = 'Green') +
  geom_text(data = point1, label = 'S', nudge_x = -0.4, nudge_y = 0.4) +
  geom_line(data = b, linetype = 'dotted', color = 'Green', size = 1.3 ) +
  geom_point(data = point3, size = 1, color = 'Blue') +
  geom_text(data = point3, label = 'B', nudge_x = -0.5, nudge_y = 0.3) +
  geom_line(data = c, linetype = 'dotted', color = 'Blue', size = 1.3 ) +
  geom_point(data = point2, size = 1, color = 'Red') +
  geom_text(data = point2, label = 'R', nudge_x = -0.5, nudge_y = 0.3) +
  geom_line(data = a, linetype = 'dotted', color = 'Red', size = 1.3 ) +
  geom_point(data = intercept, color = 'Black', size = 3) +
  geom_line(data = z, linetype = 'dotted', color = 'Green', size = 1.3 ) +
  geom_line(data = x, linetype = 'dotted', color = 'Red', size = 1.3 ) +
  geom_line(data = int_mid, linetype = 'dotted', color = 'Blue', size = 1.3 ) +
  geom_path(data = circle45, size = 0.7) + 
  geom_text(data = circle_1, label = '45º', nudge_x = 1, size = 4) + 
  geom_text_repel(data = intercept, label = 'E', nudge_y = 1, size = 4) +
  geom_point(data = label_risky, size = 1, color = 'Red') +
  geom_text(data = label_risky, label = 'R', nudge_x = 0.5) +
  geom_point(data = label_safe, size = 1, color = 'Green') +
  geom_text(data = label_safe, label = 'S', nudge_x = 0.5) +
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
Plot1


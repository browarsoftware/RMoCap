# RMoCap
R language package for processing and analysis motion capture (mocap) data.

In order to install package in R, run following commands:

#############

#R code

#############


install.packages("devtools") # if you have not installed "devtools" package

devtools::install_github("browarsoftware/RMoCap")

#test package

library(RMoCap)

data("right.arm.motion.1")

plot(right.arm.motion.1, frame = 1, my.color = "white", alpha = 1, spheres = TRUE)

plot(right.arm.motion.1, frames.fraction = 0.5, my.color = "white", alpha = 1, spheres = FALSE)


#############

#end of R code

#############


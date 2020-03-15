start = Sys.time() # Just to see how well the code is running

library(graphics)
library(animation)

setwd("C://Users//gabri//Desktop//R Studio//Projetos//Buffon Neddle")

# Gathering the information in a matrix ---------------------------------------------------------------------------------------------------------------------------------------------------------------

# For default (and simplification), each needle will length = 1 while the bounds equidistant 2*length for each other

# How many needles we'll throw?
n = 1000

# First coord. and angle for the needle

x0 = runif(n, min = -20, max = 20)
y0 = runif(n, min = 0, max = 20)
ang = runif(n, min = 0, max = 360)

# Second coord.; are they intersecting any line?

x1 = rep(NA, n)
y1 = rep(NA, n)
inside = rep(NA, n)

color = sample(1:n)

x1 = x0 + cos(ang)
y1 = y0 + sin(ang)


matriz_coordenadas = matrix(data = c(x0, y0, x1, y1, ang, inside, color), 
                            ncol = 7, 
                            nrow = n) # Each line is the information for each needle

# We need to know if they intersect:

# The first coord. will belong to certain boundary x;
# If the second coord. belong to another one, it intersects.

index = seq(0,20,2)

matriz_intersect_1 = matrix(NA, nrow = n, ncol = 10)

for(i in c(1:10)) {
  
  matriz_intersect_1[,i] = (matriz_coordenadas[,2] <= index[i+1] & matriz_coordenadas[,2] >= index[i])
  
}

matriz_intersect_2 = matrix(NA, nrow = n, ncol = 10)

for(i in c(1:10)) {
  
  matriz_intersect_2[,i] = (matriz_coordenadas[,4] <= index[i+1] & matriz_coordenadas[,4] >= index[i])
  
}



same_boundary = matriz_intersect_1 == matriz_intersect_2
same_boundary = ifelse(same_boundary == TRUE, 0, 1) # if they are in the same boundary, = 0

boulean_boundary = rep(NA, n)

for(i in c(1:n)) {
  
  boulean_boundary[i] = sum(same_boundary[i,])
  
}

boulean_boundary = ifelse(boulean_boundary == 2, 1, 0) # if = 1, the needle intersect

# 0: don't intersect
# 1: intersect

matriz_coordenadas[,6] = boulean_boundary


plot_vector = rep(NA, n)

for(i in c(1:n)) {
  
  plot_vector[i] = i / table(boulean_boundary[1:i])[2]
  plot_vector = ifelse(is.na(plot_vector) == TRUE, 0, plot_vector)
  
}

# Plotting --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


animation::saveGIF( {for(i in c(1:n)) {

par(mfrow=c(1,2))

plot(x = NA, # Blank plot as a canvas
     y = NA,
     xlim = c(-21,21),
     ylim = c(-1, 21),
     xlab = NA,
     ylab = NA,
     main = "Buffon Needle Simulation",
     xaxt = "n",
     yaxt = "n")

par(new = TRUE)

for(j in seq(0,20,2)) {
  
  abline(h = j,
         lwd = 2)
  
} # Strip Boundaries

par(new = TRUE)

segments(x0[1:i],
         y0[1:i],
         x1[1:i],
         y1[1:i],
         col = color[1:i],
         lwd = 2) # Needles

# new plot

plot(x = 1:i,
     y = plot_vector[1:i],
     ylim = c(0,max(plot_vector)),
     xlab = paste("Needles:",i),
     ylab = "Estimate of pi",
     main = paste("Value",round(plot_vector[i], digits = 3)),
     type = "l",
     lwd = 2,
     col = "darkmagenta") # Converging

par(new = TRUE)

abline(h = pi,
       lty = 2)
}},
movie.name = paste("buffon_needle_",n,".gif"),
interval = 0.01, 
ani.width = 720, 
ani.height = 480
)


end = Sys.time()

end - start

beepr::beep(sound = 8) # Mario sound completing the level
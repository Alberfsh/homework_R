data(doubs,package = "ade4")
species = doubs$fish
spa  <- doubs$xy
plot(spa,asp = 1,type = "n" ,xlab = "x (km)",ylab = "y (km)")
lines(spa,col = "lightblue")
text(spa,row.names(spa),cex = 0.5, col = "red")
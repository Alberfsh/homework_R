data(doubs,package = "ade4")
library(tidyverse)
library(dplyr)
# Load the vegan package for Bray-Curtis dissimilarity index
library(vegan)
fish <- doubs$fish
fish <- fish[rowSums(fish) != 0,]
env <- doubs$env
env <- env[-8,]
row_counts <- apply(fish > 0, 1, sum)
col_counts <- apply(fish > 0, 2, sum)
row_counts
sort(row_counts) # site 26 has most species
sort(col_counts) # the Lece is most widely spread
# Load the vegan package for Bray-Curtis dissimilarity index
library(vegan)

# Calculate the Bray-Curtis dissimilarity matrix
dissimilarity <- vegdist(t(fish), method = "bray")

# Perform hierarchical clustering using Ward's minimum variance method
hc <- hclust(dissimilarity, method = "ward.D2")

# Plot the dendrogram
plot(hc)

# Cut the dendrogram into 3 clusters
clusters <- cutree(hc, k = 4)

# Print the cluster assignments
print(clusters)
NMDS <- metaMDS(fish,k=2,trymax=100,trace=F,autotransform=FALSE,distance = "bray")
ordiplot(NMDS,type="n")
orditorp(NMDS,display = "species",col = "red",air = 0.01)
orditorp(NMDS,display = "sites",cex = 1.1,air = 0.01)
ef <- envfit(NMDS,env,permu = 999)
ef
plot(NMDS,type = "t",display = "sites")
plot(ef,p.max = 0.05,cex=1.5)
orditorp(NMDS,display = "species",col = "red",air = 0.01) # dfs,alt,slo,flo  cause a community to vary across a landscape
decorana(fish)
pca <- rda(fish)
pca
sum(apply(fish,2,var))
rda <- rda(fish,env)
rda
fish.ca <- cca(fish)
summary(fish.ca)
plot(fish.ca,scaling=1)
plot(fish.ca,scaling=2)
cca(fish,env)

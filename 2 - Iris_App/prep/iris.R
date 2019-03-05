library(mixtools)

iris <- datasets::iris
set.seed(1234) # labels are the original ones with this seed (avoid permutation)
r.km <- kmeans(iris[,1:4], centers=3)
print(mean(r.km$cluster!=as.numeric(iris$Species))*100)
#set.seed(3333)  # labels are the original ones with this seed (avoid permutation)
c0 <- list(r.km$centers[1,], r.km$centers[2,], r.km$centers[3,])
r.em <- mvnormalmixEM(iris[,1:4], mu=c0, arbvar=FALSE)
post <- r.em$posterior
r.em$class <- sapply(seq(nrow(post)), function(i) { j <- which.max(as.vector(post[i,])) })
print(mean(r.em$class!=as.numeric(iris$Species))*100)

save(r.em, r.km, file="mmiris.RData")
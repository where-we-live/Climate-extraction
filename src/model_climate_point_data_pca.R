
# Load necessary libraries
library(ncdf4)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(networkD3)
library(dplyr)
library(lme4)
library(lmerTest)
library(tidyverse)
library(rockchalk)
library(dplyr)
library(gtsummary)
library(car)
library(ordinal)
library(finalfit)
library(dplyr)
library(mice)
library(ggplot2)
library(plm)
library(flextable)
library(gt)
library(kableExtra)
library(pglm)
library(broom.mixed)
library(huxtable)
library(cluster)
library(NbClust)
library(factoextra)
library(lcmm)
library(flexmix)
library(mclust)
library(poLCA)
library(lavaan)
library(msm)
library(stats19)
library(agricolae)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(vcd)
library(networkD3)
library(poLCA)
library(psych)
library(nnet)
library(randomForest)
library(xfun)
library(pdp)
library(dunn.test)
library(RColorBrewer)
library(scales)
library(ggpubr)
library(ggExtra)
library(gridExtra)
library(gtsummary)
library(flextable)
library(officer)
# Load library
library(stats)
library(psych)


set.seed(123)

#PCA


pca_result <- prcomp(Data1, center = TRUE, scale = TRUE)
summary(pca_result)
#screeplot(pca_result,type="l")
print(pca_result$rotation)
eigenvectors<-pca_result$loadings
eigenvalues<-pca_result$sdev*pca_result$sdev
eigenvalues

pca_result <- principal(Data1, nfactors = 2, rotate = "varimax")
pca_result

fit.varimax<-principal(Data1,nfactors=2,rotate="varimax")
fit.varimax
loadings <- fit.varimax$loadings
loadings1<-loadings(fit.varimax)
loadings1
component_scores <- as.matrix(Data1) %*% loadings
normalized_scores <- scale(component_scores)
composite_data <- data.frame(RC1 = normalized_scores[, 1], RC2 = normalized_scores[, 2])
View(composite_data)
standardized_data <- scale(composite_data)

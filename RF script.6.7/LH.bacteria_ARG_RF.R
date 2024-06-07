library(vegan)
library(ggplot2)
library(RColorBrewer)
data_pca <- read.delim("LH.bacteria_ARG_RF.txt",sep = "\t", header = T)

# Split the dataset
set.seed(123)  # For reproducibility
sample_indices <- sample(1:nrow(data_pca), nrow(data_pca) * 0.3)  # 30% test set
train_data <- data_pca[sample_indices, ]
test_data <- data_pca[-sample_indices, ]

# Random forest modeling using the first 13 principal components to predict LH.ARG
library(randomForest)
rf_model <- randomForest(LH.ARG ~ ., ntree = 500, data = data_pca, importance = TRUE, nrep = 1000)
rf_model

# Predictions
predictions <- predict(rf_model, newdata = test_data)

# Evaluate model performance
mse <- mean((predictions - test_data$LH.ARG)^2)
r_squared <- 1 - mse / var(test_data$LH.ARG)
print(paste("Mean Squared Error (MSE): ", mse))
print(paste("R-squared (R^2): ", r_squared))

# Factor significance
library(rfPermute)

# Use the rfPermut() function to re-perform random forest analysis on the above data
set.seed(123)
factor_rfP <- rfPermute(LH.ARG ~ ., data = data_pca, importance = TRUE, ntree = 500, nrep = 1000, num.cores = 6)
factor_rfP

# Extract importance scores of predictive variables
importance_factor.scale <- data.frame(importance(factor_rfP, scale = TRUE), check.names = FALSE)
importance_factor.scale

# Extract the significance of importance scores of predictive variables
importance_factor.scale.pval <- (factor_rfP$pval)[ , , 2]
importance_factor.scale.pval


# Sort predictive variables by importance scores, e.g., by "%IncMSE"
importance_factor.scale <- importance_factor.scale[order(importance_factor.scale$'%IncMSE', decreasing = TRUE), ]
importance_factor.scale

# Plotting the %IncMSE values of predictive variables
library(ggplot2)

importance_factor.scale$OTU_name <- rownames(importance_factor.scale)
importance_factor.scale$OTU_name <- factor(importance_factor.scale$OTU_name, levels = importance_factor.scale$OTU_name)

p <- ggplot() +
  geom_col(data = importance_factor.scale, aes(x = reorder(OTU_name, `%IncMSE`), y = `%IncMSE`, fill = `%IncMSE`),
           colour = 'black', width = 0.8) +
  coord_flip() +
  scale_fill_gradientn(colors = c(low = brewer.pal(5, "Blues")[2],
                                  mid = brewer.pal(5, "YlGnBu")[1],
                                  high = brewer.pal(6, "OrRd")[4]),
                       values = scales::rescale(c(0, 15, 50)),
                       guide = FALSE,
                       aesthetics = "fill") +
  labs(title = NULL, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
  theme_test() +
  theme(panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = 'black')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = c(0, 0), limit = c(0, 78))

p

# Mark the significance information of predictive variables
# Default p < 0.05 as *, p < 0.01 as **, p < 0.001 as ***
for (OTU in rownames(importance_factor.scale)) {
  importance_factor.scale[OTU, '%IncMSE.pval'] <- importance_factor.scale.pval[OTU, '%IncMSE']
  if (importance_factor.scale[OTU, '%IncMSE.pval'] >= 0.05) importance_factor.scale[OTU, '%IncMSE.sig'] <- ''
  else if (importance_factor.scale[OTU, '%IncMSE.pval'] >= 0.01 & importance_factor.scale[OTU, '%IncMSE.pval'] < 0.05) importance_factor.scale[OTU, '%IncMSE.sig'] <- '*'
  else if (importance_factor.scale[OTU, '%IncMSE.pval'] >= 0.001 & importance_factor.scale[OTU, '%IncMSE.pval'] < 0.01) importance_factor.scale[OTU, '%IncMSE.sig'] <- '**'
  else if (importance_factor.scale[OTU, '%IncMSE.pval'] < 0.001) importance_factor.scale[OTU, '%IncMSE.sig'] <- '***'
}

p <- p +
  annotate("text", x = importance_factor.scale$OTU_name, y = importance_factor.scale$`%IncMSE`, 
           label = sprintf("%.2f%% %s", importance_factor.scale$`%IncMSE`, importance_factor.scale$`%IncMSE.sig`), 
           hjust=-0.2,size = 3)
p
# A3 package for evaluating model p-value
library(A3)

# model.fn = randomForest calls the randomForest method for computation
# p.acc = 0.001 indicates the estimation of p-value based on 1000 random permutations. The smaller the p.acc value, the more permutations, the slower the computation. 
# model.args is used to pass parameters to randomForest(), so the parameters inside are based on the parameters of randomForest(). For details, see ?randomForest
set.seed(123)
factor_forest.pval <- a3(LH.ARG ~ ., data = data_pca, model.fn = randomForest, p.acc = 0.001, model.args = list(importance = TRUE, ntree = 500))
factor_forest.pval
model.out <- as.data.frame(factor_forest.pval$table)
p.value <- as.numeric(gsub("<", "", model.out[1, "p value"]))
# Add known explanation rate of the model to the top right corner
p <- p +
  annotate('text', label = 'Bacterial ARGs', x = 3, y = 40, size = 3) +
  annotate('text', label = sprintf('italic(R^2) == %.2f', mean(factor_rfP[["rf"]][["rsq"]])), x = 2.5, y = 40, size = 3, parse = TRUE) +
  annotate('text', label = sprintf('italic(P) < %.3f', p.value), x = 2, y = 40, size = 3, parse = TRUE)

p

ggsave(p, file = "LH.ARG_rf.pdf", width = 5, height = 7)

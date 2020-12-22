#!/usr/bin/env Rscript
#
# Title: ""HarvardX: PH125.9x Capstone - Choose your own"
# Author: "Friederike David"
#
# This is the choose-your-own project of Friederike David for the HarvardX's
# Data Science Capstone course. In this project, smoking status is sought to be
# predicted from methylation data downloaded from GEO.


#### Prerequisites ####

# employed packages from cran
cran <- c(
  "tidyverse",    # load multiple 'tidyverse' packages in a single step
  "here",         # locate files within project
  "data.table",   # fast reading and writing of tabular data
  "BiocManager",  # install Bioconductor packages
  "caret",        # classification and regression training
  "glmnet",       # method = "glmnet"
  "kernlab",      # method = "svmLinear"    
  "randomForest", # method = "rf"
  "MLeval",       # model evaluation
  "patchwork"     # plotting
)
lapply(cran, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
    library(pkg, character.only = TRUE)
  }
})

# employed packages from Bioconductor
bioc <- c("GEOquery",   # download data from NCBI GEO
          "minfi")      # differential methylation for feature selection
lapply(bioc, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    BiocManager::install(pkg)
    library(pkg, character.only = TRUE)
  }
})

# project setup: use "<projectdir>/data" for input data
dir.create(here("data"), showWarnings = F)

# plotting theme
theme_set(theme_classic())

# R version (to use correct set.seed call)
rver <- paste0(sessionInfo()$R.version$major, sessionInfo()$R.version$minor) %>% 
  str_remove_all("\\.") %>% as.integer()


#### Data input ####

# load data from GEO
geo_id <- "GSE53045"
geo <- getGEO(geo_id, destdir = here("data"))


#### Data preparation ####

# extract methylation data
methyl <- geo[[1]]@assayData$exprs %>% t()
glimpse(methyl)

# extract relevant phenotypic data 
pheno <- pData(geo[[1]]) %>% 
  select(geo_accession, disease_state = `disease state:ch1`) %>% 
  mutate(disease_state = factor(disease_state, levels = c("Smoker", "Control")))
str(pheno)

# remove probes with missing values
probes_remove <- apply(methyl, 2, function(x) any(is.na(x)))
methyl <- methyl[, !probes_remove]


# split sample into distinct sets for model development and testing
if (rver < 360) {set.seed(42)} else {set.seed(42, sample.kind = "Rounding")}
test_index <- createDataPartition(y = pheno$disease_state, p = .2, list = F)

methyl_train <- methyl[-test_index, ]
methyl_test <- methyl[test_index, ]

pheno_train <- pheno[-test_index, ]
pheno_test <- pheno[test_index, ]


#### Data exploration ####

# dimensionality reduction via principal component analysis 
pca <- prcomp(methyl_train)

pca.df <- pca$x %>% as_tibble() %>%
  mutate(Group = pheno_train$disease_state)

# pca plots for first three principal components
p1 <- ggplot(pca.df, aes(PC1, PC2, col = Group)) + geom_point()
p2 <- ggplot(pca.df, aes(PC1, PC3, col = Group)) + geom_point()
p3 <- ggplot(pca.df, aes(PC3, PC2, col = Group)) + geom_point()

p2 + grid::textGrob('') + p1 + p3 + 
  plot_layout(nrow = 2, guides = "collect") &
  theme(legend.position = "bottom")


#### Feature selection ####

# split training set into distinct sets for feature selection and model fitting
if (rver < 360) {set.seed(42)} else {set.seed(42, sample.kind = "Rounding")}
feat_index <- createDataPartition(y = pheno_train$disease_state,
                                  p = .4, list = FALSE)

methyl_feat <- methyl_train[feat_index, ]
methyl_dev <- methyl_train[-feat_index, ]

pheno_feat <- pheno_train[feat_index, ]
pheno_dev <- pheno_train[-feat_index, ]


# perform differential methylation testing on the feature selection subset
methyl_diff <- dmpFinder(dat = t(methyl_feat), pheno = pheno_feat$disease_state,
                         qCutoff = 0.05, type = "categorical")


# selected features: 100 probes with most significant differential methylation
feat_sel <- slice_min(methyl_diff, order_by = qval, n = 100) %>% rownames()
feat_sel_index <- colnames(methyl_feat) %in% feat_sel

# filter training and test set for selected features
methyl_dev <- methyl_dev[, feat_sel_index]
methyl_test <- methyl_test[, feat_sel_index]


#### Model fitting ####

# train control: 5-fold cross validation
cv <- trainControl(method = "cv", number = 5, p = .2, 
                   classProbs = TRUE, savePredictions = TRUE)


# ridge regression (alpha = 0)
if (rver < 360) {set.seed(42)} else {set.seed(42, sample.kind = "Rounding")}
fit.ridge <- train(x = methyl_dev, y = pheno_dev$disease_state,
                   method = "glmnet", trControl = cv, 
                   tuneGrid = data.frame(alpha = 0,
                                         lambda = seq(0.0001, 1, length = 100)))

# lasso regression (alpha = 1)
if (rver < 360) {set.seed(42)} else {set.seed(42, sample.kind = "Rounding")}
fit.lasso <- train(x = methyl_dev, y = pheno_dev$disease_state,
                   method = "glmnet", trControl = cv, 
                   tuneGrid = data.frame(alpha = 1,
                                         lambda = seq(0.0001, 1, length = 100)))

# elastic net regression (0 < alpha < 1)
if (rver < 360) {set.seed(42)} else {set.seed(42, sample.kind = "Rounding")}
fit.elasticnet <- train(x = methyl_dev, y = pheno_dev$disease_state,
                        method = "glmnet", trControl = cv, 
                        tuneGrid = expand.grid(alpha = seq(0.1, 0.9, 0.1),
                                               lambda = seq(0.0001, 1, 
                                                            length = 100)))


# additional models 
models.add <- c("glm", "lda", "knn", "rf", "svmLinear")

tunegrids.add <- list(glm = NULL,
                      lda = NULL,
                      svmLinear = data.frame(C = seq(0, 2, length = 20)),
                      gamLoess = expand.grid(span = seq(0.15, 0.65, len = 10), 
                                             degree = 1),
                      rf = data.frame(mtry = 1:7),
                      knn = data.frame(k = seq(3, 51, 2)))

fits.add <- lapply(models.add, function(model){
  if (rver < 360) {set.seed(42)} else {set.seed(42, sample.kind = "Rounding")}
  train(x = methyl_dev, y = pheno_dev$disease_state, 
        trControl = cv, tuneGrid = tunegrids.add[[model]],
        method = model)
})


# combine all models into single object
fits <- c(list(fit.ridge, fit.lasso, fit.elasticnet), fits.add) %>% 
  set_names(c("ridge", "lasso", "elasticnet", models.add))

# select models for ensemble prediction
training_accuracies <- sapply(fits, function(fit) mean(fit$resample$Accuracy))
models_keep <- training_accuracies >= 0.8


#### Model evaluation ####

# function to calculate model metrics: accuracy, recall, precision, F1 score
get_metrics <- function(prediction, truth = pheno_test$disease_state) {
  prediction <- factor(prediction, levels = c("Smoker", "Control"))
  data.frame(F1_score   = F_meas(prediction, truth),
             Accuracy  = mean(prediction == truth),
             Recall    = recall(prediction, truth),
             Precision = precision(prediction, truth))
}


# predictions on hold-out testing set for all individual models
pred <- sapply(fits, predict, methyl_test)

# ensemble prediction only with models that have a training accuracy > 0.8
ensemble <- apply(pred[, models_keep], 1, 
                  function(i) names(sort(table(i), decreasing = T))[1])

# combine single-model predictions with ensemble prediction
pred <- cbind(pred, "ensemble" = ensemble)


# results table ordered by F1 score
res <- apply(pred, 2, get_metrics) %>% bind_rows(.id = "Model") %>% 
  arrange(-F1_score)


# plot ROC curves with AUC values
mleval <- evalm(fits, gnames = names(fits), plots = "r", positive = "Smoker")


# plot confusion matrices for all models
cmtables <- sapply(res$Model, simplify = "array", function(pred_name) {
  cm <- confusionMatrix(data = factor(pred[, pred_name], 
                                      levels = c("Smoker", "Control")), 
                        reference = pheno_test$disease_state)
  return(cm$table)
})
names(dimnames(cmtables))[3] <- "Model"
fourfoldplot(cmtables, mfcol = c(2, 5))


# visualize sample-level concordance of predictions between models
bind_cols(pheno_test, as.data.frame(pred)) %>%
  pivot_longer(cols = colnames(pred), 
               names_to = "Model", values_to = "Prediction") %>% 
  mutate(accurate = (disease_state == Prediction)) %>% 
  ggplot(aes(geo_accession, Model, fill = disease_state, alpha = accurate)) +
  geom_tile(color = "white") +
  scale_alpha_discrete(range = c(0.5, 1)) +
  theme(axis.text.x = element_blank()) +
  labs(x = "Sample", alpha = "Accurate Prediction") +
  guides(fill = FALSE) +
  facet_grid(. ~ disease_state, scales = "free_x")

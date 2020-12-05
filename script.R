#!/usr/bin/env Rscript
#
# Title: ""HarvardX: PH125.9x Capstone - Choose your own"
# Author: "Friederike David"
#
# In this project, smoking status is sought to be predicted from methylation
# data downloaded from GEO


#### Prerequisites ####

# employed packages from cran/bioconductor
cran <- c(
  "tidyverse",    # load multiple 'tidyverse' packages in a single step
  "here",         # locate files within project
  "data.table",   # fast reading and writing of tabular data
  "BiocManager",  # install Bioconductor packages
  "caret",        # classification and regression training
  "glmnet",       # method = "glmnet"
  "naivebayes",   # method = "naive_bayes"
  "kernlab",      # method = "svmLinear"    
  "randomForest", # method = "rf"
  "patchwork"     # plotting
)
lapply(cran, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
    library(pkg, character.only = TRUE)
  }
})

bioc <- c("GEOquery")     # Bioconductor package to download data from NCBI GEO
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


#### Data input and preparation ####

# load data from GEO
geo_id <- "GSE53045"
geo <- getGEO(geo_id, destdir = here("data"))

# # download raw data files
# getGEOSuppFiles(geo_id, baseDir = here("data"))
# geo_files <- list.files(here("data", geo_id))
# 
# geo_supp <- lapply(geo_files, function(file) {
#   fread(file = here("data", geo_id, file))
# }) %>% set_names(geo_files)
# 
# supp_mat <- as.data.frame(geo_supp$GSE53045_matrix_processed_GEO.txt.gz)
# supp_methyl <- select(supp_mat, -`Detection Pval`) %>% column_to_rownames("ID_REF")
# supp_samples <- colnames(supp_methyl)
# supp_detectpval <- supp_mat[, seq(1, ncol(supp_mat), 2)] %>% 
#   column_to_rownames("ID_REF") %>% set_names(supp_samples)


# extract relevant phenotypic data and convert outcome (group) to numerical variable
pheno <- pData(geo[[1]]) %>% 
  select(geo_accession, biosample = source_name_ch1, 
         disease_state = `disease state:ch1`) %>% 
  mutate(disease_state = factor(disease_state),
         group = ifelse(disease_state == "Smoker", 1, 0) %>% factor())

# extract methylation data
methyl <- geo[[1]]@assayData$exprs %>% t()

# extract meta data on methylation probes
probes <- fData(geo[[1]])

# remove probes with missing values
probes_remove <- apply(methyl, 2, function(x) any(is.na(x)))
methyl <- methyl[, !probes_remove]
probes <- probes[!probes_remove, ]

# split sample into training and validation set
if (rver < 360) {set.seed(1)} else {set.seed(1, sample.kind = "Rounding")}
test_index <- createDataPartition(y = pheno$disease_state, p = .2, list = F)

methyl_train <- methyl[-test_index, ]
methyl_test <- methyl[test_index, ]

pheno_train <- pheno[-test_index, ]
pheno_test <- pheno[test_index, ]

# clean environment to free RAM
rm(bioc, cran, geo, methyl, pheno, probes, probes_remove, test_index)


# #### Data exploration ####
# 
# dim(methyl_train)
# pca <- prcomp(methyl_train)
# 
# pca.df <- pca$x %>% as_tibble() %>%
#   mutate(group = pheno_train$disease_state)
# 
# p1 <- ggplot(pca.df, aes(PC1, PC2, col = group)) + geom_point()
# p2 <- ggplot(pca.df, aes(PC1, PC3, col = group)) + geom_point()
# p3 <- ggplot(pca.df, aes(PC3, PC2, col = group)) + geom_point()
# 
# p2 + grid::textGrob('') + p1 + p3 + 
#   plot_layout(nrow = 2, guides = "collect") &
#   theme(legend.position = "bottom")
# 
# rm(pca, pca.df, p1, p2, p3)


#### Model fitting ####

# # type of regularized regression: 0 = ridge, 1 = lasso, 0.3 = elastic net
# alpha <- 1
# 
# # find best lambda (i.e. amount of shrinkage) via cross-validation
# cv.lambda <- cv.glmnet(x = methyl_train, y = pheno_train$group,
#                        family = "binomial", # response type: binary outcome
#                        alpha = alpha)
# plot(cv.lambda)
# 
# # penalized logistic regression in form of elastic regression
# fit <- glmnet(x = methyl_train, y = pheno_train$group,
#               family = "binomial", # response type: binary outcome
#               alpha = alpha,         
#               lambda = cv.lambda$lambda.min) 

## DEBUG
n <- 10000
methyl_test <- methyl_test[, 1:n]
methyl_train <- methyl_train[, 1:n]


# train control: 5-fold cross validation
control <- trainControl(method = "cv", number = 5, p = .2)

models <- c("lda", "glm", "glmnet", "naive_bayes", "svmLinear", "rf", "knn")

tunegrids <- list(lda = NULL,
                  glm = NULL,
                  glmnet = expand.grid(alpha = seq(0, 1, 0.25),
                                       lambda = seq(0.0001, 1, length = 100)),
                  naive_bayes = data.frame(laplace = c(0, 0.5, 1), usekernel = TRUE, 
                                           adjust = c(0, 0.5, 1)),
                  svmLinear = data.frame(C = seq(0, 2, length = 20)),
                  gamLoess = expand.grid(span = seq(0.15, 0.65, len = 10), 
                                         degree = 1),
                  rf = data.frame(mtry = 1:7),
                  knn = data.frame(k = seq(3, 51, 2)))

fits <- lapply(models, function(model){
  print(model)
  fit <- train(x = methyl_train, y = pheno_train$disease_state, 
        trControl = control, tuneGrid = tunegrids[[model]],
        method = model)
  
  if (!is.null(tunegrids[[model]])) print(plot(fit) + theme_classic())
  
  return(fit)
})

training_accuracies <- sapply(fits, function(fit) mean(fit$resample$Accuracy))
models_keep <- training_accuracies >= 0.8


#### Model evaluation ####


# predicted groups in test data for different models
predictions <- sapply(fits, predict, methyl_test)

# model accuracies
accuracies <- apply(predictions, 2,
                    function(pred) mean(pheno_test$disease_state == pred))
accuracies; mean(accuracies)

# ensemble prediction (all vs. selected models)
ensemble_pred <- apply(predictions, 1, 
                       function(i) names(sort(table(i), decreasing = T))[1])
mean(pheno_test$disease_state == ensemble_pred) 

ensemble_pred <- apply(predictions[,models_keep], 1,
                       function(i) names(sort(table(i), decreasing = T))[1])
mean(pheno_test$disease_state == ensemble_pred)


# confusionMatrix(pheno_test_pred, pheno_test$disease_state)
# plot(pheno_test$disease_state, 
#      predict(fit, newdata = methyl_test, type = "prob")$Smoker)

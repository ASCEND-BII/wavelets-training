################################################################################
#' @title Comparison of wavelet spectra for predicting leaf traits
################################################################################

#' @author J. Antonio Guzmán Q., University of Minnesota

#-------------------------------------------------------------------------------
#' @step-1 Loading libraries

library(data.table)
library(CWT)
library(pls)
library(caret)
library(parallel)

#-------------------------------------------------------------------------------
#' @step-2 Read files and define datasets

# Read file
file <- fread("data/spectra.csv", header = TRUE)

# Select columns with metadata
meta <- file

# Select columns with leaf traits
traits <- file[, .SD, .SDcols = c(17:20)]

# Select columns with reflectance spectra
reflectance <- file[, .SD, .SDcols = c(60:ncol(file))]

#-------------------------------------------------------------------------------
#' @step-3 Perform transformations and select regions of interest

# Estimate vector normalization
normalization <- sqrt(apply(reflectance^2, 1, sum, na.rm = TRUE))
normalization <- reflectance / normalization

# Estimate wavelet spectra
wavelet <- cwt(reflectance, 
               scales = c(2, 4, 6),
               summed_wavelet = TRUE,
               threads = 1L)

#-------------------------------------------------------------------------------
#' @step-4 Define partitions for training, testing, and cross-validation

# Define vector for training (i.e, split) and testing (i.e, !split).
split <- createDataPartition(y = 1:nrow(meta),
                             p = 0.6)
split <- split$Resample1

# Define cross-validation strategy
# Create a vector file
folds <- createFolds(y = split, 
                     k = 10, 
                     list = TRUE, 
                     returnTrain = FALSE)


#-------------------------------------------------------------------------------
#' @step-5 Define optimal numbers of components

#-------------------------------------------------------------------------------
#' @step-6 Run final model

#-------------------------------------------------------------------------------
#' @step-7 Evaluate performance

#' #----------------------------------------------------------------------------
#' @step-8 Evaluate importance of bands (VIP)
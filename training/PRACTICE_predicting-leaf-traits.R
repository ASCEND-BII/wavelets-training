################################################################################
#' @title Comparison of wavelet spectra for predicting leaf traits
################################################################################

#' @author J. Antonio Guzmán Q., University of Minnesota

#-------------------------------------------------------------------------------
#' @step-1 Loading libraries and source code

# Libraries
library(data.table)
library(CWT)
library(pls)
library(parallel)

# Source code
source("training/iterative_cross_valiation.R")


#-------------------------------------------------------------------------------
#' @step-2 Read files and define datasets

# Read file
file <- fread("data/file.csv", header = TRUE)

# Sample ID (may be your metadata)
sample_id <- file$Sample_ID

# Select columns with leaf traits
traits <- file[, .SD, .SDcols = c(2:6)]

# Select columns with reflectance spectra
reflectance <- file[, .SD, .SDcols = c(7:ncol(file))]

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
#' @step-4 Clean datasets and select range

# Select trait
trait <- traits$LMA

# Cleaning for training
reflectance <- reflectance[!is.na(trait), ]
normalization <- normalization[!is.na(trait), ]
wavelet <- wavelet[!is.na(trait), ]
trait <- trait[!is.na(trait)]

# Range
reflectance <- reflectance[, .SD, ]
normalization <- normalization[, .SD, ]
wavelet <- wavelet[, .SD, ]

#-------------------------------------------------------------------------------
#' @step-5 Define partitions for training, testing, and cross-validation


# Define vector for training (i.e, split) and testing (i.e, !split).
split <- createDataPartition(y = 1:length(trait),
                             p = 0.6)
split <- split$Resample1

# Define cross-validation strategy
icv <- iterative_cross_valiation((1:length(trait))[split],
                                 psamples = 0.8,
                                 k = 10,
                                 times = 30)

#-------------------------------------------------------------------------------
#' @step-6 Build models 

threads <- 1

ref_model <- models(trait = trait, 
                    spectra = reflectance, 
                    split = split,
                    icv = icv,
                    ncomp_max = 30,
                    threads = 1)

vn_model <- models(trait = trait, 
                   spectra = normalization, 
                   split = split,
                   icv = icv,
                   ncomp_max = 30,
                   threads = 1)

wav_model <- models(trait = trait, 
                    spectra = wavelet, 
                    split = split,
                    icv = icv,
                    ncomp_max = 30,
                    threads = 1)

#-------------------------------------------------------------------------------
#' @step-7 Run final model

#-------------------------------------------------------------------------------
#' @step-8 Evaluate performance

#' #----------------------------------------------------------------------------
#' @step-9 Evaluate importance of bands (VIP)
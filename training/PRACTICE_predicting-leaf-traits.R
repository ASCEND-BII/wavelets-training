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
library(plsVarSel)
library(parallel)
library(ggplot2)

# Source code
source("training/iterative_cross_valiation.R")
source("training/iterative_models.R")
source("training/iterative_PRESS.R")
source("training/iterative_VIP.R")
source("training/iterative_predict.R")
source("training/iterative_performance.R")
source("figures/figure_PRESS.R")
source("figures/figure_VIP.R")
source("figures/figure_obs-pred.R")

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
               scales = c(2, 4, 6, 8),
               summed_wavelet = TRUE,
               threads = 1L)

#-------------------------------------------------------------------------------
#' @step-4 Clean datasets and select range

# Select trait
trait <- traits$C_mass

# Cleaning for training
reflectance <- reflectance[!is.na(trait), ]
normalization <- normalization[!is.na(trait), ]
wavelet <- wavelet[!is.na(trait), ]
trait <- trait[!is.na(trait)]

# Range (425 nm to 2375)
reflectance <- reflectance[, .SD, .SDcols = 25:1975]
normalization <- normalization[, .SD, .SDcols = 25:1975]
wavelet <- wavelet[, .SD, .SDcols = 25:1975]

#-------------------------------------------------------------------------------
#' @step-5 Define partitions for training, testing, and cross-validation

# Define vector for training (i.e, split) and testing (i.e, !split).
split <- sample(x = 1:nrow(reflectance), 
                size = ceiling(nrow(reflectance)*0.6))
split <- split[order(split)]

# Define cross-validation strategy
icv <- iterative_cross_valiation((1:length(trait))[split],
                                 psamples = 0.8,
                                 k = 10,
                                 times = 30)

#-------------------------------------------------------------------------------
#' @step-6 Build models 

threads <- 16

ref_model <- iterative_models(trait = trait, 
                              spectra = reflectance, 
                              split = split,
                              icv = icv,
                              ncomp_max = 30,
                              threads = threads)

vn_model <- iterative_models(trait = trait, 
                             spectra = normalization, 
                             split = split,
                             icv = icv,
                             ncomp_max = 30,
                             threads = threads)

wav_model <- iterative_models(trait = trait, 
                              spectra = wavelet, 
                              split = split,
                              icv = icv,
                              ncomp_max = 30,
                              threads = threads)

#-------------------------------------------------------------------------------
#' @step-7 Get optimal number of components

# Get PRESS
ref_press <- iterative_PRESS(models = ref_model)
vn_press <- iterative_PRESS(models = vn_model)
wav_press <- iterative_PRESS(models = wav_model)

# Visualize the optimal number of components
figure_PRESS(ref_press, PRESS_range = range(ref_press), title = "Reflectance")
figure_PRESS(vn_press, PRESS_range = range(ref_press), title = "Vector Normalization")
figure_PRESS(wav_press, PRESS_range = range(ref_press), title = "Wavelet")

ncomp_models <- c(27, 26, 18)

#-------------------------------------------------------------------------------
#' @step-8 Evaluate the Value of Importance in Projection

# Get VIP
ref_vip <- iterative_VIP(ref_model, ncomp = ncomp_models[1])
vn_vip <- iterative_VIP(vn_model, ncomp = ncomp_models[2])
wav_vip <- iterative_VIP(wav_model, ncomp = ncomp_models[3])

# Visualize VIPs
figure_VIP(ref_vip, 
           Wavelength_range = c(424:2374),
           VIP_range = c(0, 4),
           title = "Reflectance")

figure_VIP(vn_vip, 
           Wavelength_range = c(424:2374),
           VIP_range = c(0, 4),
           title = "Vector Normalization")

figure_VIP(wav_vip, 
           Wavelength_range = c(424:2374),
           VIP_range = c(0, 4),
           title = "Wavelet")

#' #----------------------------------------------------------------------------
#' @step-9 Asses the performance

# Apply models
ref_testing <- iterative_predict(ref_model, 
                                 spectra = reflectance[!split,],
                                 ncomp = ncomp_models[1])

vn_testing <- iterative_predict(vn_model, 
                                spectra = normalization[!split,],
                                ncomp = ncomp_models[2])

wav_testing <- iterative_predict(wav_model, 
                                 spectra = wavelet[!split,],
                                 ncomp = ncomp_models[3])

# Estimate performance
ref_performance <- iterative_performance(observed = trait[-split],
                                         predicted = ref_testing)

vn_performance <- iterative_performance(observed = trait[-split],
                                        predicted = vn_testing)

wav_performance <- iterative_performance(observed = trait[-split],
                                         predicted = wav_testing)


# Observed and predicted
figure_obs_pred(observed = trait[-split],
                predicted = ref_testing,
                performance = ref_performance,
                title = "Reflectance")

figure_obs_pred(observed = trait[-split],
                predicted = vn_testing,
                performance = vn_performance,
                title = "Vector Normalization")

figure_obs_pred(observed = trait[-split],
                predicted = wav_testing,
                performance = wav_performance,
                title = "Wavelet")


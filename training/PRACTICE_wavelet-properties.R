################################################################################
#' @title Exploration of properties of Continuous Wavelet Transformation
################################################################################

#' @author J. Antonio Guzmán Q., University of Minnesota

#' @description
#' The goal of this script is to familiarize users on the application of CWT to
#' time-series; specifically those that comes from spectroscopy. Steps 2 and 4 users
#' will assess of amplitude and magnitude of the signals on resulting transformations.
#' Steps 5 and 6 users will evaluate the impact of changes in frequencies and the
#' presence of noise on resulting transformations. Finaly, steps 7 ans 8 provides
#' examples on the application of signals of reflectance spectra from leaf and
#' airborne spectroscopy derived from RTM. 

#-------------------------------------------------------------------------------
#' @step-1 Loading libraries

# Libraries
library(data.table)
library(CWT)
library(ccrtm)

#-------------------------------------------------------------------------------
#' @step-2 A basic wavelet application using a time-series with changes 
#' in amplitude

# Define the amplitude modulation function
amplitude_modulation <- function(t) {
  return(sin(2 * pi * t) / (1 + 0.1 * t))
}

# Generate the time vector
time <- seq(0, 20, by = 0.01)

# Generate signal with changes in frequency
signal_amplitude <- amplitude_modulation(time)
plot(signal_amplitude)

# Apply the CWT
wavelet_amplitude <- cwt(signal_amplitude,
                         scales = 1:5)

# Dimensions (samples, transformed signal, scales)
dim(wavelet_amplitude)

# Play with the scales
plot(wavelet_amplitude[1,,1], ylim = range(signal_amplitude), main = "Scale 1")
plot(wavelet_amplitude[1,,2], ylim = range(signal_amplitude), main = "Scale 2")
plot(wavelet_amplitude[1,,3], ylim = range(signal_amplitude), main = "Scale 3")
plot(wavelet_amplitude[1,,4], ylim = range(signal_amplitude), main = "Scale 5")
plot(wavelet_amplitude[1,,5], ylim = range(signal_amplitude), main = "Scale 4")

#-------------------------------------------------------------------------------
#' @step-3 Comparison of time-series with different magnitudes and same amplitudes 

# Let's increase the basal magnitude of the signal with changes in amplitude
higher_magnitude <- signal_amplitude - min(signal_amplitude)
plot(higher_magnitude)
plot(signal_amplitude, higher_magnitude)

# Apply the CWT
wavelet_higher_magnitude <- cwt(higher_magnitude,
                                scales = 1:5)

# Dimensions (samples, transformed signal, scales)
dim(wavelet_higher_magnitude)

# Play with the scales
plot(wavelet_higher_magnitude[1,,1], ylim = range(signal_amplitude), main = "Scale 1")
plot(wavelet_higher_magnitude[1,,2], ylim = range(signal_amplitude), main = "Scale 2")
plot(wavelet_higher_magnitude[1,,3], ylim = range(signal_amplitude), main = "Scale 3")
plot(wavelet_higher_magnitude[1,,4], ylim = range(signal_amplitude), main = "Scale 5")
plot(wavelet_higher_magnitude[1,,5], ylim = range(signal_amplitude), main = "Scale 4")

# Let's compare signals with different magnitudes both similar amplitudes
plot(wavelet_amplitude[1,,3], ylim = range(signal_amplitude), main = "Scale 3")
plot(wavelet_higher_magnitude[1,,3], ylim = range(signal_amplitude), main = "Scale 3")
plot(wavelet_amplitude[1,100:900,3], wavelet_higher_magnitude[1,100:900,3])

#-------------------------------------------------------------------------------
#' @step-4 A basic wavelet application using a time-series with changes 
#' in frequency

# Define the frequency modulation function
frequency_modulation <- function(t) {
  return(sin((0.75 * pi * (1 + 0.1 * t)) * t))
}

# Generate the time vector
time <- seq(0, 20, by = 0.01)

# Generate signal with changes in frequency
signal_frecuency <- frequency_modulation(time)
plot(signal_frecuency, type = "l")

# Apply the CWT
wavelet_frecuency <- cwt(signal_frecuency,
                         scales = 1:5)

# Dimensions (samples, transformed signal, scales)
dim(wavelet_frecuency)

# Play with the scales
plot(wavelet_frecuency[1,,1], ylim = range(signal_frecuency), main = "Scale 1")
plot(wavelet_frecuency[1,,2], ylim = range(signal_frecuency), main = "Scale 2")
plot(wavelet_frecuency[1,,3], ylim = range(signal_frecuency), main = "Scale 3")
plot(wavelet_frecuency[1,,4], ylim = range(signal_frecuency), main = "Scale 4")
plot(wavelet_frecuency[1,,5], ylim = range(signal_frecuency), main = "Scale 5")

#-------------------------------------------------------------------------------
#' @step-5 Comparison of time-series with same frequencies but different noise 

# Let's add noise to our previews signal
noise <- rnorm(n = length(time), sd = 0.01)
signal_noise <- signal_frecuency + noise
plot(signal_noise)
plot(signal_frecuency, signal_noise)

# Apply the CWT
wavelet_noise <- cwt(signal_noise,
                     scales = 1:5)

# Dimensions (samples, transformed signal, scales)
dim(wavelet_noise)

# Play with the scales
plot(wavelet_noise[1,,1], ylim = range(wavelet_noise), main = "Scale 1")
plot(wavelet_noise[1,,2], ylim = range(wavelet_noise), main = "Scale 2")
plot(wavelet_noise[1,,3], ylim = range(wavelet_noise), main = "Scale 3")
plot(wavelet_noise[1,,4], ylim = range(wavelet_noise), main = "Scale 4")
plot(wavelet_noise[1,,5], ylim = range(wavelet_noise), main = "Scale 5")

# Let's compare signals with different noise but same frequencies
plot(wavelet_frecuency[1,,3], ylim = range(signal_frecuency), main = "Scale 3")
plot(wavelet_noise[1,,3], ylim = range(signal_frecuency), main = "Scale 3")
plot(wavelet_frecuency[1,50:250,3], wavelet_noise[1,50:250,3])
plot(wavelet_frecuency[1,250:750,3], wavelet_noise[1,250:750,3])
plot(wavelet_frecuency[1,750:950,3], wavelet_noise[1,750:950,3])

#-------------------------------------------------------------------------------
#' @step-6 Example a bit more real using leaf reflectance spectra and noise

# Let's get a leaf reflectance spectra using RTM (Prospect5)
reflectance <-fRTM(rho~prospect5)
plot(reflectance)
leaf_reflectance <- as.vector(reflectance)

# Apply the CWT
wavelet_leaf <- cwt(leaf_reflectance, 
                      scales = 1:10)

# Dimensions (samples, transformed signal, scales)
dim(wavelet_leaf)

# Play with the scales
plot(400:2500, wavelet_leaf[1,,1], ylim = range(wavelet_leaf), main = "Scale 1")
plot(400:2500, wavelet_leaf[1,,2], ylim = range(wavelet_leaf), main = "Scale 2")
plot(400:2500, wavelet_leaf[1,,4], ylim = range(wavelet_leaf), main = "Scale 4")
plot(400:2500, wavelet_leaf[1,,6], ylim = range(wavelet_leaf), main = "Scale 6")
plot(400:2500, wavelet_leaf[1,,8], ylim = range(wavelet_leaf), main = "Scale 8")

# Let's apply noise
noise <- rnorm(n = length(leaf_reflectance), sd = 0.001)
leaf_noise <- leaf_reflectance + noise
plot(leaf_noise, type = "l")

# Apply the CWT
wavelet_leaf_noise <- cwt(leaf_noise,
                          scales = 1:5)

# Play with the scales
plot(wavelet_leaf_noise[1,,1], ylim = range(wavelet_leaf_noise), main = "Scale 1")
plot(wavelet_leaf_noise[1,,2], ylim = range(wavelet_leaf_noise), main = "Scale 2")
plot(wavelet_leaf_noise[1,,3], ylim = range(wavelet_leaf_noise), main = "Scale 3")
plot(wavelet_leaf_noise[1,,4], ylim = range(wavelet_leaf_noise), main = "Scale 4")
plot(wavelet_leaf_noise[1,,5], ylim = range(wavelet_leaf_noise), main = "Scale 5")

# Compare signals 
plot(wavelet_leaf[1,,4], ylim = range(wavelet_leaf), main = "Scale 3")
plot(wavelet_leaf_noise[1,,4], ylim = range(wavelet_leaf), main = "Scale 3")
plot(wavelet_leaf[1,,4], wavelet_leaf_noise[1,,4])
abline(a = 0, b = 1)

#-------------------------------------------------------------------------------
#' @step-7 Example a bit more real using canopy reflectance spectra from different
#' viewing-angles

# Let's modify the viewing angle of 4SAIL
canopy_reflectance <-fRTM(rho~prospectd+foursail)
plot(canopy_reflectance)
canopy_reflectance <- t(as.data.frame(canopy_reflectance))

#  Bi-hemispherical reflectance (rddt).
#  Hemispherical directional reflectance (rsdt).
#  Directional hemispherical reflectance (rdot).
#  Bi-directional reflectance (rsot).

# Let's make the spectra a bit more real (4 nm band spacing0)
canopy_reflectance <- canopy_reflectance[,seq(1, ncol(canopy_reflectance), 4)]

# Apply the CWT
wavelet_canopy <- cwt(as.data.table(canopy_reflectance),
                      scales = 1:10)

# Dimensions (samples, transformed signal, scales)
dim(wavelet_canopy)

# Play with the scales and types of reflectance
scale <- 5
plot(seq(400, 2500, 4), wavelet_canopy[1,,scale], ylim = range(wavelet_canopy), main = paste0("Bi-hemispherical reflectance - Scale ", scale))
plot(seq(400, 2500, 4), wavelet_canopy[2,,scale], ylim = range(wavelet_canopy), main = paste0("Hemispherical directional reflectance - Scale ", scale))
plot(seq(400, 2500, 4), wavelet_canopy[3,,scale], ylim = range(wavelet_canopy), main = paste0("Directional hemispherical reflectance - Scale ", scale))
plot(seq(400, 2500, 4), wavelet_canopy[4,,scale], ylim = range(wavelet_canopy), main = paste0("Bi-directional reflectance - Scale ", scale))

# Compare rddt and rsdt
plot(canopy_reflectance[1,5:520], canopy_reflectance[4,5:520], main = "Reflectance")
abline(a = 0, b = 1)
plot(wavelet_canopy[1,5:520,scale], wavelet_canopy[4,5:520,scale], main = "Wavelet")
abline(a = 0, b = 1)

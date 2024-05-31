################################################################################
#' @title Exploration of properties of wavelet spectra
################################################################################

#' @author J. Antonio Guzmán Q., University of Minnesota

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
#' @step-6 Let's make an example a bit more real using reflectance spectra 


# histogram.R
# Copyright (C) flossCoder 2016
#
# This file is part of dataAnalysis
#
# evacuationmodel is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# evacuationmodel is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Plot the given histogram.
#' 
#' @param histogram The given histogram.
#' @param categories The categories which should be counted.
plotHistogram <- function(histogram, categories) {
  maxVal <- max(histogram[[2]])
  plot(histogram[[2]], axes = FALSE, xlab = "", ylab = "probability")
  axis(1, at = seq(1, length(categories), by = 1), labels = categories, las = 2)
  axis(2, at = seq(0, maxVal, by = maxVal / 5))
}

#' Calculate the probability distribution of the given histogram.
#' 
#' @param histogram The given histogram.
#' 
#' @return The probability distribution of the given histogram.
probabilityDistributionOfHistogram <- function(histogram) {
  numberOfEntries <- numberOfHistogramEntries(histogram)
  for (i in 1:dim(histogram)[1]) {
    histogram[[2]][i] <- histogram[[2]][i] / numberOfEntries
  }
  return(histogram)
}

#' Sum up two histograms.
#' 
#' @param histogram1 The first histogram.
#' @param histogram2 The second histogram.
#' 
#' @return The sum of the two histograms.
#' 
#' @error Histograms differ in length.
#' @error Histograms differ in categories.
addHistograms <- function(histogram1, histogram2) {
  if (dim(histogram1)[1] != dim(histogram2)[1]) {
    stop("Histograms differ in length.")
  }
  
  for (i in 1:dim(histogram1)[1]) {
    if (histogram1[[1]][i] != histogram2[[1]][i]) {
      stop(paste("Histograms differ in categories ", toString(histogram1[[1]][i]), " and ", toString(histogram2[[1]][i]), " in line ", toString(i), ".", sep = ""))
    }
    
    histogram1[[2]][i] <- histogram1[[2]][i] + histogram2[[2]][i]
  }
  return(histogram1)
}

#' Calculate the number of entries of the given histogram.
#' 
#' @param histogram The given histogram.
#' 
#' @return The number of entries in the histogram.
numberOfHistogramEntries <- function(histogram) {
  counter <- 0
  for (i in 1:dim(histogram)[1]) {
    counter <- counter + histogram[[2]][i]
  }
  return(counter)
}
#' Calculate a histogram of the occurencies of all entries of the given
#' categories in the column of the input data.
#' 
#' @param data The input data frame.
#' @param column The columne which should be checked.
#' @param categories The categories which should be counted.
#' 
#' @return The histogram containing the calculated occurencies.
calculateHistogramOfEntries <- function(data, column, categories) {
  histogram <- data.frame(matrix(NA, length(categories), 2))
  histogram[[1]] <- categories
  for (i in 1:length(categories)) {
    histogram[i, 2] <- countOccurencyOfEntry(data, column, categories[[i]])
  }
  return(histogram)
}
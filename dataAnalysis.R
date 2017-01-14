# dataAnalysis.R
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

# Set up r-file sources.

# Function from the ?source documentation.
sourceDir <- function(path, thisFile, trace = TRUE, ...) {
	for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
		if (trace) {
			cat(file.path(path, nm))
		}
		if (file.path(path, nm) != file.path(path, thisFile)) {
			source(file.path(path, nm))
		}
		if (trace) {
			cat("\n")
		}
	}
}

# source all files in the same directory of main.r
sourceDir(dirname(sys.frame(1)$ofile), "dataAnalysis.R")

#' Do the data analysis.
#' 
#' @param directory The directory where the file can be found.
#' @param filename The name of the file.
#' @param string The string that should be removed.
#' @param columnIn Column of the data that should be checked against the result.
#' @param columnResult The result column (finally a histogram will be calculated).
#' @param numberOfShuffles How many shuffling operation should be performed?
#' 
#' @return The probability distribution of the given histogram.
dataAnalysis <- function(directory, filename, string, columnIn, columnResult, numberOfShuffles) {
  data <- readData(directory, filename)
  if (string != "") {
    data <- removeLinesContainingString(data, columnIn, string)
  }
  categories <- generateListOfEntries(data, columnResult)
  histogram <- calculateHistogramOfEntries(data, columnResult, categories)
  for (i in 1:numberOfShuffles) {
    new <- calculateHistogramOfEntries(shuffleData(data, columnIn), columnResult, categories)
    histogram <- addHistograms(histogram, new)
  }
  histogram <- probabilityDistributionOfHistogram(histogram)
  plotHistogram(histogram, categories)
  return(histogram)
}

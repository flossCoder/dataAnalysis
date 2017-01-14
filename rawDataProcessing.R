# rawDataProcessing.R
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

#' Determine the number of occurencies of the string in the column.
#' 
#' @param data The input data frame.
#' @param column The columne which should be checked.
#' @param string The string that should be removed.
#' 
#' @return The number of occurencies of the string.
countOccurencyOfEntry <- function(data, column, string) {
  counter <- 0
  for (i in 1:dim(data)[1]) {
    entry <- strsplit(toString(data[[column]][i]), ";")
    if (entry == string) {
      counter <- counter + 1
    }
  }
  return(counter)
}

#' Generate a list of all entries that occure in the given column.
#' 
#' @param data The input data frame.
#' @param column The columne which should be checked.
#' 
#' @return The list of entries.
generateListOfEntries <- function(data, column) {
  entries <- c()
  for (i in 1:dim(data)[1]) {
    entry <- strsplit(toString(data[[column]][i]), ";")
    for (j in 1:length(entry[[1]])) {
      if (!entry[[1]][j] %in% names(entries)) {
        entries[length(entries) + 1] <- entry[[1]][j]
        names(entries)[length(entries)] <- entry[[1]][j]
      }
    }
  }
  return(entries)
}

#' Shuffle the data in the given column: Each entry moves one position up.
#' The first entry and the final row of the data frame are deleted.
#' 
#' @param data The input data frame.
#' @param column The columne which should be checked.
#' 
#' @return The data frame after shuffling.
shuffleData <- function(data, column) {
  for (i in 1:(dim(data)[1] - 1)) {
    data[[column]][i] <- data[[column]][i + 1]
  }
  keepRow <- matrix(TRUE, 1, dim(data)[1])
  keepRow[dim(data)[1]] <- FALSE # throw last element away
  return(data[keepRow,])
}

#' Remove all lines of the given data frame where string exists in the column.
#' 
#' @param data The input data frame.
#' @param column The columne which should be checked.
#' @param string The string that should be removed.
#' 
#' @return The filtered data frame.
removeLinesContainingString <- function(data, column, string) {
  return(data[data[[column]] != string,])
}

#' Read in a csv file containing the data.
#' 
#' @param directory The directory where the file can be found.
#' @param filename The name of the file.
#' 
#' @return A data frame containing the read data.
readData <- function(directory, filename) {
  return(read.table(paste(directory, "/", filename, ".csv", sep = ""), header = FALSE, sep = ","))
}
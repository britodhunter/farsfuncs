% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/farsfunc.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Create Map of Accidents in Each State}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{An integer value assigned to each state or territory within the United States (1-56).}

\item{year}{The year the user would like printed in the file name tag.}
}
\value{
A plot of the state map and marks for recorded accidents by geograhical location (longitude and latitude).
}
\description{
This function creates a map graph of a given state and plots the location of accidents within that state for the specified year.
}
\note{
If a state number is entered that does not have data or year is entered into the function that cannot be connected to a dataset located on the user's computer,
         an error message will appear. If there are no accidents within the state with the year specified, an error will appear.
}

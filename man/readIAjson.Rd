% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_json.r
\name{readIAjson}
\alias{readIAjson}
\title{Read json-data from IA}
\usage{
readIAjson(json_file = NULL)
}
\arguments{
\item{json_file}{The json file used by IA}
}
\value{
A data frame
}
\description{
All the data used to produce an Instant Atlas is stored in
a json file. This function will read such a file and put the
data into a data frame.
}

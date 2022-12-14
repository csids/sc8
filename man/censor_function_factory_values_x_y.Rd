% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r6_SchemaRedirect.r
\name{censor_function_factory_values_x_y}
\alias{censor_function_factory_values_x_y}
\title{censor x-y function factory}
\usage{
censor_function_factory_values_x_y(
  column_name_to_be_censored,
  column_name_value = column_name_to_be_censored,
  censored_value = 0,
  lower_value = 0,
  upper_value = 4,
  granularity_time = NULL,
  granularity_time_not = NULL,
  granularity_geo = NULL,
  granularity_geo_not = NULL
)
}
\arguments{
\item{column_name_to_be_censored}{Name of the column to be censored}

\item{column_name_value}{Name of the column whose value is determining if something should be censored}

\item{censored_value}{The value that censored data will be set to}

\item{lower_value}{censor if value is equal or greater than lower_value & if}

\item{upper_value}{value is equal or lower than upper_value}

\item{granularity_time}{Which granularity_times to use this function on}

\item{granularity_time_not}{Which granularity_times to not use this function on}

\item{granularity_geo}{Which granularity_geos to use this function on}

\item{granularity_geo_not}{Which granularity_geos to not use this function on}
}
\description{
censor x-y function factory
}

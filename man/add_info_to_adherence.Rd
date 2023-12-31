% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_info_to_adherence.R
\name{add_info_to_adherence}
\alias{add_info_to_adherence}
\title{Add additional info of chronic medication consumers to your calculated adherence dataframe}
\usage{
add_info_to_adherence(connection, adherence_df, schema)
}
\arguments{
\item{connection}{Database connection based on individual connection details (ordinarily as conn-variable)}

\item{adherence_df}{calculated adherence dataframe using adherence function}

\item{schema}{OMOP CDM schema where info about the consumer is}
}
\value{
dataframe of medication adherence with included gender, age, year of birth, age group and medication's ATC code
}
\description{
Add additional info of chronic medication consumers to your calculated adherence dataframe
}
\examples{
add_info_to_adherence(list_of_calculated_adherence_dataframes$CMA4, 'ohdsi_cdm_202207')
}

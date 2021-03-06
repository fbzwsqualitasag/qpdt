% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_pedigree_parent.R
\name{check_pedig_parent}
\alias{check_pedig_parent}
\title{Check Properties of Parents in a Pedigree}
\usage{
check_pedig_parent(
  ps_pedig_path,
  ps_delim = "|",
  ps_id_col = "#IDTier",
  ps_sire_col = "IDVater",
  ps_dam_col = "IDMutter",
  ps_bd_col = "Birthdate",
  ps_sex_col = "Geschlecht",
  pcol_types = NULL,
  ptbl_pedigree = NULL,
  pn_bd_tol = 0,
  pl_wrong_sex = list(sire = "F", dam = "M")
)
}
\arguments{
\item{ps_pedig_path}{path to the pedigree input file}

\item{ps_delim}{column delimiting character}

\item{ps_id_col}{column title for animal IDs}

\item{ps_sire_col}{column title for sire IDs}

\item{ps_dam_col}{column title for dam IDs}

\item{ps_bd_col}{column title for birthdates}

\item{ps_sex_col}{column title for sex}

\item{pcol_types}{column types of pedigree in ps_pedig_path}

\item{ptbl_pedigree}{tibble containing pedigree information}

\item{pn_bd_tol}{minimal tolerance for age difference between animal and parents (in days)}

\item{pl_wrong_sex}{list with characters denoting the wrong sex}
}
\description{
Some descriptive statistics about the pedigree are collected. The main
check consists of the comparison of the birthdates of animals to the
birthdates of their parents. The check of birthdates can be parametrized
by a minimal tolerance of the difference using the argument \code{pn_bd_tol}.
The last check lists all animals that have the same IDs as one of their
parents.
}
\details{
The comparison of the birthdates is done via a join of the parent birthdates
to a tibble that consists of only animals, their birthdates and their parents.
The comparison is done for sires and dams in two separate steps.
}
\examples{
\dontrun{
check_pedig_parent(ps_pedig_path = system.file('extdata',
    'PopReport_SN_ohne_20210115.csv_adaptfin2.csv',
  package = 'qpdt'))
}

}

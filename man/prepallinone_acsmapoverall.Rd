% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocess_data.R
\name{prepallinone_acsmapoverall}
\alias{prepallinone_acsmapoverall}
\title{All in one preparation of files needed to generate ACS Map Overall}
\usage{
prepallinone_acsmapoverall(
  geogs = c("block group", "tract", "county", "place"),
  tidyyear = 2020,
  tidystate = "Maryland",
  basedir = "./data",
  downloadgeog = T,
  .geomyear = tidyyear
)
}
\arguments{
\item{geogs}{Set of geographies to read in and save data for; default c("block group", "tract", "place", "county")}

\item{tidyyear}{Last year of 5-year ACS data; default 2020 (for 2016-2020 data)}

\item{tidystate}{State to download data for; default "Maryland"}

\item{basedir}{The base-directory to save data and geometry files in; default "./data/". Creates directory if does not exist}

\item{downloadgeog}{Whether to download and save spatial geographies associated with data downloading; default F}

\item{.geomyear}{If downloadgeog is TRUE, the year of the geometry file to download; default tidyyear}
}
\value{
All files needed for gen_acsmap_overal() function
}
\description{
Reads in, saves, and processes all files needed to generate ACS map overall, following sequence of functions 1) acsmapping::loadtidydata_acsmapoverall() 2) acsmapping::readprocessdata_acsmapoverall() and 3) acsmapping::readsave_spatial_acsmapoverall().
}

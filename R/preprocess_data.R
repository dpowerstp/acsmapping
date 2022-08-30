

#' Pivot-wider acs-data
#'
#'
#' Function to pivot acs5 processed-data for joining to spatial file. Leaflet works better with wide than short-data, so pivot data so that totals are associated with each group in the data
#'
#' @param df Tidycensus-processed dataframe pivoting to join to spatial file. Data should have been processed with corresponding acsprocess package function for dataframe
#' @param namescol Column containing groups that are being pivoted wider (e.g., "agegrp", "incpov_ratio"), as string
#'
#' @return Pivoted dataframe with each group as column
#' @export
#'
#' @examples
pivotfunct <- function(df, namescol) {

  colspct <- grep("pct", colnames(df), value = T) %>%
    grep("(upper)|(lower)|(moe)", ., value = T, invert = T)

  df <- df %>%
    dplyr::rename(pct := !!dplyr::sym(colspct))

  print(colspct)

  totcol <- grep("(tot)|(tenure_overall)", colnames(df), value = T)

  print(totcol)

  df %>%
    tidyr::pivot_wider(
      id_cols = c("geoid", "name", totcol),
      names_from = namescol,
      values_from = c("estimate", pct),
      names_repair = "universal")

}

#' Recode percent-column to be NA if less than 10 total observations
#'
#' Recodes a percentage column to be NA if there are less than 10 observations in the denominator of the percentage column (the total column of the dataset)
#'
#' @param df ACS5 processed dataset
#' @param totcol Column with denominator of percentage column (total), as string
#' @param colname Name of percentage column, as string
#'
#' @return Dataframe with percentage column set to NA if less than 10 total observations
#' @export
#'
#' @examples
pctna_recode <- function(df, totcol, colname) {

  df %>%
    dplyr::mutate(
      {{colname}} := ifelse({{totcol}} < 10, NA, {{colname}})
    )
}


#' Generate shapefile for ACS map overall function
#'
#' Creates shapefile for operation with acsmapping::gen_acsmap_overall() function, joining procssed ACS datasets at the same geometry saved by acsmapping::processdfs_acsmapoverall() or readprocessdata_acsmapoverall() to an ACS shapefile for visualization by gen_acsmap_overall().
#'
#' @param geog Tidycensus geography, as a string (e.g., "block group", "tract", "place", "county")
#' @param tidyyear Last year of 5-year ACS data; default 2020 (for 2016-2020 data)
#' @param tidystate State data is for; default "Maryland"
#' @param basedir Base directory state geography file is in; default "./data"
#'
#' @return Saves shapefile with tidycensus data joined-to-it ready for visualization by acsmapping::gen_acsmap_overall()
#' @export
#'
#' @examples
readsave_spatial_acsmapoverall <- function(geog, tidyyear = 2020, tidystate = "Maryland", basedir = "./data"){

  if (is.null(tidystate)){

    geogpath <- glue::glue("{basedir}/{tidyyear}/{geog}/processed")

    geogfile <- glue::glue("{geog}_{tidyyear}.rds")

  }

  else {
    # stateabr <- state.abb[grep(tidystate, state.name)]

    stateabr <- usa::states %>%
      dplyr::filter(name == tidystate) %>%
      dplyr::pull(abb)


    geogpath <- glue::glue("{basedir}/{tidyyear}/{stateabr}/{geog}/processed")

    geogfile <- glue::glue("{geog}_{tidyyear}_{stateabr}.rds")


  }

  agedf <- readRDS(glue::glue("{geogpath}/process_vars_agesex.rds")) %>%
    acsmapping::pctna_recode(totcol = tot_people, colname = pct_age) %>%
    # rename(estimate = age_tot_est,
    #        moe = age_tot_moe) %>%
    acsmapping::pivotfunct( "agegrp")

  povdf <- readRDS(glue::glue("{geogpath}/process_vars_pov_ratio.rds")) %>%
    acsmapping::pctna_recode(totcol = pop_tot, colname = pct_incpov) %>%
    acsmapping::pivotfunct("incpov_ratio")

  vehicledf <- readRDS(glue::glue("{geogpath}/process_vars_vehicle.rds")) %>%
    # dplyr::rename(estimate = anyveh_est,
    #        moe = anyveh_moe) %>%
    acsmapping::pctna_recode(totcol = tothous, colname = pct_anyveh) %>%
    acsmapping::pivotfunct("anyvehicle")

  racedf <- readRDS(glue::glue("{geogpath}/process_vars_race.rds")) %>%
    dplyr::filter(!grepl("Not Hispanic", race_ethnicity)) %>%
    acsprocess::race_recode(race_col = race_ethnicity)%>%
    acsmapping::pctna_recode(totcol = pop_total, colname = pct_race) %>%
    acsmapping::pivotfunct("race")

  tenuredf <- readRDS(glue::glue("{geogpath}/process_vars_tenure.rds")) %>%
    acsmapping::pctna_recode(totcol = tenure_overall, colname = pct_tenure) %>%
    acsmapping::pivotfunct("tenure")

  popdf <- readRDS(glue::glue("{geogpath}/process_vars_pop.rds")) %>%
    dplyr::rename(estimate_Population = estimate)%>%
    dplyr::select(-c(label, concept, variable, moe, geography))

  geogfile <- read_rds(glue::glue("{basedir}/{geogfile}")) %>%
    dplyr::rename_all(tolower) %>%
    sf::st_transform(4326)

  # if (grepl("place", geog)){
  #   browser()
  # }

  joineddf <- geogfile %>%
    dplyr::left_join(povdf, by = "geoid") %>%
    dplyr::left_join(tenuredf, by = "geoid") %>%
    dplyr::left_join(vehicledf, by = "geoid") %>%
    dplyr::left_join(agedf, by = "geoid") %>%
    dplyr::left_join(popdf, by = "geoid") %>%
    dplyr::left_join(racedf, by = "geoid") %>%
    dplyr::select(-matches("(\\.x\\.x)|(\\.y)|(\\.z)$", ignore.case = T))

  saveRDS(joineddf, glue::glue("{geogpath}/acs_shp.rds"))

}

#' Read and Process Tidycensus Data for Multiple Geographies
#'
#' Reads in, processes, and saves unprocessed tidycensus dataframes that make up the ACS Map Overall, for a given set of geographies. Specifically, processes set of dataframes saved by acsmapping::loadtidydata_acsmapoverall() at the same geography in sequence of steps enabling datasets to be visualized by acsmapping::gen_acsmap_overall().
#'
#' @param geogs Tidycensus geographies, as a string vector. Default c("block group", "tract", "place", "county subdivision", "county")
#' @param tidyyear Last year of 5-year ACS data; default 2020 (for 2016-2020 data)
#' @param tidystate State to download data for; default "Maryland"
#' @param basedir The base-directory to data and geometry files are in; default "./data/"
#'
#' @return Saves set of processed-tidycensus dataframes at a given set of geographies as RDS files in standard directory structure
#' @export
#'
#' @examples
readprocessdata_acsmapoverall <- function(geogs = c("block group", "tract", "place", "county subdivision", "county"), tidyyear = 2020, tidystate = "Maryland", basedir = "./data"){

  # at different geometries - read in/save
  purrr::walk(geogs, ~ acsmapping::processdfs_acsmapoverall(.x, tidyyear = tidyyear, tidystate = tidystate, basedir = basedir))

}


#' Load and Save tidycensus Data for ACS Map Overall
#'
#' Reads in and saves 5-year tidycensus data to be visualized by acsmapping::gen_acsmap_overall, saving them in a common directory structure for processing by acsmapping::readprocessdata_acsmapoverall() or acsmapping::processdfs_acsmapoverall().
#'
#' Sequence of functions to prepare data for acsmapping::gen_acsmap_overall() is 1) acsmapping::loadtidydata_acsmapoverall() 2) acsmapping::readprocessdata_acsmapoverall() or acsmapping::processdfs_acsmapoverall() 3) acsmapping::readsave_spatial_acsmapoverall().
#'
#' @param tidyyear Last year of 5-year ACS data; default 2020 (for 2016-2020 data)
#' @param geogs Set of geographies to read in and save data for; default c("block group", "tract", "place", "county subdivision", "county")
#' @param tidystate State to download data for; default "Maryland"
#' @param downloadgeog Whether to download and save spatial geographies associated with data downloading; default F
#' @param .geomyear If downloadgeog is TRUE, the year of the geometry file to download; default tidyyear
#' @param basedir The base-directory to save data and geometry files in; default "./data/". Creates directory if does not exist
#'
#' @return Reads in and saves tidycensus downloaded ACS data at different geographies to standard file structure
#' @export
#'
#' @examples
loadtidydata_acsmapoverall <- function(geogs = c("block group", "tract", "place", "county subdivision", "county"), tidyyear = 2020, tidystate = "Maryland", downloadgeog = F, basedir= "./data", .geomyear = tidyyear ){

  # load tidycensus variables
  varscensus <- tidycensus::load_variables(year = tidyyear, "acs5", cache = T)

  # read
  vars_race <- varscensus %>%
    dplyr::filter(grepl("^B02001", name)) %>%
    dplyr::pull(name)

  vars_hisp <- varscensus %>%
    dplyr::filter(name %in% c("B03002_002", "B03002_012")) %>%
    dplyr::pull(name)

  vars_agesex <- varscensus %>%
    dplyr::filter(grepl("AGE", concept) & grepl("SEX", concept)) %>%
    dplyr::filter(concept == "SEX BY AGE") %>%
    dplyr::pull(name)

  vars_disab <- varscensus %>%
    dplyr::filter(grepl("DISABILITY", concept)) %>%
    dplyr::filter(geography == "block group")

  vars_disab <- varscensus %>%
    dplyr::filter(grepl("^B22010", name)) %>%
    dplyr::pull(name)

  vars_pov <- varscensus %>%
    dplyr::filter(grepl("(POVERTY)|(INCOME)", concept))

  vars_pov_bg <- vars_pov %>%
    dplyr::filter(geography == "block group") %>%
    dplyr::filter(grepl("POVERTY", concept))

  vars_pov_ratio <- vars_pov %>%
    dplyr::filter(concept == "RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS") %>%
    dplyr::pull(name)

  vars_vehicle <- varscensus %>%
    dplyr::filter(grepl("car", label))

  vars_vehicle <- varscensus %>%
    dplyr::filter(grepl("VEHICLE", concept)) %>%
    dplyr::filter(geography == "block group")

  vars_vehicle <- vars_vehicle %>%
    dplyr::filter(grepl("^B25044", name)) %>%
    dplyr::pull(name)

  vars_tenure <- varscensus %>%
    dplyr::filter(grepl("^B25003_", name)) %>%
    dplyr::pull(name)

  vars_pop <- varscensus %>%
    dplyr::filter(grepl("^B01003_", name)) %>%
    dplyr::pull(name)

  # create list of dfs to download
  readvars <- list(
    "vars_race" = c(vars_race, vars_hisp),
    "vars_agesex" = vars_agesex,
    "vars_disab" = vars_disab,
    "vars_pov_ratio" = vars_pov_ratio,
    "vars_vehicle" = vars_vehicle,
    'vars_pop' = vars_pop,
    "vars_tenure" = vars_tenure
  )

  if (downloadgeog){

    acsmapping::downloadgeogs(geogs = geogs, tidyyear = .geomyear, tidystate = tidystate, dirsave = basedir)
  }

  # save each at different geographies
  purrr::walk2(
    readvars,
    names(readvars), ~
      {
        purrr::walk(geogs, function(geog){

          acsprocess::acspull(.x, .y, geog = geog, state = tidystate, basedir = basedir)

        })
      })
}


#' Download shapefiles associated with geographies
#'
#' @param tidyyear Year of geography data to download; default 2020 (for 2016-2020 data)
#' @param geogs Set of geographies to read in and save data for; default c("block group", "tract", "place", "county", "county subdivision")
#' @param tidystate State to download data for; default "Maryland"
#' @param dirsave Directory to save geometries in
#'
#' @return Saved shapefiles for Census geographies
#' @export
#'
#' @examples
downloadgeogs <- function(geogs = c("block group", "tract", "place", "county", "county subdivision"), tidyyear = 2020, tidystate = "Maryland", dirsave = "./data"){

  options(tigris_use_cache = T)

  purrr::walk(geogs, ~{

    acsmapping::quickdircreate(dirsave)

    filename <- glue::glue("{.x}_{tidyyear}")

    if (.x == "state"){
      fips_pull <- NULL
    }

    else{

      statefilt <- tidycensus::fips_codes %>%
        dplyr::filter(state_name == tidystate)

      fips_pull <- statefilt %>%
        dplyr::pull(state_code) %>%
        unique()

      state_pull <- statefilt %>%
        dplyr::pull(state) %>%
        unique()


      filename <- paste(filename, state_pull, sep = "_")

    }

    filename <- paste0(filename, ".rds")

    tigrisfunct <- switch(
      .x,
      "block group" = tigris::block_groups,
      "tract" = tigris::tracts,
      "place" = tigris::places,
      "county" = tigris::counties,
      "state" = tigris::states,
      "county subdivision" = tigris::county_subdivisions,
    )

    if (is.null(tigrisfunct)){
      stop("Error; unrecognized geography in geogs parameter")
    }

    tigrisgeom <- tigrisfunct(
      state = fips_pull,
      year = tidyyear
    )

    saveRDS(tigrisgeom, paste(dirsave, filename, sep = "/"))
  })
}

#' Check if directory exists and create it if not
#'
#' Check if directory exists and create one if not.
#'
#' @param dirpath Path to directory
#'
#' @return
#' @export
#'
#' @examples
quickdircreate <- function(dirpath){

  if (!dir.exists(dirpath)){
    dir.create(dirpath)
  }

}


#' Process dataframes that build ACS Map Overall
#'
#' Reads in, processes, and saves unprocessed tidycensus dataframes that make up the ACS Map Overall. Specifically, processes set of dataframes saved by acsmapping::loadtidydata_acsmapoverall() at the same geography in sequence of steps enabling datasets to be visualized by acsmapping::gen_acsmap_overall().
#'
#' Sequence of functions to prepare data for acsmapping::gen_acsmap_overall() is 1) acsmapping::loadtidydata_acsmapoverall() 2) acsmapping::readprocessdata_acsmapoverall() or acsmapping::processdfs_acsmapoverall() 3) acsmapping::readsave_spatial_acsmapoverall().
#'
#' @param geog Tidycensus geography, as a string (e.g., "block group", "place", "county")
#' @param tidyyear Last year of 5-year ACS data; default 2020 (for 2016-2020 data)
#' @param tidystate State data assocaited with; default "Maryland"
#' @param basedir Base-directory geometry located in; default "./data"
#'
#' @return Saves set of processed-tidycensus dataframes at a given geography as RDS files in standard directory structure
#' @export
#'
#' @examples
processdfs_acsmapoverall <- function(geog, tidyyear = 2020, tidystate = "Maryland", basedir = "./data"){
  # browser()

  acsmapping::quickdircreate(basedir)
  acsmapping::quickdircreate(glue::glue("{basedir}/{tidyyear}"))

  if (is.null(tidystate)){

    dirfiles<- glue::glue("{basedir}/{tidyyear}/{geog}")
    dirprocess <- glue::glue("{dirfiles}/processed")

  }

  else{

    # stateabr <- state.abb[grep(tidystate, state.name)]

    stateabr <- usa::states %>%
      dplyr::filter(name == tidystate) %>%
      dplyr::pull(abb)

    acsmapping::quickdircreate(glue::glue("{basedir}/{tidyyear}/{stateabr}"))

    dirfiles<- glue::glue("{basedir}/{tidyyear}/{stateabr}/{geog}")

    dirprocess <- glue::glue("{dirfiles}/processed")

  }

  acsmapping::quickdircreate(dirfiles)
  acsmapping::quickdircreate(dirprocess)

  readRDS(glue::glue("{dirfiles}/vars_race.rds")) %>%
    acsprocess::process_race_ethn() %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_race.rds"))

  readRDS(glue::glue("{dirfiles}/vars_agesex.rds"))  %>%
    # acsprocess::location_col() %>%
    acsprocess::process_age_overall() %>%
    dplyr::mutate(
      agegrp = dplyr::case_when(
        grepl("(^Under)|(^[1-9] to)|(^1[0-7] )", age) ~ "Under 18",
        grepl("(^6[5-9] )|(^[7-9][0-9] )|(over$)", age) ~ "65 and over",
        T ~ "18-65"
      )
    ) %>%
    acsprocess::est_moe_derive(group_cols = c("name", "agegrp")) %>%
    dplyr::select(
      geoid, name, agegrp, tot_people, tot_people_moe, name_agegrp_est, name_agegrp_moe
    ) %>%
    dplyr::distinct() %>%
    dplyr::rename(estimate = name_agegrp_est,
                  moe = name_agegrp_moe) %>%
    acsprocess::derive_pct_est_moe("pct_age", "tot_people", "tot_people_moe") %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_agesex.rds"))

  readRDS(glue::glue("{dirfiles}/vars_disab.rds"))  %>%
    acsprocess::location_col() %>%
    acsprocess::process_disab_foodstamp(overall = T) %>%
    dplyr::distinct() %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_disab.rds"))

  readRDS(glue::glue("{dirfiles}/vars_pov_ratio.rds")) %>%
    acsprocess::process_poverty_detail() %>%
    acsprocess::incpov_recode(incpov_ratio) %>%
    dplyr::group_by(name) %>%
    dplyr::arrange(incpov_new) %>%
    dplyr::mutate(estimate = cumsum(estimate),
                  pct_incpov = estimate/ pop_tot) %>%
    dplyr::ungroup() %>%
    dplyr::filter(incpov_new %in% c(".50 to .99", "1.85 to 1.99")) %>%
    dplyr::mutate(incpov_ratio = dplyr::case_when(
      incpov_new == ".50 to .99" ~ "Under poverty line",
      incpov_new == "1.85 to 1.99"~ "Under 2X poverty line"
    )) %>%
    dplyr::mutate(
      dplyr::across(grep("^pct_", colnames(.), value = T),
             ~ round(.x * 100, 2))
    ) %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_pov_ratio.rds"))

  readRDS(glue::glue("{dirfiles}/vars_vehicle.rds")) %>%
    acsprocess::process_tenure_vehicleown(overall = T) %>%
    # dplyr::select(geoid, name, tothous, tothous_moe, anyvehicle, anyveh_est, anyveh_moe, pct_anyveh, pct_moe, pct_upper, pct_lower) %>%
    # dplyr::distinct() %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_vehicle.rds"))

  readRDS(glue::glue("{dirfiles}/vars_pop.rds")) %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_pop.rds"))

  read_rds(glue::glue("{dirfiles}/vars_tenure.rds")) %>%
    acsprocess::process_tenure_df() %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_tenure.rds"))

}


#' All in one preparation of files needed to generate ACS Map Overall
#'
#' Reads in, saves, and processes all files needed to generate ACS map overall, following sequence of functions 1) acsmapping::loadtidydata_acsmapoverall() 2) acsmapping::readprocessdata_acsmapoverall() and 3) acsmapping::readsave_spatial_acsmapoverall().
#'
#' @param tidyyear Last year of 5-year ACS data; default 2020 (for 2016-2020 data)
#' @param geogs Set of geographies to read in and save data for; default c("block group", "tract", "county", "county subdivision", "place")
#' @param tidystate State to download data for; default "Maryland"
#' @param downloadgeog Whether to download and save spatial geographies associated with data downloading; default F
#' @param .geomyear If downloadgeog is TRUE, the year of the geometry file to download; default tidyyear
#' @param basedir The base-directory to save data and geometry files in; default "./data/". Creates directory if does not exist
#'
#' @return All files needed for gen_acsmap_overal() function
#' @export
#'
#' @examples
prepallinone_acsmapoverall <- function(geogs = c("block group", "tract", "county", "county subdivision", "place"), tidyyear = 2020, tidystate = "Maryland", basedir = "./data", downloadgeog = T, .geomyear = tidyyear){

  acsmapping::loadtidydata_acsmapoverall(
    geogs = geogs,
    tidyyear = tidyyear,
    tidystate = tidystate,
    downloadgeog = downloadgeog, basedir = basedir,
    .geomyear = .geomyear)

  acsmapping::readprocessdata_acsmapoverall(geogs = geogs, tidyyear = tidyyear, basedir = basedir, tidystate = tidystate)

  purrr::walk(geogs,
    ~ acsmapping::readsave_spatial_acsmapoverall(geog = .x, tidyyear = tidyyear, basedir = basedir, tidystate = tidystate)
  )

}

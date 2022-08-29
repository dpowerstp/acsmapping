#' Add polygons and legend for ACS variable
#'
#' Standard function for visualizng acs variables in web-maps
#'
#' @param leafletobj Leaflet obj is base leaflet map adding layer to
#' @param colvisualize String name of column visualizing
#' @param labtext Text to supply to leafletwrappers::label_output() function to generate labels for dataset
#' @param acsdata spatial data visualizing in this polygon. Should be a spatial dataset with columns for data of interest. Default dataset fed to map object
#' @param totcol total column/universe of column visualizing (e.g., tot_population, tot_households)
#' @param colors Color scheme to shade polygons by
#' @param title Title of layer/legend
#' @param sizelabs Size of labels; default "11px"
#'
#' @return Leaflet object with polygons shaded/added
#' @export
#'
#' @examples
addpoly_acs <- function(leafletobj, colvisualize, labtext, acsdata = leaflet::getMapData(leafletobj), totcol = estimate_Population, colors = "Blues",  title = NULL, sizelabs = "11px"){

  # browser()

  # create labels for dataframe
  p <- "<p></p>"

  labs <- leafletwrappers::label_output(
    sf::st_drop_geometry(acsdata),
    label_text = labtext
  )

  # color polygons based on supplied color set
  palnumeric <- leaflet::colorNumeric(domain = range(acsdata[[colvisualize]], na.rm = T), na.color = "#e3e3e3", palette = colors)

  # create grp from name of column
  grp <- gsub("(estimate_)|(pct_)", "", colvisualize) %>%
    gsub("\\.", " ", .)

  if (is.null(title)){
    # standard title based on column name unless provided
    title <- dplyr::case_when(
      grepl("estimate", colvisualize) ~ paste0("Total ", tolower(grp)),
      T ~ paste0("Percent ", tolower(grp))
    )
  }

  leafletobj %>%
    leafletwrappers::addpoly_legend(
      df_select = sf::st_drop_geometry(acsdata),
      pal_funct_select = palnumeric,
      variable_select = colvisualize,
      group_select = grp,
      labels_select = labs,
      title_select = title,
      .data = acsdata,
      .label_textsize = sizelabs
    )

}

#' Add polygons for ACS5 race data
#'
#' Adds polygons for ACS5 race data to leaflet map.
#'
#' @param leafletobj leaflet object adding polygons to
#' @param colvisualize Column visualizing in polygons (e.g., Hispanic_estimate)
#' @param acsdata spatial dataset column is in; default to pull data from map object
#' @param sizelabs Size of labels; default 11
#'
#' @return Leaflet map with polygons added
#' @export
#'
#' @examples
addpoly_race <- function(leafletobj, colvisualize, acsdata = leaflet::getMapData(leafletobj), sizelabs = "12px") {

  # p <- "<p></p>"

  # identify appropriate labeling columns
  est_col <- dplyr::case_when(
    grepl("estimate", colvisualize) ~ colvisualize,
    T ~ gsub("pct_", "estimate_", colvisualize)
  )
  pct_col <- dplyr::case_when(
    grepl("pct_", colvisualize) ~ colvisualize,
    T ~ gsub("estimate_", "pct_", colvisualize)
  )

  labtext <- paste0("Name: {name.x}<p></p>Population: {estimate_Population %>% tpfuncts::commafy()}<p></p>Race total: {", est_col, " %>% tpfuncts::commafy()}<p></p>Percent race: {", pct_col, " %>% round(0)}%")

  leafletobj %>%
    acsmapping::addpoly_acs(acsdata = acsdata, colvisualize = colvisualize, labtext = labtext, totcol = estimate_Population, colors = "Blues", sizelabs = sizelabs)

}

#' Quick label for ACS5 data
#'
#' Function to generate label text for ACS5 spatial data
#'
#' @param colsuffix String-suffix of column visualizing in data. E.g., estimate_Under.poverty.line. Assumes common format of estimate and percentage columns with estimate_{colsuffix} and pct_{colsuffix}
#' @param tot Name of total column for universe of variable as string (e.g., if total renter-households, column for total households). Default "estimate_Population"
#'
#' @return Text to be supplied to a leafletwrappers label function for a spatial ACS5 dataframe
#' @export
#'
#' @examples
quicklab <- function(colsuffix, tot = "estimate_Population"){

  paste0("Place: {name.x}<p></p>Population: {", tot, " %>% tpfuncts::commafy()}<p></p>", gsub("\\.", " ", colsuffix), " total: {estimate_", colsuffix, " %>% tpfuncts::commafy()}<p></p>Percent ", gsub("\\.", " ", stringr::str_to_lower(colsuffix)), ": {pct_", colsuffix, " %>% round(0)}%")

}


#' Generate map of key ACS variables at a given geography
#'
#' Generates leaflet map of key ACS variables--including population by race/ethnicity, poverty status, and renter occupancy--for processed ACS dataframe.
#'
#' @param baseacsdata Spatial dataframe for a Census geometry pre-processed by acsmapping::prepallinone_acsmapoverall() sequence of functions
#' @param pct_est Percent or estimate prefix as string; default "pct_"
#' @param inccountycontrols Whether to include overlay layers for county and place boundaries; default TRUE
#' @param ... Additional arguments to leafletwrappers::layercontrolsquick
#' @param sizelabs Label text size; default "12px"
#'
#' @return A leaflet map with key ACS variables visualized
#' @export
#'
#' @examples
gen_acsmap_overall <- function(baseacsdata, pct_est = "pct_", inccountycontrols = T, sizelabs = "12px", ...){

  # groups to cycle through
  grps_race <- c(
    "White",
    "Black",
    "Hispanic",
    "Asian",
    "AIAN",
    "NHPI",
    "Other",
    "Multiracial"
  )

  grps <- c(
    "Population",
    grps_race,
    "Under poverty line",
    "Under 2X poverty line",
    "Renter occupied",
    "No vehicle",
    "Under 18",
    "65 and over"
  )

  # initialize leaflet map
  leafobj <- leaflet::leaflet(baseacsdata) %>%
    leaflet::addProviderTiles(providers$CartoDB)

  # add race layers to data
  purrr::walk(grps_race, ~{

    pct_col <- paste0(pct_est, "_", .x)

    leafobj <<- leafobj %>%
      acsmapping::addpoly_race(pct_col,
                               sizelabs = sizelabs)
  })

  leafobj <- leafobj %>%
    acsmapping::addpoly_acs(
      colvisualize = "estimate_Population",
      labtext = "Name: {name.x}<p></p>Total population: {estimate_Population}",
      sizelabs = sizelabs) %>%
    acsmapping::addpoly_acs(
      colvisualize = glue::glue("{pct_est}_Under.poverty.line"),
      labtext = acsmapping::quicklab("Under.poverty.line"),
      colors = "Oranges",
      sizelabs = sizelabs) %>%
    acsmapping::addpoly_acs(
      colvisualize = glue::glue("{pct_est}_Under.2X.poverty.line"),
      labtext = acsmapping::quicklab("Under.2X.poverty.line"),
      colors = "Oranges",
      sizelabs = sizelabs) %>%
    acsmapping::addpoly_acs(
      colvisualize = glue::glue("{pct_est}_Renter.occupied"),
      labtext = acsmapping::quicklab("Renter.occupied", tot = "tothous"),
      colors = "YlGn",
      totcol = "tothous",
      sizelabs = sizelabs) %>%
    acsmapping::addpoly_acs(
      colvisualize = glue::glue("{pct_est}_No.vehicle"),
      labtext = acsmapping::quicklab("No.vehicle", tot = "tothous"),
      colors = "PuRd",
      totcol = "tothous",
      sizelabs = sizelabs) %>%
    acsmapping::addpoly_acs(
      colvisualize = glue::glue("{pct_est}_Under.18"),
      labtext = acsmapping::quicklab("No.vehicle"),
      colors = "PuRd",
      sizelabs = sizelabs) %>%
    acsmapping::addpoly_acs(
      colvisualize = glue::glue("{pct_est}_65.and.over"),
      labtext = acsmapping::quicklab("65.and.over"),
      colors = "PuRd",
      sizelabs = sizelabs)

  if (inccountycontrols){
    grps <- c(grps, "Place boundaries", "County boundaries")

    leafobj <- leafobj %>%
      leafletwrappers::addbounds(placetype = "Place boundaries", spatialdf = acsshp_place) %>%
      leafletwrappers::addbounds(placetype = "County boundaries", spatialdf = acsshp_county)

  }

  # create object in global environment for grps, so can modify
  leaflet_grps <<- grps


  leafobj %>%
    leafletwrappers::layercontrolsquick(
      groups = grps,
      hide = grps[-1],
      ...
    )

}



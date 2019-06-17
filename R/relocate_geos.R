#' Moves specified geographies for aesthetic purposes
#'
#' `relocate_geos()` returns sf object. relocates geographies for maps that are different than `states` and `counties`
#'
#'
#' @param df `sp` or `sf` object
#' @param fips numeric or character vector; column name containing state fips designations
#' @param move selection of states to move; current options are `"alaska"`, `"hawaii"`, `"guam"`, `"rico"`, `"samoa"`, `"virgin"`, and `"mariana"`
#'
#' @md
#' @export
relocate_geos <- function(df, fips, move = c("alaska", "hawaii"), coords = NULL, crs = NULL) {

  #checks
  df_class<-class(df)
  df_boolean <- is(df, "SpatialPointsDataFrame")|is(df, "SpatialPolygonsDataFrame")|is(df, "sf")|is_tibble(df)

  if(!(df_boolean)){
    stop("invalid `df` argument. `df` must be an `sp`, `sf`, or `tibble()` dataframe.", call. = F)
  }
  if(!(is.character(fips)&length(fips)==1)){
    stop("invalid 'fips' argument. `fips`` must be a character value of length 1.",
         call. = Fl
    )
  }

  if(!fips%in%names(df)){
    stop("invalid 'fips' argument. fips must be a column name of dataframe df.",
         call. = F
        )
  }

  if(any(!(move %in% c("alaska", "hawaii", "guam", "rico", "samoa","virgin","mariana")))){
    stop("invalid `move` argument. Valid `move` values are: ",
         "alaska, hawaii, guam, rico, samoa, virgin, and mariana.",
         call.=F)
  }

  if(is(df, "sf")){
    df <- as(df, 'Spatial')
  }

  if(is(df, "tbl")) {

    if(!class(crs)=="CRS"){
      stop("invalid crs argument. when df is a tibble(), crs must be equal to a CRS object.", call. = F)
    }

    pts <- df[, coords]
    df <- SpatialPointsDataFrame(coords = pts, data = df,
                                 proj4string = crs)
  }

  if(is.null(proj4string(df))){
    warning("no projection found: projecting data using espg: 2163",  call. = F)
    proj4string(df) <- CRS("+init=epsg:2163")
  } else {
    old_proj <- proj4string(df)
    df <-spTransform(df, CRS("+init=epsg:2163"))
  }




  if ("alaska" %in% move) {

    alaska <- df[df[[fips]] == "02", ] %>%
      transform_state(-35, 2, c(-2600000, -2300000))

    proj4string(alaska) <- proj4string(df)


   df  <- df[df[[fips]]!="02", ] %>% rbind(alaska)

  }

  if ("hawaii" %in% move) {

    hawaii <- df[df[[fips]] == "15", ] %>%
      transform_state(-35, 0.8, c(-1170000, -2363000))

    proj4string(hawaii) <- proj4string(df)

    df  <- df[df[[fips]]!="15", ] %>% rbind(hawaii)

  }

  if ("guam" %in% move) {


  }

  if("rico" %in% move) {

  }

  if("samoa" %in% move) {

  }

  if("virgin" %in% move) {

  }

  if("mariana" %in% move) {

  }


 df %>% spTransform(CRS("+init=epsg:4326")) %>% st_as_sf()

}

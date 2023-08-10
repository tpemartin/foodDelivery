#' Get weather data from all Taiwan weather observation stations
#'
#' @param date a string of "YYYY-MM-DD" format for day weather, "YYYY-MM" for month weather, "YYYY" for year weather.
#'
#' @return a list of weather data from all observation stations. Check weather_stations data frame for more details.
#' @export
get_weather_data <- function(date) {
  weather_stations_list$byID |>
    purrr::map(
      purrr::safely(~{
        .x |>
          get_weather_from_one_station_from_weather_stations_list(date) |>
          extract_valid_weather_dataFrame_from_apiResult() -> dfX
      })
    ) -> list_weather_dfs
  list_weather_dfs
}
# helpers ----
get_weather_from_station <- function(date="2023-07-01", stationID=466910, stationName="鞍部", stationAltitude="837.6m"){
  # date="2023-07-01"; stationID=466910; stationName="鞍部"; stationAltitude="837.6m"
  stationName|> URLencode() |> URLencode(repeated = T) -> stationName_encode

  date |> stringr::str_split("-") -> split_date
  switch (as.character(length(split_date[[1]])),
    "3" = {
      controlText = "DayDataController"
    },
    "2" = {
      controlText = "MonthDataController"
    },
    "1" = {
      controlText = "YearDataController"
    }
  )
  url <- glue::glue(
    "https://e-service.cwb.gov.tw/HistoryDataQuery/{controlText}.do?command=viewMain&station={stationID}&stname={stationName_encode}&datepicker={date}&altitude={stationAltitude}")
  httr::GET(url) -> response
  response |>
    httr::content(type="text/html", encoding = "utf-8") -> .html
  .html |>
    rvest::html_table() -> tb
  return(tb)
}
get_weather_from_one_station_from_weather_stations_list <- function(
    weather_stationX, date="2023-07-01"){
  get_weather_from_station(
    date,
    stationID = weather_stationX$stationID,
    stationName = weather_stationX$stationName,
    stationAltitude = paste0(weather_stationX$stationAltitude,"m")
  )
}

extract_valid_weather_dataFrame_from_apiResult <- function(results) {
  df <- results[[2]]
  df[2, ] |> as.character() -> valid_colnames

  names(df) <- valid_colnames
  df[-c(1:2), ] |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = as.numeric)
    ) -> df_valid
  df_valid
}
prepare_valid_weather_station_list <- function() {
  names(weather_stations) <- c(
    "stationID", "stationName", "站種", "stationAltitude", "lon",
    "lat", "城市", "地址", "資料起始日期", "撤站日期",
    "備註", "原站號", "新站號"
  )

  weather_stations |>
    econDV2::augment_county_township_using_lon_lat() -> weather_stations

  weather_stations |>
    dplyr::select(
      stationID, stationName, stationAltitude, lon, lat
    ) -> weather_stations2

  weather_stations_by_stationID <- weather_stations2 |>
    split(weather_stations2$stationID)
  weather_stations_by_stationName <- weather_stations2 |>
    split(weather_stations2$stationName)


  weatherOnDate <- purrr::map(
    weather_stations_by_stationID,
    purrr::safely(~get_weather_from_station(
      stationID = .x$stationID,
      stationName = .x$stationName,
      stationAltitude = paste0(.x$stationAltitude,"m")
    ))
  ) |> setNames(weather_stations_by_stationID)

  # which error is not null
  pick_weatherOnDate_errorNULL <-
    seq_along(weatherOnDate) |> purrr::map_lgl(~{is.null(weatherOnDate[[.x]]$error)})
  weather_stations_by_stationID_validated = weather_stations_by_stationID[pick_weatherOnDate_errorNULL]

  weather_stations_by_stationID_validated |>
    purrr::map_chr(~{.x$stationName}) -> valid_stationNames

  weather_stations_by_stationName_validated = weather_stations_by_stationName[valid_stationNames]

  weatherOnDate_validated = weatherOnDate[pick_weatherOnDate_errorNULL]
  weatherOnDate_validated2 <-
    purrr::keep(
      weatherOnDate_validated,
      ~{.x$result[[2]] |> nrow() -> nrowX
        nrowX > 2})

  whichStationHasData <-
    names(weather_stations_by_stationID_validated) %in% names(weatherOnDate_validated2)
  weather_stations_by_stationID_validated2 <-
    weather_stations_by_stationID_validated[whichStationHasData]

  weather_stations_by_stationID_validated2 |>
    purrr::map_chr(~{.x$stationName}) -> valid_stationNames2

  weather_stations_by_stationName_validated2 <-
    weather_stations_by_stationName_validated[valid_stationNames2]

  weather_stations_list = list(
    byID = weather_stations_by_stationID_validated2,
    byName = weather_stations_by_stationName_validated2
  )

  usethis::use_data(weather_stations, weather_stations_list,
                    overwrite = T)
}

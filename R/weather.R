get_weather_from_station <- function(date="2023-07-01", stationID=466910, stationName="鞍部", stationAltitude="837.6m"){
  # date="2023-07-01"; stationID=466910; stationName="鞍部"; stationAltitude="837.6m"
  stationName|> URLencode() |> URLencode(repeated = T) -> stationName_encode
  url <- glue::glue(
    "https://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station={stationID}&stname={stationName_encode}&datepicker={date}&altitude={stationAltitude}")
  httr::GET(url) -> response
  response |>
    httr::content(type="text/html", encoding = "utf-8") -> .html
  .html |>
    rvest::html_table() -> tb
  return(tb)
}


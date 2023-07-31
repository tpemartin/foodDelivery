augment_county_township <- function(panel_menu) {
  panel_menu$address |>
    stringr::str_extract("[\u4E00-\u9FFF]+") -> addresses
  addresses |>
    stringr::str_replace_all(c('台'="臺", "巿"="市", "湾"="灣")) |>
    stringr::str_remove_all("^臺灣省?") -> addresses_corrected

  fct_county <- extract_fct_county(addresses_corrected)

  whereCountyMissing <- which(is.na(fct_county))

  addresses_corrected[whereCountyMissing] |>
    stringr::str_extract("^[^縣市]{2}(?=[縣市])") |>
    paste0("縣") -> toBeAdded
  toBeAdded[which(toBeAdded=="NA縣")] <- ""
  toBeAdded

  addresses_corrected[whereCountyMissing] <- {
    paste0(toBeAdded, addresses_corrected[whereCountyMissing])
  }

  addresses_corrected_without_county <-
    stringr::str_remove_all(
      addresses_corrected,
      officialCountiesRegex)

  fct_county <- extract_fct_county(addresses_corrected)

  panel_menu$county <- fct_county

  addresses_corrected_without_county |>
    stringr::str_extract("^[^鄉鎮市區]{1,2}[鄉鎮市區]") -> township

  panel_menu$township <- township
  panel_menu
}

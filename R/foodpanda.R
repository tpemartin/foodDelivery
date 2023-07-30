#' Initiate a Foodpanda Data instance for preliminary foodpada data import and analysis
#'
#' @param shopListFolderUrl a character vector
#' @param shopMenuFolderUrl a character vector
#'
#' @return an R6 Data Foodpanda class object
#' @export
FoodPanda <- function(
    shopListFolderUrl = "https://drive.google.com/drive/folders/1gbUGUggYxvUBzyMyW1et4UZLQ4UguUCk",
    shopMenuFolderUrl = "https://drive.google.com/drive/folders/14Yk8kBRcWrULfI5vqI3508eyJNMH9RtM"){
  return(DataFoodPanda$new(
    shopListFolderUrl = shopListFolderUrl,
    shopMenuFolderUrl = shopMenuFolderUrl
  ))
}

DataFoodPanda <-
  R6::R6Class(
    "Data Foodpanda",
    public = list(
      source = list(
        shopListFolderUrl = character(0),
        shopMenuFolderUrl = character(0)
      ),
      available_dates = character(0),
      initialize = function(shopListFolderUrl = "https://drive.google.com/drive/folders/1gbUGUggYxvUBzyMyW1et4UZLQ4UguUCk",
                            shopMenuFolderUrl = "https://drive.google.com/drive/folders/14Yk8kBRcWrULfI5vqI3508eyJNMH9RtM") {
        self$source$shopListFolderUrl <- shopListFolderUrl
        self$source$shopMenuFolderUrl <- shopMenuFolderUrl
        private$get_shopListDribbles()
        private$get_shopMenuDribbles()
        private$get_availableDates()
      },
      retrieve_data = function(dates, need2download = T) {
        retrieveData(dates, shopListDribbles = private$shopListDribbles, shopMenuDribbles = private$shopMenuDribbles, need2download = need2download)
      },
      detect_feature_changes = function(df, df2, removeFeatures){
        compare_feature_changes(df, df2, removeFeatures)
      }
    ),
    private = list(
      shopListDribbles = list(),
      shopMenuDribbles = list(),
      get_shopListDribbles = function() {
        private$shopListDribbles <- get_shopListDribbles(
          shopListFolderUrl =
            self$source$shopListFolderUrl
        )
      },
      get_shopMenuDribbles = function() {
        private$shopMenuDribbles <- get_shopMenuDribbles(
          shopMenuFolderUrl =
            self$source$shopMenuFolderUrl
        )
      },
      get_availableDates = function() {
        self$available_dates <- get_availableDates(
          shopListDribbles = private$shopListDribbles,
          shopMenuDribbles = private$shopMenuDribbles
        )
      }
    )
  )

extract_fct_county <- function( addresses_corrected) {
  addresses_corrected |>
    stringr::str_extract(
      glue::glue("^{officialCountiesRegex}")) -> county

  fct_county = factor(county)
  fct_county
}
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
augment_lat_lon <- function(panel_menu) {
  panel_menu$location  |>
    stringr::str_extract_all("[0-9]+\\.[0-9]+", T)  -> latLong
  latLong[,1] |> is.na() |> which() -> whereHasMissingLatLon
  latLong[whereHasMissingLatLon, ] <- ""

  panel_menu$lat <- as.numeric(latLong[,1])
  panel_menu$lon <- as.numeric(latLong[,2])
  panel_menu
}
get_shopListDribbles <- function(shopListFolderUrl) {
  shopListFolderUrl |>
    googledrive::drive_get() -> dribble
  dribble |>
    googledrive::drive_ls() |>
    dplyr::arrange(name) |>
    dplyr::filter(
      stringr::str_detect(name, "^[0-9]{4}\\-[0-9]+\\-[0-9]+$")
    ) -> listDribbles
}
get_shopMenuDribbles <- function(shopMenuFolderUrl) {
  shopMenuFolderUrl |>
    googledrive::drive_ls() -> shopMenuDribbles
  shopMenuDribbles
}
get_availableDates <- function(shopListDribbles, shopMenuDribbles) {
  shopListDribbles$name -> shopListDates
  shopMenuDribbles$name |> stringr::str_extract("[0-9]{4}\\-[0-9]+\\-[0-9]+") -> shopMenuDates
  intersect(shopListDates, shopMenuDates)
}


get_all_most_shopListDribble <- function(shopListDribbles) {
  seq_along(shopListDribbles$name) |>
    purrr::map_dfr(
      ~ {
        shopListDribbles[.x, ] |>
          googledrive::drive_ls(pattern = "^all_most", type = "text/csv")
      }
    )
}

retrieveData <- function(selectedFileDates, shopListDribbles, shopMenuDribbles, need2download = T) {
  pj <- rprojroot::find_rstudio_root_file() # rstudioapi::getActiveProject()
  if(!dir.exists(file.path(pj, "local-data"))) dir.create(file.path(pj, "local-data"))
  pickedDribbles <- # pick dribbles based on selected dates
    {
      pickDribbles <-
        (shopListDribbles$name %in% selectedFileDates)

      dribblePicked <- shopListDribbles[pickDribbles, ]

      dribblePicked |>
        dplyr::arrange(name)
    }

  allMostShopListDribbles <- # obtain dribble of all_most_....csv
    {
      seq_along(pickedDribbles$name) |>
        purrr::map_dfr(
          ~ {
            pickedDribbles[.x, ] |>
              googledrive::drive_ls(pattern = "^all_most", type = "text/csv")
          }
        )
    }

  listShopListDownloaded <- {
    seq_along(allMostShopListDribbles$name) |>
      purrr::map(
        ~ {
          .file <- file.path(pj, "local-data", allMostShopListDribbles$name[[.x]])
          googledrive::drive_download(allMostShopListDribbles[.x, ], path = .file, overwrite = T)
        }
      )
  }

  panel_shopList <- construct_panel_from_dribbles(allMostShopListDribbles, pj,
    download = need2download,
    useFilenameToCreateDateColumn = T
  )

  panel_shopMenus <- # obtain corresponding shop menus
    {
      selectedShopMenuDribbles <- # filter selected menu dribbles
        {
          shopMenuDribbles |>
            dplyr::filter(stringr::str_extract(name, "[0-9]{4}\\-[0-9]+\\-[0-9]+") %in% selectedFileDates) |>
            dplyr::arrange(name)
        }

      panel_menu <- construct_panel_from_dribbles(selectedShopMenuDribbles, pj, download = need2download)

      aug_panel_menu <- # augment date, lon, lat, county, township
        {
          # augment lat lon
          panel_menu <- augment_lat_lon(panel_menu)

          # augment date
          panel_menu$updateDate <- lubridate::ymd_hms(panel_menu$updateDate)
          panel_menu$date <- lubridate::date(panel_menu$updateDate)

          panel_menu <- econDV2::augment_county_township_using_lon_lat(panel_menu) #augment_county_township(panel_menu)
        }
      aug_panel_menu
    }

  panel_shopMenus |>
    dplyr::full_join(
      panel_shopList,
      by = c("shopCode", "date")
    )

  panel_shopMenus |>
    dplyr::select(
      -tidyselect::matches("^\\.\\.\\."),
      -tidyselect::contains("date", ignore.case=T),
      -location
    ) |>
    dplyr::arrange(shopCode)
}
save_as_json <- function(df) {
  df$shopCode |> stringr::str_sub(1,1) -> shopCode1stLetter
  fct_shopCode1stLetter <- factor(shopCode1stLetter)
  levels(fct_shopCode1stLetter) |>
    stringr::str_detect("[a-f]") -> pickGroup1
  levels(fct_shopCode1stLetter) |>
    stringr::str_detect("[g-p]") -> pickGroup2
  pickGroup3 <- ! (pickGroup1 | pickGroup2)

  levels(fct_shopCode1stLetter) -> levels_codeLetter
  levels_codeLetter[pickGroup1] <- "g1"
  levels_codeLetter[pickGroup2] <- "g2"
  levels_codeLetter[pickGroup3] <- "g3"
  levels(fct_shopCode1stLetter) <-  levels_codeLetter

  df |>
    split(fct_shopCode1stLetter) -> split_df

  seq_along(split_df) |>
    purrr::walk(
      ~{
        df = split_df[[.x]]
        df |> convert_to_json() -> json
        json |>
          xfun::write_utf8(
            glue::glue("/Users/martin/Documents/GitHub/foodpanda-ubereats-data/foodpanda{.x}.json"))
      }
    )
}
convert_to_json <- function(df) {
  df |>
    # purrr::transpose() |>
    jsonlite::toJSON(dataframe = "rows", auto_unbox = T, pretty = T) -> json
  json
}
compare_feature_changes <- function(df, df2, removeFeatures=c("rate") ){
  df |>
    dplyr::select(-tidyselect::any_of(removeFeatures)) -> df
  df2 |>
    dplyr::select(-tidyselect::any_of(removeFeatures)) -> df2
  df |>
    purrr::transpose() -> df_as_list
  df_shopCodes <- df$shopCode
  df2 |>
    purrr::transpose() -> df_as_list2
  df_shopCodes2 <- df2$shopCode

  # new shops
  newShops <- setdiff(df_shopCodes2, df_shopCodes)
  # exit shops
  exitShops <- setdiff(df_shopCodes, df_shopCodes2)
  # common shops
  commonShops <- intersect(df_shopCodes, df_shopCodes2)
  ## common shops difference
  commonShops_record_changes <-
    purrr::map(
      seq_along(commonShops),
      ~ {
        commonShopX <- commonShops[[.x]]
        whereIsCommonShopXIn_df_as_list <- which(df_shopCodes == commonShopX)
        whereIsCommonShopXIn_df_as_list2 <- which(df_shopCodes2 == commonShopX)

        target1 <- df_as_list[[whereIsCommonShopXIn_df_as_list]]
        target2 <- df_as_list2[[whereIsCommonShopXIn_df_as_list2]]
        seq_along(target1) |>
          purrr::map_lgl(
            ~ {
              !identical(target1[[.x]], target2[[.x]])
            }
          ) -> pickChanges

        names(target2)[pickChanges]
      }
    ) |> setNames(commonShops)

  # summarise number of features changed
  commonShops_record_changes |>
    purrr::map_int(length) |>
    table() -> how_many_features_had_changed

  # summarise frequency of each feature change
  commonShops_record_changes |>
    unlist() |>
    table() -> each_feature_change_has_how_many_shops

  list(
    newShops = newShops,
    exitShops = exitShops,
    commonShops = commonShops,
    commonShops_changes = list(
      change_in_each_shop = commonShops_record_changes,
      how_many_features_had_changed = how_many_features_had_changed,
      each_feature_change_has_how_many_shops = each_feature_change_has_how_many_shops
    )
  )
}
construct_panel_from_dribbles <- function(selectedShopMenuDribbles, pj, download=T, useFilenameToCreateDateColumn=F) {
  listMenuDownload <- constuct_listDownload_from_listDribble(selectedShopMenuDribbles, pj, download)
  if(useFilenameToCreateDateColumn){
    panel_menu <- construct_panel_data_frame(listMenuDownload, selectedShopMenuDribbles)
    return(panel_menu)
  }
  panel_menu <- construct_panel_data_frame(listMenuDownload)
  panel_menu
}
constuct_listDownload_from_listDribble <- function(selectedShopMenuDribbles, pj, download=T) {
  if (download) {
    listMenuDownload <- # download menu csv
      {
        seq_along(selectedShopMenuDribbles$name) |>
          purrr::map(
            ~ {
              .file <- file.path(pj, "local-data", selectedShopMenuDribbles$name[[.x]])
              googledrive::drive_download(selectedShopMenuDribbles[.x, ], path = .file, overwrite = T)
            }
          )
      }
  } else {
    listMenuDownload <-
      seq_along(selectedShopMenuDribbles$name) |>
      purrr::map(
        ~ {
          .file <- file.path(pj, "local-data", selectedShopMenuDribbles$name[[.x]])
          list(local_path = .file)
        }
      )
  }
  listMenuDownload
}
construct_panel_data_frame <- function(listDownloaded, listDribble=NULL) {
  listDfShopMenus <- # read shop menus
    {
      listDownloaded |>
        purrr::map(
          ~{
            .x$local_path |>
              readr::read_csv()
          })
    }

  commonColumnNames <- # obtain column names shared across df
    {
      listDfShopMenus |>
        purrr::reduce(
          function(acc, x) intersect(acc, names(x)),
          .init = names(listDfShopMenus[[1]])
        )
    }

  panel_menu <- # construct panel data frame
    {
      seq_along(listDfShopMenus) |>
        purrr::map_dfr(
          ~{
            df = listDfShopMenus[[.x]][,commonColumnNames]
            df$fileNumber = paste0("file",.x)
            df
          }
        )
    }

  if(!is.null(listDribble)){
    panel_menu <- add_dateColumn_byFilename(panel_menu, listDribble)
  }
  panel_menu$fileNumber <- NULL
  panel_menu
}
add_dateColumn_byFilename <- function(panel_shopList, allMostShopListDribbles) {
  panel_shopList$date <- factor(panel_shopList$fileNumber)
  levels(panel_shopList$date) <- {
    allMostShopListDribbles$name |>
      stringr::str_extract("[0-9]{4}\\-[0-9]+\\-[0-9]+") ->
      fileDates
    fileDates
  }
  panel_shopList$date <- lubridate::ymd(
    as.character(panel_shopList$date)
  )
  panel_shopList
}

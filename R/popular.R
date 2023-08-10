#' Get popular items from menu json
#'
#' @param menu a list from jsonlite::fromJSON()
#'
#' @return a character vector of popular items
#' @export
get_popular_items_from_menuJson <- function(menu) {
  menu |>
    get_popular_id_from_menuJson() -> popularId
  menu |>
    get_id_name_mapping_from_menuJson() -> menuMapping

  menuMapping$name |>
    subset(
      menuMapping$id %in% popularId
    )
}
get_id_name_mapping_from_menuJson <- function(menu) {
  menu$data$menus$menu_categories[[1]]$products |>
    purrr::map_dfr(
      ~.x[c("id","name")]
    )
}
get_popular_id_from_menuJson <- function(menu) {
  menu$data$menus$tags$popular$metadata$sorting |>
    unlist()
}

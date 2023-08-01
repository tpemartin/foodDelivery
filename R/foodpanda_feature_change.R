#' Parse menu feature into list of menu data frames
#'
#' @param df A data frame with menu column as string of Json
#'
#' @return a list of menu data frames
#' @export
parse_menu_to_list_of_dataframe <- function(df) {
  menu = df$menu
  menu |>
    purrr::map(
      purrr::safely(~{
        .x |>
          stringr::str_replace_all("'",'"') |>
          jsonlite::fromJSON() |>
          list2DF()
      })
    ) |>
    setNames(df$shopCode) -> list_menus
   list_menus |>
     purrr::map(
       ~purrr::pluck(.x, "result")
     )-> list_menus_result
   list_menus_result
}
#' Compare two Foodpanda data frame for preliminary feature change analysis
#'
#' @param df_before a data frame from foodpanda
#' @param df_after a data frame fro foodpanda
#' @param fp a Data Foodpanda class object
#' @param removeFeatures a character of feature names that don't need to be analyzed (default = '')
#'
#' @return a list of analyzed result
#' @export
analyze_feature_changes <- function(df_before, df_after, fp, removeFeatures="") {
  assertthat::assert_that(
    "Data Foodpanda" %in% class(fp),
    msg="fp input should be a Data Foodpanda class object"
  )
  fp$detect_feature_changes(df_before, df_after, removeFeatures = "") -> changes

  feature_change <- initialize_feature_change_for_commonShops(
    df_before, df_after, changes
  )

  summarise_change_in_shopNumbers(feature_change = feature_change) ->
    feature_change$shopNumbers
  summarise_change_in_discountType(feature_change = feature_change) ->
    feature_change$commonShop_summary$discountType

  analyse_discrete_change(feature_change$commonShop_data$before,
                          feature_change$commonShop_data$after,
                          feature="inShopPrice") ->
    feature_change$commonShop_summary$inShopPrice

  feature_change
}
#' A wrapper of knitr::kable
#'
#' @param ... parameters passed to kable
#'
#' @return pass kable return to clipboard for pasting somewhere else
#' @export
kable_clip <- function(...){
  knitr::kable(...) |>
    clipr::write_clip()
}

# helpers ----

summarise_change_in_discountType <- function(feature_change) {
  # Before how many has discount
  feature_change$commonShop_data$before = augment_discountType(feature_change$commonShop_data$before)
  feature_change$commonShop_data$after = augment_discountType(feature_change$commonShop_data$after)

  # change summary ------
  change_summary_discountType = analyse_discrete_change(
    feature_change$commonShop_data$before,
    feature_change$commonShop_data$after, "discountType"
  )
  colnames(change_summary_discountType$distribution) <- c(
    # "discount type",
    "before","after")
  change_summary_discountType
}# Change in number of shops
summarise_change_in_shopNumbers <- function( feature_change) {
  feature_change$data$changes[c( "exitShops", "newShops","commonShops")] |>
    purrr::map(length) |>
    setNames(c("only before", "only after", "Both Dates")) |>
    list2DF() -> commonShops_change_in_shop_numbers

  # class(commonShops_change_in_shop_numbers) <- "summary"
  commonShops_change_in_shop_numbers
}

produce_table_from_summary <- function(.x, caption=NULL, format="markdown") {
  # assertthat::assert_that(
  #   "summary" %in% class(.x),
  #   msg="Input is not a summary class object"
  # )
  .x  |>
    knitr::kable(
      caption=caption,
      format=format
    )
}

initialize_feature_change_for_commonShops <- function(df_before, df_after, changes) {
  feature_change <- list()
  feature_change$data = list(
    "before"= df_before,
    "after"= df_after,
    changes = changes
  )


  # Common shops analysis
  changes$commonShops -> commonShops


  df_before |>
    dplyr::filter(
      shopCode %in% changes$commonShops
    ) -> df_before_common
  df_after |>
    dplyr::filter(
      shopCode %in% changes$commonShops
    ) -> df_after_common

  feature_change$commonShop_data <- list(
    "before" = df_before_common,
    "after" = df_after_common
  )
  feature_change
}
augment_discountType <- function(df) {
  df$discount ->
    discounts

  discounts |>
    stringr::str_which("滿[^滿折]+折")  -> whichHas_discount_exceed_certain_amount
  which(discounts == "[]") -> whichHas_discount_none
  discounts |>
    stringr::str_which("指定[^折]+折") -> whichHas_discount_on_certain_item
  discounts |>
    stringr::str_which("滿[^滿折]+折|\\[\\]|指定[^折]+折", negate = T) ->
    whichHas_discount_others

  whichHasBothTypes =
    intersect(whichHas_discount_exceed_certain_amount,whichHas_discount_on_certain_item)
  whichHasOnly_discount_exceed_certain_amount =
    setdiff(whichHas_discount_exceed_certain_amount,whichHasBothTypes)
  whichHasOnly_discount_on_certain_item =
    setdiff(whichHas_discount_on_certain_item,whichHasBothTypes)

  max(whichHasBothTypes,
      whichHasOnly_discount_exceed_certain_amount,
      whichHasOnly_discount_on_certain_item,
      whichHas_discount_none,
      whichHas_discount_others)

  fct_discount = character(length=length(discounts))
  fct_discount[whichHas_discount_none] = "無折扣"
  fct_discount[whichHasOnly_discount_exceed_certain_amount] = "滿X折Y"
  fct_discount[whichHasOnly_discount_on_certain_item] = "指定XXY折"
  fct_discount[whichHasBothTypes] = "滿X折Y，同時指定XXY折"
  fct_discount[whichHas_discount_others] = "其它折扣"
  fct_discount = factor(fct_discount,
                        levels=c("無折扣", "滿X折Y", "指定XXY折","滿X折Y，同時指定XXY折","其它折扣"))

  df$discountType = fct_discount
  df
}
analyse_discrete_change <- function(df_before_common, df_after_common, feature) {
  table(df_before_common[[feature]]) |>
    as.data.frame() -> tb_before
  table(df_after_common[[feature]]) |>
    as.data.frame() -> tb_after

  dplyr::full_join(
    tb_before, tb_after, by="Var1"
  ) -> change_distribution

  .rownames <- change_distribution$Var1

  change_distribution <- change_distribution[, -1] |> as.matrix()

  rownames(change_distribution) <- .rownames

  colnames(change_distribution) <- c(
    # feature,
    "before", "after")

  df_before_common[,c("shopCode", feature)] |>
    dplyr::inner_join(
      df_after_common[,c("shopCode", feature)],
      by="shopCode"
    ) -> df_before_after

  featureX = paste0(feature,".x")
  featureY = paste0(feature,".y")
  df_before_after[[featureX]] <- as.factor(df_before_after[[featureX]])
  df_before_after[[featureY]] <- as.factor(df_before_after[[featureY]])
  mat_dim = length(levels(df_before_after[[featureX]]))
  df_before_after[[featureX]]:df_before_after[[featureY]] |>
    table() |>
    matrix(mat_dim, mat_dim, byrow = T) -> mat_before_after

  rownames(mat_before_after) <- df_before_after[[featureX]] |> levels()
  colnames(mat_before_after) <- df_before_after[[featureY]] |> levels()

  list(
    distribution = change_distribution,
    transition = mat_before_after
  )
}

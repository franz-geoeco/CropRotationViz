# Global variable declarations to avoid R CMD check NOTEs
# This file declares global variables used in NSE contexts (dplyr, data.table, etc.)

#' @importFrom grDevices adjustcolor
#' @importFrom methods as is
#' @importFrom stats as.formula weighted.mean
#' @importFrom utils setTxtProgressBar txtProgressBar
NULL

utils::globalVariables(c(
  # Data variables
  ".",
  "..agg_cols",
  "..crop_cols",
  "AOI",
  "Area",
  "BS_mean",
  "Category",
  "District",
  "Districts",
  "EZG",
  "EZG_CropRotViz_intersection",
  "EZGs",
  "Input_App_data",
  "NAME",
  "NAME_3",
  "NC",
  "Percentage",
  "Year",
  "area",
  "area_ha",
  "area_km2",
  "chunk_idx",
  "distribution_df",
  "district_CropRotViz_intersection",
  "diversity_data",
  "freq",
  "geometry",
  "group",
  "group_id",
  "input",
  "language",
  "mean_transi_weight",
  "mean_unique_weight",
  "n_fields",
  "percentage",
  "rank",
  "rotation",
  "rotation_short",
  "sankey_data",
  "target",
  "task",
  "tile_idx",
  "transitions",
  "unique_count",
  "value",
  "years"
))

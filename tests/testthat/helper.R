# Test helper functions and shared utilities

#' Create simple test polygon
#'
#' @param coords_offset Offset for coordinates (default 0)
#' @return sf polygon object
create_simple_polygon <- function(coords_offset = 0) {
  coords <- list(rbind(
    c(0 + coords_offset, 0 + coords_offset),
    c(1 + coords_offset, 0 + coords_offset),
    c(1 + coords_offset, 1 + coords_offset),
    c(0 + coords_offset, 1 + coords_offset),
    c(0 + coords_offset, 0 + coords_offset)
  ))
  sf::st_polygon(coords)
}

#' Create test field data with crops
#'
#' @param n_fields Number of fields to create
#' @param year Year for the data
#' @param crops Vector of crop names
#' @param nc_codes Vector of NC codes
#' @return sf object with fields
create_test_fields <- function(n_fields = 3, year = 2020,
                               crops = c("Wheat", "Corn", "Barley"),
                               nc_codes = c(110, 200, 300)) {

  # Recycle crops and codes if needed
  crops <- rep_len(crops, n_fields)
  nc_codes <- rep_len(nc_codes, n_fields)

  # Create polygons
  polys <- lapply(seq_len(n_fields), function(i) {
    create_simple_polygon(coords_offset = (i - 1) * 0.5)
  })

  # Create sf object
  df <- data.frame(
    id = seq_len(n_fields),
    stringsAsFactors = FALSE
  )
  df[[paste0("NC_", year)]] <- nc_codes
  df[[paste0("Name_", year)]] <- crops

  sf::st_sf(df, geometry = sf::st_sfc(polys), crs = 4326)
}

#' Create test crop code lookup table
#'
#' @return data.frame with NC codes and names
create_crop_codierung <- function() {
  data.frame(
    NC = c(110, 111, 112, 200, 201, 300, 301, 400, 500),
    german_names = c("Weizen", "Winterweizen", "Sommerweizen",
                     "Mais", "Silomais", "Gerste", "Wintergerste",
                     "Hafer", "Sojabohnen"),
    english_names = c("Wheat", "Winter Wheat", "Spring Wheat",
                      "Corn", "Silage Corn", "Barley", "Winter Barley",
                      "Oats", "Soybeans"),
    stringsAsFactors = FALSE
  )
}

#' Create test crop classification codes
#'
#' @return Named list of crop code groups
create_crop_classification <- function() {
  list(
    Cereals = c(110, 111, 112, 300, 301, 400),
    Corn = c(200, 201),
    Legumes = c(500, 501)
  )
}

#' Create test rotation data
#'
#' @param n_rows Number of rotation records
#' @param years Vector of years to include
#' @return data.frame with rotation sequences
create_rotation_data <- function(n_rows = 10, years = 2020:2022) {
  crops <- c("Wheat", "Corn", "Barley", "Soybean", "Rapeseed")

  df <- data.frame(
    Area_km2 = runif(n_rows, 1, 50),
    stringsAsFactors = FALSE
  )

  for (year in years) {
    df[[paste0("Name_", year)]] <- sample(crops, n_rows, replace = TRUE)
    df[[paste0("NC_", year)]] <- sample(c(110, 200, 300, 500, 600), n_rows, replace = TRUE)
  }

  df
}

#' Skip if running on CRAN
#'
#' Wrapper to skip resource-intensive tests on CRAN
skip_if_cran <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "false")) {
    skip("Skipping on CRAN")
  }
}

#' Skip if required package not available
#'
#' @param pkg Package name
skip_if_no_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    skip(paste("Package", pkg, "not available"))
  }
}

#' Create temporary output directory for tests
#'
#' @return Path to temporary directory
get_temp_test_dir <- function() {
  temp_dir <- file.path(tempdir(), paste0("CropRotationViz_test_",
                                          format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  temp_dir
}

#' Clean up test files
#'
#' @param pattern File pattern to match
#' @param path Directory path
cleanup_test_files <- function(pattern = "*", path = tempdir()) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  unlink(files, recursive = TRUE)
  invisible(NULL)
}

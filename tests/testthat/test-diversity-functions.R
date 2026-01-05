# Tests for diversity analysis functions

# Create test data for diversity mapping
create_test_diversity_data <- function() {
  coords_list <- list(
    list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))),
    list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0))),
    list(rbind(c(2,0), c(3,0), c(3,1), c(2,1), c(2,0)))
  )

  polys <- lapply(coords_list, sf::st_polygon)

  sf::st_sf(
    Aggregated_2020 = c("Wheat", "Corn", "Wheat"),
    Aggregated_2021 = c("Corn", "Wheat", "Barley"),
    Aggregated_2022 = c("Barley", "Barley", "Corn"),
    geometry = sf::st_sfc(polys[[1]], polys[[2]], polys[[3]]),
    crs = 4326
  )
}

create_test_districts <- function() {
  coords <- list(rbind(c(0,0), c(3,0), c(3,1), c(0,1), c(0,0)))
  poly <- sf::st_polygon(coords)

  sf::st_sf(
    NAME = "District1",
    geometry = sf::st_sfc(poly),
    crs = 4326
  )
}

# Tests for diversity_mapping
test_that("diversity_mapping calculates crop diversity metrics", {
  skip_on_cran()

  input <- create_test_diversity_data()
  districts <- create_test_districts()
  agg_cols <- c("Aggregated_2020", "Aggregated_2021", "Aggregated_2022")

  result <- diversity_mapping(
    input = input,
    agg_cols = agg_cols,
    districts = districts
  )

  # Check that result is a list
  expect_type(result, "list")

  # Check for District_div_data
  expect_true("District_div_data" %in% names(result))

  # Check that District_div_data has required components
  if (!is.null(result$District_div_data)) {
    expect_true("BISCALE" %in% names(result$District_div_data))
  }
})

test_that("diversity_mapping handles EZG (river basins)", {
  skip_on_cran()

  input <- create_test_diversity_data()
  districts <- create_test_districts()
  ezgs <- create_test_districts()  # Use same geometry for testing
  names(ezgs)[1] <- "Basin1"

  agg_cols <- c("Aggregated_2020", "Aggregated_2021", "Aggregated_2022")

  result <- diversity_mapping(
    input = input,
    agg_cols = agg_cols,
    districts = districts,
    EZGs = ezgs
  )

  # Check for EZG_div_data
  expect_true("EZG_div_data" %in% names(result))
})

test_that("diversity_mapping handles AOI (areas of interest)", {
  skip_on_cran()

  input <- create_test_diversity_data()
  districts <- create_test_districts()
  aois <- create_test_districts()  # Use same geometry for testing

  agg_cols <- c("Aggregated_2020", "Aggregated_2021", "Aggregated_2022")

  result <- diversity_mapping(
    input = input,
    agg_cols = agg_cols,
    districts = districts,
    AOIs = aois
  )

  # Check for AOI_div_data
  expect_true("AOI_div_data" %in% names(result))
})

test_that("diversity_mapping returns list with correct structure", {
  skip_on_cran()

  input <- create_test_diversity_data()
  districts <- create_test_districts()
  agg_cols <- c("Aggregated_2020", "Aggregated_2021", "Aggregated_2022")

  result <- diversity_mapping(
    input = input,
    agg_cols = agg_cols,
    districts = districts
  )

  # Result should be a list
  expect_type(result, "list")

  # Should have at least District_div_data
  expect_true(length(result) > 0)
})

# Tests for diversity_mapper
test_that("diversity_mapper creates leaflet map object", {
  skip_on_cran()
  skip_if_not_installed("leaflet")

  # Create mock diversity data structure
  coords <- list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))
  poly <- sf::st_polygon(coords)

  biscale_data <- sf::st_sf(
    NAME = "District1",
    unique_crops = 5,
    transitions = 3,
    bi_class = "2-2",
    geometry = sf::st_sfc(poly),
    crs = 4326
  )

  data <- list(
    BISCALE = biscale_data,
    color_pal = c("2-2" = "#FF0000"),
    labels1 = "Test label"
  )

  result <- diversity_mapper(
    data = data,
    type = "District"
  )

  # Check that result is a leaflet object
  expect_s3_class(result, "leaflet")
})

test_that("diversity_mapper accepts different types", {
  skip_on_cran()
  skip_if_not_installed("leaflet")

  coords <- list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))
  poly <- sf::st_polygon(coords)

  biscale_data <- sf::st_sf(
    NAME = "Basin1",
    unique_crops = 5,
    transitions = 3,
    bi_class = "2-2",
    geometry = sf::st_sfc(poly),
    crs = 4326
  )

  data <- list(
    BISCALE = biscale_data,
    color_pal = c("2-2" = "#FF0000"),
    labels1 = "Test label"
  )

  # Test with "River Basin" type
  result <- diversity_mapper(
    data = data,
    type = "River Basin"
  )

  expect_s3_class(result, "leaflet")
})

# Tests for diversity_soil_plotter
test_that("diversity_soil_plotter creates plotly boxplot", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  # Create mock data with soil values
  coords <- list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))
  poly <- sf::st_polygon(coords)

  biscale_data <- sf::st_sf(
    NAME = "District1",
    unique_crops = 5,
    transitions = 3,
    bi_class = "2-2",
    BS_mean = 75.5,
    geometry = sf::st_sfc(poly),
    crs = 4326
  )

  data <- list(
    BISCALE = biscale_data,
    color_pal = c("2-2" = "#FF0000"),
    labels1 = "Test label"
  )

  result <- diversity_soil_plotter(
    data = data,
    type = "District"
  )

  # Check that result is a plotly object
  expect_s3_class(result, "plotly")
})

test_that("diversity_soil_plotter handles different types", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  coords <- list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))
  poly <- sf::st_polygon(coords)

  biscale_data <- sf::st_sf(
    NAME = "Basin1",
    unique_crops = 5,
    transitions = 3,
    bi_class = "3-1",
    BS_mean = 80.2,
    geometry = sf::st_sfc(poly),
    crs = 4326
  )

  data <- list(
    BISCALE = biscale_data,
    color_pal = c("3-1" = "#00FF00"),
    labels1 = "Test label"
  )

  result <- diversity_soil_plotter(
    data = data,
    type = "River Basin"
  )

  expect_s3_class(result, "plotly")
})

test_that("diversity_soil_plotter handles multiple bi_class categories", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  coords_list <- list(
    list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))),
    list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)))
  )

  polys <- lapply(coords_list, sf::st_polygon)

  biscale_data <- sf::st_sf(
    NAME = c("District1", "District2"),
    unique_crops = c(5, 7),
    transitions = c(3, 4),
    bi_class = c("2-2", "3-3"),
    BS_mean = c(75.5, 85.2),
    geometry = sf::st_sfc(polys[[1]], polys[[2]]),
    crs = 4326
  )

  data <- list(
    BISCALE = biscale_data,
    color_pal = c("2-2" = "#FF0000", "3-3" = "#00FF00"),
    labels1 = "Test label"
  )

  result <- diversity_soil_plotter(
    data = data,
    type = "District"
  )

  expect_s3_class(result, "plotly")
})

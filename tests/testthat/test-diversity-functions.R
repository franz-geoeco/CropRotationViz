# Tests for diversity analysis functions

# Create test data for diversity mapping
create_test_diversity_data <- function() {
  # Create 12 fields - 6 per district with varied diversity patterns
  coords_list <- lapply(0:11, function(i) {
    list(rbind(c(i,0), c(i+1,0), c(i+1,1), c(i,1), c(i,0)))
  })

  polys <- lapply(coords_list, sf::st_polygon)

  sf::st_sf(
    District = c(rep("District1", 6), rep("District2", 6)),
    area = c(1.0, 1.5, 2.0, 1.8, 2.2, 1.3, 0.8, 1.2, 2.5, 1.9, 1.1, 2.3),
    # District1: High diversity - many different crops and transitions
    # District2: Low diversity - same crops repeated, fewer transitions
    Aggregated_2020 = c("Wheat", "Corn", "Barley", "Rapeseed", "Soybean", "Wheat",
                        "Wheat", "Wheat", "Corn", "Corn", "Wheat", "Corn"),
    Aggregated_2021 = c("Corn", "Barley", "Rapeseed", "Soybean", "Wheat", "Corn",
                        "Wheat", "Corn", "Wheat", "Corn", "Wheat", "Wheat"),
    Aggregated_2022 = c("Barley", "Rapeseed", "Soybean", "Wheat", "Corn", "Barley",
                        "Wheat", "Wheat", "Corn", "Wheat", "Corn", "Wheat"),
    geometry = sf::st_sfc(polys[[1]], polys[[2]], polys[[3]], polys[[4]], polys[[5]], polys[[6]],
                          polys[[7]], polys[[8]], polys[[9]], polys[[10]], polys[[11]], polys[[12]]),
    crs = 4326
  )
}

create_test_districts <- function() {
  coords_list <- list(
    list(rbind(c(0,0), c(6,0), c(6,1), c(0,1), c(0,0))),
    list(rbind(c(6,0), c(12,0), c(12,1), c(6,1), c(6,0)))
  )

  polys <- lapply(coords_list, sf::st_polygon)

  sf::st_sf(
    NAME = c("District1", "District2"),
    geometry = sf::st_sfc(polys[[1]], polys[[2]]),
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
  expect_equal(length(result), 3)

  # Check that first element (District_div_data) exists and has required components
  expect_false(is.null(result[[1]]))
  expect_true("BISCALE" %in% names(result[[1]]))
  expect_true("color_pal" %in% names(result[[1]]))
  expect_true("labels1" %in% names(result[[1]]))
})

test_that("diversity_mapping handles EZG (river basins)", {
  skip_on_cran()

  input <- create_test_diversity_data()
  # Add EZG column to input data
  input$EZG <- rep(c("Basin1", "Basin2"), each = 6)

  districts <- create_test_districts()
  ezgs <- create_test_districts()  # Use same geometry for testing
  names(ezgs)[1] <- "EZG"  # Column name must be "EZG"
  ezgs$EZG <- c("Basin1", "Basin2")

  agg_cols <- c("Aggregated_2020", "Aggregated_2021", "Aggregated_2022")

  result <- diversity_mapping(
    input = input,
    agg_cols = agg_cols,
    districts = districts,
    EZGs = ezgs
  )

  # Check for EZG_div_data (second element in returned list)
  expect_false(is.null(result[[2]]))
  expect_true("BISCALE" %in% names(result[[2]]))
})

test_that("diversity_mapping handles AOI (areas of interest)", {
  skip_on_cran()

  input <- create_test_diversity_data()
  districts <- create_test_districts()
  aois <- create_test_districts()  # Use same geometry for testing

  # Set up AOI column names correctly
  names(aois)[1] <- "AOI"  # Column name must be "AOI"
  aois$AOI <- c("AOI1", "AOI2")

  agg_cols <- c("Aggregated_2020", "Aggregated_2021", "Aggregated_2022")

  # Need to add AOI column to input data for AOI processing
  input$AOI <- rep(c("AOI1", "AOI2"), each = 6)

  result <- diversity_mapping(
    input = input,
    agg_cols = agg_cols,
    districts = districts,
    AOIs = aois
  )

  # Check for AOI_div_data (third element in returned list)
  expect_false(is.null(result[[3]]))
  expect_true("BISCALE" %in% names(result[[3]]))
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
    District = "District1",
    mean_unique_weight = 2.5,
    mean_transi_weight = 1.8,
    bi_class = "2-2",
    geometry = sf::st_sfc(poly),
    crs = 4326
  )

  color_pal <- leaflet::colorFactor(
    palette = c("2-2" = "#FF0000"),
    domain = "2-2"
  )

  labels1 <- data.frame(
    bi_x = c("Low", "Medium", "High"),
    bi_y = c("Low", "Medium", "High")
  )

  data <- list(biscale_data, color_pal, labels1)

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
    EZG = "Basin1",
    mean_unique_weight = 2.5,
    mean_transi_weight = 1.8,
    bi_class = "2-2",
    geometry = sf::st_sfc(poly),
    crs = 4326
  )

  color_pal <- leaflet::colorFactor(
    palette = c("2-2" = "#FF0000"),
    domain = "2-2"
  )

  labels1 <- data.frame(
    bi_x = c("Low", "Medium", "High"),
    bi_y = c("Low", "Medium", "High")
  )

  data <- list(biscale_data, color_pal, labels1)

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
    District = "District1",
    mean_unique_weight = 2.5,
    mean_transi_weight = 1.8,
    bi_class = "2-2",
    BS_mean = 75.5,
    geometry = sf::st_sfc(poly),
    crs = 4326
  )

  color_pal <- leaflet::colorFactor(
    palette = c("2-2" = "#FF0000"),
    domain = "2-2"
  )

  labels1 <- data.frame(
    bi_x = c("Low", "Medium", "High"),
    bi_y = c("Low", "Medium", "High")
  )

  data <- list(biscale_data, color_pal, labels1)

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
    EZG = "Basin1",
    mean_unique_weight = 3.2,
    mean_transi_weight = 2.1,
    bi_class = "3-1",
    BS_mean = 80.2,
    geometry = sf::st_sfc(poly),
    crs = 4326
  )

  color_pal <- leaflet::colorFactor(
    palette = c("3-1" = "#00FF00"),
    domain = "3-1"
  )

  labels1 <- data.frame(
    bi_x = c("Low", "Medium", "High"),
    bi_y = c("Low", "Medium", "High")
  )

  data <- list(biscale_data, color_pal, labels1)

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
    District = c("District1", "District2"),
    mean_unique_weight = c(2.5, 3.5),
    mean_transi_weight = c(1.8, 2.4),
    bi_class = c("2-2", "3-3"),
    BS_mean = c(75.5, 85.2),
    geometry = sf::st_sfc(polys[[1]], polys[[2]]),
    crs = 4326
  )

  color_pal <- leaflet::colorFactor(
    palette = c("2-2" = "#FF0000", "3-3" = "#00FF00"),
    domain = c("2-2", "3-3")
  )

  labels1 <- data.frame(
    bi_x = c("Low", "Medium", "High"),
    bi_y = c("Low", "Medium", "High")
  )

  data <- list(biscale_data, color_pal, labels1)

  result <- diversity_soil_plotter(
    data = data,
    type = "District"
  )

  expect_s3_class(result, "plotly")
})

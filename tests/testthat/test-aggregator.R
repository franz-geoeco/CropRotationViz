# Tests for aggregator function

# Create test data
create_test_intersected <- function() {
  coords <- list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))
  poly <- sf::st_polygon(coords)

  sf::st_sf(
    NC_2020 = c(110, 200, 300),
    NC_2021 = c(200, 300, 110),
    geometry = sf::st_sfc(poly, poly, poly),
    crs = 4326
  )
}

create_test_crop_codes <- function() {
  list(
    Cereals = c(110, 111, 112),
    Corn = c(200, 201),
    Barley = c(300, 301)
  )
}

test_that("aggregator adds Aggregated columns for each year", {
  intersected <- create_test_intersected()
  years <- c(2020, 2021)
  crop_codes <- create_test_crop_codes()

  result <- aggregator(
    intersected = intersected,
    years = years,
    crop_codes = crop_codes,
    type = "NC"
  )

  # Check that Aggregated columns were added
  expect_true("Aggregated_2020" %in% names(result))
  expect_true("Aggregated_2021" %in% names(result))
})

test_that("aggregator correctly classifies crops by NC code", {
  intersected <- create_test_intersected()
  years <- c(2020, 2021)
  crop_codes <- create_test_crop_codes()

  result <- aggregator(
    intersected = intersected,
    years = years,
    crop_codes = crop_codes,
    type = "NC"
  )

  # Check classifications
  expect_equal(result$Aggregated_2020, c("Cereals", "Corn", "Barley"))
  expect_equal(result$Aggregated_2021, c("Corn", "Barley", "Cereals"))
})

test_that("aggregator preserves original columns", {
  intersected <- create_test_intersected()
  years <- c(2020, 2021)
  crop_codes <- create_test_crop_codes()

  original_names <- names(intersected)

  result <- aggregator(
    intersected = intersected,
    years = years,
    crop_codes = crop_codes,
    type = "NC"
  )

  # Check that original columns are still present
  expect_true(all(original_names %in% names(result)))
})

test_that("aggregator works with Name columns when type is not NC", {
  coords <- list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))
  poly <- sf::st_polygon(coords)

  intersected <- sf::st_sf(
    Name_2020 = c("Wheat", "Corn", "Barley"),
    Name_2021 = c("Corn", "Barley", "Wheat"),
    geometry = sf::st_sfc(poly, poly, poly),
    crs = 4326
  )

  years <- c(2020, 2021)

  # Crop codes with names instead of numeric codes
  crop_codes <- list(
    Cereals = c("Wheat", "Rye"),
    Corn = c("Corn"),
    Barley = c("Barley")
  )

  result <- aggregator(
    intersected = intersected,
    years = years,
    crop_codes = crop_codes,
    type = "Name"
  )

  # Check classifications
  expect_equal(result$Aggregated_2020, c("Cereals", "Corn", "Barley"))
  expect_equal(result$Aggregated_2021, c("Corn", "Barley", "Cereals"))
})

test_that("aggregator handles crops not in classification list", {
  intersected <- create_test_intersected()
  years <- c(2020)

  # Incomplete crop codes (missing 300)
  crop_codes <- list(
    Cereals = c(110, 111),
    Corn = c(200, 201)
  )

  result <- aggregator(
    intersected = intersected,
    years = years,
    crop_codes = crop_codes,
    type = "NC"
  )

  # Field with NC 300 should be classified as something (e.g., NA, "Other", or the original code)
  # The exact behavior depends on implementation
  expect_true("Aggregated_2020" %in% names(result))
  expect_equal(length(result$Aggregated_2020), 3)
})

test_that("aggregator preserves sf object class", {
  intersected <- create_test_intersected()
  years <- c(2020, 2021)
  crop_codes <- create_test_crop_codes()

  result <- aggregator(
    intersected = intersected,
    years = years,
    crop_codes = crop_codes,
    type = "NC"
  )

  expect_s3_class(result, "sf")
})

test_that("aggregator returns same number of rows as input", {
  intersected <- create_test_intersected()
  years <- c(2020, 2021)
  crop_codes <- create_test_crop_codes()

  result <- aggregator(
    intersected = intersected,
    years = years,
    crop_codes = crop_codes,
    type = "NC"
  )

  expect_equal(nrow(result), nrow(intersected))
})

# Tests for dummy_field_creator function

test_that("dummy_field_creator creates correct number of fields", {
  skip_on_cran()

  temp_dir <- tempdir()

  result <- dummy_field_creator(
    output_dir = temp_dir,
    field_count = 10,
    years = 2020:2021,
    min_field_size = 0.003,
    max_field_size = 0.005
  )

  # Check that result is a list
  expect_type(result, "list")

  # Check that we have the right number of years
  expect_equal(length(result), 2)

  # Check that each year has the right number of fields
  expect_equal(nrow(result[[1]]), 10)
  expect_equal(nrow(result[[2]]), 10)

  # Clean up
  unlink(file.path(temp_dir, "*.shp"))
  unlink(file.path(temp_dir, "*.dbf"))
  unlink(file.path(temp_dir, "*.shx"))
  unlink(file.path(temp_dir, "*.prj"))
})

test_that("dummy_field_creator creates valid sf objects", {
  skip_on_cran()

  temp_dir <- tempdir()

  result <- dummy_field_creator(
    output_dir = temp_dir,
    field_count = 5,
    years = 2020,
    min_field_size = 0.003,
    max_field_size = 0.005
  )

  # Check that result contains sf objects
  expect_s3_class(result[[1]], "sf")

  # Check for required columns
  expect_true("NC_2020" %in% names(result[[1]]))
  expect_true("Name_2020" %in% names(result[[1]]))
  expect_true("geometry" %in% names(result[[1]]))

  # Check that geometries are polygons
  expect_equal(unique(sf::st_geometry_type(result[[1]])), sf::st_geometry_type("POLYGON"))

  # Clean up
  unlink(file.path(temp_dir, "*.shp"))
  unlink(file.path(temp_dir, "*.dbf"))
  unlink(file.path(temp_dir, "*.shx"))
  unlink(file.path(temp_dir, "*.prj"))
})

test_that("dummy_field_creator respects base_location parameter", {
  skip_on_cran()

  temp_dir <- tempdir()
  custom_location <- c(13.4, 52.5)  # Berlin coordinates

  result <- dummy_field_creator(
    output_dir = temp_dir,
    base_location = custom_location,
    field_count = 3,
    years = 2020
  )

  # Get centroids of created fields
  centroids <- sf::st_coordinates(sf::st_centroid(result[[1]]))

  # Check that fields are near the specified location (within ~0.01 degrees)
  expect_true(all(abs(centroids[, "X"] - custom_location[1]) < 0.1))
  expect_true(all(abs(centroids[, "Y"] - custom_location[2]) < 0.1))

  # Clean up
  unlink(file.path(temp_dir, "*.shp"))
  unlink(file.path(temp_dir, "*.dbf"))
  unlink(file.path(temp_dir, "*.shx"))
  unlink(file.path(temp_dir, "*.prj"))
})

test_that("dummy_field_creator creates fields with valid NC codes", {
  skip_on_cran()

  temp_dir <- tempdir()

  result <- dummy_field_creator(
    output_dir = temp_dir,
    field_count = 20,
    years = 2020:2021
  )

  # Check that NC codes are numeric
  expect_type(result[[1]]$NC_2020, "integer")
  expect_type(result[[2]]$NC_2021, "integer")

  # Check that NC codes are within expected range (100-999 for agricultural crops)
  expect_true(all(result[[1]]$NC_2020 >= 100 & result[[1]]$NC_2020 <= 999))
  expect_true(all(result[[2]]$NC_2021 >= 100 & result[[2]]$NC_2021 <= 999))

  # Clean up
  unlink(file.path(temp_dir, "*.shp"))
  unlink(file.path(temp_dir, "*.dbf"))
  unlink(file.path(temp_dir, "*.shx"))
  unlink(file.path(temp_dir, "*.prj"))
})

test_that("dummy_field_creator writes shapefiles to disk", {
  skip_on_cran()

  temp_dir <- tempdir()

  result <- dummy_field_creator(
    output_dir = temp_dir,
    field_count = 5,
    years = 2020:2021
  )

  # Check that shapefiles were created
  expect_true(file.exists(file.path(temp_dir, "fields_2020.shp")))
  expect_true(file.exists(file.path(temp_dir, "fields_2021.shp")))

  # Clean up
  unlink(file.path(temp_dir, "*.shp"))
  unlink(file.path(temp_dir, "*.dbf"))
  unlink(file.path(temp_dir, "*.shx"))
  unlink(file.path(temp_dir, "*.prj"))
})

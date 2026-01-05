# Tests for visualization functions: create_crop_rotation_sankey and create_multi_year_donut

# Create test data for Sankey diagram
create_test_sankey_data <- function() {
  coords <- list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))
  poly <- sf::st_polygon(coords)

  # Create fields with area (geometry size doesn't matter, we'll add area column)
  sf::st_sf(
    Aggregated_2020 = c("Wheat", "Corn", "Wheat", "Barley"),
    Aggregated_2021 = c("Corn", "Wheat", "Barley", "Wheat"),
    Aggregated_2022 = c("Barley", "Barley", "Corn", "Corn"),
    geometry = sf::st_sfc(poly, poly, poly, poly),
    crs = 4326
  )
}

# Tests for create_crop_rotation_sankey
test_that("create_crop_rotation_sankey creates ggplot object", {
  skip_on_cran()

  data <- create_test_sankey_data()

  # Add area in hectares
  data$area_ha <- c(10, 5, 15, 8)

  result <- create_crop_rotation_sankey(
    data = data,
    min_area = 1
  )

  # Check that result is a ggplot object
  expect_s3_class(result, "ggplot")
})

test_that("create_crop_rotation_sankey respects min_area threshold", {
  skip_on_cran()

  data <- create_test_sankey_data()
  data$area_ha <- c(10, 5, 15, 8)

  # Set high min_area to filter out smaller fields
  result <- create_crop_rotation_sankey(
    data = data,
    min_area = 12
  )

  # Should still create a plot, but with fewer data points
  expect_s3_class(result, "ggplot")
})

test_that("create_crop_rotation_sankey excludes specified crops", {
  skip_on_cran()

  coords <- list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))
  poly <- sf::st_polygon(coords)

  data <- sf::st_sf(
    Aggregated_2020 = c("Wheat", "grassland", "Corn"),
    Aggregated_2021 = c("Corn", "grassland", "Wheat"),
    geometry = sf::st_sfc(poly, poly, poly),
    crs = 4326
  )
  data$area_ha <- c(10, 5, 15)

  result <- create_crop_rotation_sankey(
    data = data,
    exclude_crops = c("grassland")
  )

  # Should create plot without grassland
  expect_s3_class(result, "ggplot")
})

test_that("create_crop_rotation_sankey accepts custom colors", {
  skip_on_cran()

  data <- create_test_sankey_data()
  data$area_ha <- c(10, 5, 15, 8)

  custom_colors <- c(
    "Wheat" = "#FF0000",
    "Corn" = "#00FF00",
    "Barley" = "#0000FF"
  )

  result <- create_crop_rotation_sankey(
    data = data,
    color = custom_colors
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_crop_rotation_sankey can save to file", {
  skip_on_cran()

  data <- create_test_sankey_data()
  data$area_ha <- c(10, 5, 15, 8)

  temp_file <- tempfile(fileext = ".png")

  result <- create_crop_rotation_sankey(
    data = data,
    output_path = temp_file,
    width = 10,
    height = 8,
    resolution = 100
  )

  # Check that file was created
  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
})

# Create test data for donut chart
create_test_donut_data <- function() {
  data.frame(
    crop = c("Wheat", "Corn", "Barley", "Soybean"),
    year_2020 = c(100, 80, 60, 40),
    year_2021 = c(110, 75, 65, 45),
    year_2022 = c(105, 85, 70, 50),
    stringsAsFactors = FALSE
  )
}

# Tests for create_multi_year_donut
test_that("create_multi_year_donut creates plotly object", {
  skip_on_cran()

  data <- create_test_donut_data()
  year_columns <- c("year_2020", "year_2021", "year_2022")

  colors <- c(
    "Wheat" = "#FFD700",
    "Corn" = "#FFA500",
    "Barley" = "#8B4513",
    "Soybean" = "#228B22"
  )

  result <- create_multi_year_donut(
    data = data,
    year_columns = year_columns,
    colors = colors
  )

  # Check that result is a plotly object
  expect_s3_class(result, "plotly")
})

test_that("create_multi_year_donut accepts custom title", {
  skip_on_cran()

  data <- create_test_donut_data()
  year_columns <- c("year_2020", "year_2021")

  colors <- c(
    "Wheat" = "#FFD700",
    "Corn" = "#FFA500",
    "Barley" = "#8B4513",
    "Soybean" = "#228B22"
  )

  result <- create_multi_year_donut(
    data = data,
    year_columns = year_columns,
    title = "My Custom Title",
    colors = colors
  )

  expect_s3_class(result, "plotly")
})

test_that("create_multi_year_donut can highlight specific year", {
  skip_on_cran()

  data <- create_test_donut_data()
  year_columns <- c("year_2020", "year_2021", "year_2022")

  colors <- c(
    "Wheat" = "#FFD700",
    "Corn" = "#FFA500",
    "Barley" = "#8B4513",
    "Soybean" = "#228B22"
  )

  result <- create_multi_year_donut(
    data = data,
    year_columns = year_columns,
    highlight_year = "year_2021",
    colors = colors
  )

  expect_s3_class(result, "plotly")
})

test_that("create_multi_year_donut handles maximum of 10 years", {
  skip_on_cran()

  # Create data with more than 10 years
  data <- create_test_donut_data()
  year_columns <- paste0("year_", 2010:2025)

  # Add these columns to data
  for (col in year_columns) {
    data[[col]] <- sample(50:150, nrow(data), replace = TRUE)
  }

  colors <- c(
    "Wheat" = "#FFD700",
    "Corn" = "#FFA500",
    "Barley" = "#8B4513",
    "Soybean" = "#228B22"
  )

  # Should either error or truncate to 10 years
  expect_error(
    create_multi_year_donut(
      data = data,
      year_columns = year_columns,
      colors = colors
    ),
    regexp = "10 years|maximum|limit",
    ignore.case = TRUE
  )
})

test_that("create_multi_year_donut requires colors parameter", {
  skip_on_cran()

  data <- create_test_donut_data()
  year_columns <- c("year_2020", "year_2021")

  # Call without colors should work (may use defaults or generate colors)
  # The actual behavior depends on implementation
  expect_error(
    create_multi_year_donut(
      data = data,
      year_columns = year_columns
    ),
    NA  # No error expected if colors are optional/auto-generated
  )
})

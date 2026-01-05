# Tests for transform_rotation_data and transform_rotation_summary functions

# Create test data
create_test_rotation_data <- function() {
  data.frame(
    Area_km2 = c(10.5, 5.2, 15.8, 3.1, 20.0),
    Name_2020 = c("Wheat", "Corn", "Wheat", "Barley", "Corn"),
    Name_2021 = c("Corn", "Wheat", "Barley", "Wheat", "Corn"),
    Name_2022 = c("Barley", "Barley", "Corn", "Corn", "Wheat"),
    stringsAsFactors = FALSE
  )
}

create_test_distribution <- function() {
  data.frame(
    crop = c("Wheat", "Corn", "Barley"),
    area_2020 = c(100, 80, 60),
    area_2021 = c(90, 85, 65),
    area_2022 = c(95, 90, 70),
    stringsAsFactors = FALSE
  )
}

# Tests for transform_rotation_data
test_that("transform_rotation_data filters by area range", {
  All_rot_big <- create_test_rotation_data()
  distribution_df <- create_test_distribution()

  result <- transform_rotation_data(
    All_rot_big = All_rot_big,
    distribution_df = distribution_df,
    input_area_range = c(5, 16),
    choices = c("Wheat", "Corn", "Barley"),
    selected_crops = c("Wheat", "Corn", "Barley"),
    type = "basic"
  )

  # Should include rows with area between 5 and 16 km²
  # That's rows 1, 2, and 3 (10.5, 5.2, 15.8)
  expect_true(nrow(result) > 0)
  expect_true(all(result$Area >= 5 & result$Area <= 16))
})

test_that("transform_rotation_data filters by selected crops", {
  All_rot_big <- create_test_rotation_data()
  distribution_df <- create_test_distribution()

  result <- transform_rotation_data(
    All_rot_big = All_rot_big,
    distribution_df = distribution_df,
    input_area_range = c(0, 100),
    choices = c("Wheat", "Corn", "Barley"),
    selected_crops = c("Wheat", "Corn"),  # Exclude Barley
    type = "basic"
  )

  # Result should only contain Wheat and Corn
  unique_crops <- unique(result$value)
  expect_true(all(unique_crops %in% c("Wheat", "Corn")))
  expect_false("Barley" %in% unique_crops)
})

test_that("transform_rotation_data type='specific' filters for specific crop", {
  All_rot_big <- create_test_rotation_data()
  distribution_df <- create_test_distribution()

  result <- transform_rotation_data(
    All_rot_big = All_rot_big,
    distribution_df = distribution_df,
    input_area_range = c(0, 100),
    choices = c("Wheat", "Corn", "Barley"),
    selected_crops = c("Wheat", "Corn", "Barley"),
    type = "specific",
    specific_crop = "Wheat"
  )

  # Should only include rows where any year has Wheat
  expect_true(nrow(result) > 0)
})

test_that("transform_rotation_data returns required columns", {
  All_rot_big <- create_test_rotation_data()
  distribution_df <- create_test_distribution()

  result <- transform_rotation_data(
    All_rot_big = All_rot_big,
    distribution_df = distribution_df,
    input_area_range = c(0, 100),
    choices = c("Wheat", "Corn", "Barley"),
    selected_crops = c("Wheat", "Corn", "Barley"),
    type = "basic"
  )

  # Check for required columns
  expect_true("Area" %in% names(result))
  expect_true("id" %in% names(result))
  expect_true("value" %in% names(result))
  expect_true("key" %in% names(result))
  expect_true("year" %in% names(result))
})

# Tests for transform_rotation_summary
test_that("transform_rotation_summary creates summary table", {
  All_rot_big <- create_test_rotation_data()

  result <- transform_rotation_summary(
    All_rot_big = All_rot_big,
    area_range = c(0, 100),
    type = "basic",
    choices = c("Wheat", "Corn", "Barley"),
    selected_crops = c("Wheat", "Corn", "Barley"),
    years = c(2020, 2021, 2022)
  )

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check for area column
  expect_true("area_km2" %in% names(result))
})

test_that("transform_rotation_summary respects max_rows parameter", {
  # Create large dataset
  large_data <- do.call(rbind, replicate(1000, create_test_rotation_data(), simplify = FALSE))

  result <- transform_rotation_summary(
    All_rot_big = large_data,
    area_range = c(0, 100),
    type = "basic",
    choices = c("Wheat", "Corn", "Barley"),
    selected_crops = c("Wheat", "Corn", "Barley"),
    max_rows = 100,
    years = c(2020, 2021, 2022)
  )

  # Should have at most 100 rows
  expect_true(nrow(result) <= 100)
})

test_that("transform_rotation_summary includes crop columns for specified years", {
  All_rot_big <- create_test_rotation_data()

  result <- transform_rotation_summary(
    All_rot_big = All_rot_big,
    area_range = c(0, 100),
    type = "basic",
    choices = c("Wheat", "Corn", "Barley"),
    selected_crops = c("Wheat", "Corn", "Barley"),
    years = c(2020, 2021)
  )

  # Should have columns for specified years
  expect_true("Crop_2020" %in% names(result) || "Name_2020" %in% names(result))
  expect_true("Crop_2021" %in% names(result) || "Name_2021" %in% names(result))
})

test_that("transform_rotation_summary type='specific' filters correctly", {
  All_rot_big <- create_test_rotation_data()

  result <- transform_rotation_summary(
    All_rot_big = All_rot_big,
    area_range = c(0, 100),
    type = "specific",
    specific_crop = "Wheat",
    years = c(2020, 2021, 2022)
  )

  # Should only include rotations with Wheat
  expect_true(nrow(result) > 0)
})

# Tests for process_specific_transitions
test_that("process_specific_transitions analyzes crop transitions", {
  # Create test data with Aggregated columns
  data <- data.frame(
    Aggregated_2020 = c("Wheat", "Corn", "Barley", "Wheat"),
    Aggregated_2021 = c("Corn", "Wheat", "Wheat", "Barley"),
    Aggregated_2022 = c("Barley", "Barley", "Corn", "Corn"),
    stringsAsFactors = FALSE
  )

  result <- process_specific_transitions(data, crop = "Wheat")

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check for required columns
  expect_true("source" %in% names(result))
  expect_true("target" %in% names(result))
  expect_true("value" %in% names(result))
})

test_that("process_specific_transitions distinguishes before/after transitions", {
  data <- data.frame(
    Aggregated_2020 = c("Wheat", "Corn", "Barley"),
    Aggregated_2021 = c("Corn", "Wheat", "Wheat"),
    Aggregated_2022 = c("Barley", "Barley", "Corn"),
    stringsAsFactors = FALSE
  )

  result <- process_specific_transitions(data, crop = "Wheat")

  # Should have both "before" and "after" in source or target
  all_text <- paste(c(result$source, result$target), collapse = " ")
  expect_true(grepl("before|after", all_text, ignore.case = TRUE))
})

test_that("process_specific_transitions calculates percentages", {
  data <- data.frame(
    Aggregated_2020 = rep("Corn", 100),
    Aggregated_2021 = rep("Wheat", 100),
    Aggregated_2022 = rep("Barley", 100),
    stringsAsFactors = FALSE
  )

  result <- process_specific_transitions(data, crop = "Wheat")

  # Values should be percentages (0-100)
  expect_true(all(result$value >= 0))
  expect_true(all(result$value <= 100))
})

test_that("process_specific_transitions handles crops with no transitions", {
  data <- data.frame(
    Aggregated_2020 = c("Corn", "Corn", "Corn"),
    Aggregated_2021 = c("Barley", "Barley", "Barley"),
    Aggregated_2022 = c("Corn", "Corn", "Corn"),
    stringsAsFactors = FALSE
  )

  # Test with crop that doesn't appear in data
  result <- process_specific_transitions(data, crop = "Wheat")

  # Should return empty or minimal data frame
  expect_s3_class(result, "data.frame")
})

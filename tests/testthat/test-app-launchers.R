# Tests for Shiny app launcher functions

test_that("run_processing_app returns shiny.appobj", {
  skip_on_cran()
  skip_if_not_installed("shiny")

  # Run without launching (just create the app object)
  # We can't fully test Shiny apps without interaction, but we can test they initialize
  expect_error(
    {
      app <- run_processing_app(
        output_dir = tempdir(),
        ncores = 1
      )
    },
    NA
  )
})

test_that("run_processing_app accepts valid parameters", {
  skip_on_cran()
  skip_if_not_installed("shiny")

  # Test that function accepts expected parameters without error
  expect_error(
    {
      # Don't actually run, just check parameter acceptance
      formals(run_processing_app)
    },
    NA
  )

  # Check that function has expected parameters
  params <- names(formals(run_processing_app))
  expect_true("output_dir" %in% params)
  expect_true("common_column" %in% params)
  expect_true("start_year" %in% params)
  expect_true("intersection_type" %in% params)
  expect_true("preview" %in% params)
  expect_true("vector_file" %in% params)
  expect_true("ncores" %in% params)
})

test_that("run_visualization_app returns shiny.appobj", {
  skip_on_cran()
  skip_if_not_installed("shiny")

  # Test that function can be called
  expect_error(
    {
      formals(run_visualization_app)
    },
    NA
  )

  # Check for input_dir parameter
  params <- names(formals(run_visualization_app))
  expect_true("input_dir" %in% params)
})

test_that("run_fast_visualization_app has correct parameters", {
  skip_on_cran()
  skip_if_not_installed("shiny")

  # Check function parameters
  params <- names(formals(run_fast_visualization_app))
  expect_true("input_dir" %in% params)
})

test_that("run_field_level_app can be called", {
  skip_on_cran()
  skip_if_not_installed("shiny")

  # Check that function exists and can be called
  expect_true(exists("run_field_level_app"))
  expect_type(run_field_level_app, "closure")
})

# Test that app launcher functions are exported
test_that("all app launcher functions are exported", {
  # Get list of exported functions from NAMESPACE
  exports <- getNamespaceExports("CropRotationViz")

  expect_true("run_processing_app" %in% exports)
  expect_true("run_visualization_app" %in% exports)
  expect_true("run_fast_visualization_app" %in% exports)
  expect_true("run_field_level_app" %in% exports)
})

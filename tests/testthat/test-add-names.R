# Tests for add_names function

# Create test data
create_test_fields <- function() {
  coords <- list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))
  poly <- sf::st_polygon(coords)

  field1 <- sf::st_sf(
    NC_2020 = c(110, 200),
    geometry = sf::st_sfc(poly, poly),
    crs = 4326
  )

  field2 <- sf::st_sf(
    NC_2021 = c(300, 110),
    geometry = sf::st_sfc(poly, poly),
    crs = 4326
  )

  list(`2020` = field1, `2021` = field2)
}

create_test_codierung <- function() {
  data.frame(
    NC = c(110, 200, 300),
    german_names = c("Weizen", "Mais", "Gerste"),
    english_names = c("Wheat", "Corn", "Barley"),
    stringsAsFactors = FALSE
  )
}

test_that("add_names adds Name columns for NC codes in English", {
  fields_list <- create_test_fields()
  codierung_all <- create_test_codierung()

  result <- add_names(
    fields_list = fields_list,
    codierung_all = codierung_all,
    column = "Code",
    language = "English"
  )

  # Check that Name columns were added
  expect_true("Name_2020" %in% names(result[[1]]))
  expect_true("Name_2021" %in% names(result[[2]]))

  # Check that names are correct
  expect_equal(result[[1]]$Name_2020, c("Wheat", "Corn"))
  expect_equal(result[[2]]$Name_2021, c("Barley", "Wheat"))
})

test_that("add_names adds Name columns for NC codes in German", {
  fields_list <- create_test_fields()
  codierung_all <- create_test_codierung()

  result <- add_names(
    fields_list = fields_list,
    codierung_all = codierung_all,
    column = "Code",
    language = "German"
  )

  # Check that names are in German
  expect_equal(result[[1]]$Name_2020, c("Weizen", "Mais"))
  expect_equal(result[[2]]$Name_2021, c("Gerste", "Weizen"))
})

test_that("add_names preserves original columns", {
  fields_list <- create_test_fields()
  codierung_all <- create_test_codierung()

  result <- add_names(
    fields_list = fields_list,
    codierung_all = codierung_all,
    column = "Code",
    language = "English"
  )

  # Check that original NC columns are preserved
  expect_true("NC_2020" %in% names(result[[1]]))
  expect_true("NC_2021" %in% names(result[[2]]))
  expect_equal(result[[1]]$NC_2020, fields_list[[1]]$NC_2020)
})

test_that("add_names handles missing NC codes gracefully", {
  fields_list <- create_test_fields()

  # Codierung with missing code
  codierung_incomplete <- data.frame(
    NC = c(110, 200),
    german_names = c("Weizen", "Mais"),
    english_names = c("Wheat", "Corn"),
    stringsAsFactors = FALSE
  )

  result <- add_names(
    fields_list = fields_list,
    codierung_all = codierung_incomplete,
    column = "Code",
    language = "English"
  )

  # Code 300 in 2021 should result in NA or "Unknown"
  expect_true(is.na(result[[2]]$Name_2021[1]) || result[[2]]$Name_2021[1] == "Unknown")
})

test_that("add_names works with direct crop names (non-Code column)", {
  # Create fields with direct names
  coords <- list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))
  poly <- sf::st_polygon(coords)

  field1 <- sf::st_sf(
    Crop_2020 = c("Wheat", "Corn"),
    geometry = sf::st_sfc(poly, poly),
    crs = 4326
  )

  fields_list <- list(`2020` = field1)
  codierung_all <- create_test_codierung()

  result <- add_names(
    fields_list = fields_list,
    codierung_all = codierung_all,
    column = "Crop",
    language = "English"
  )

  # Should have Name column
  expect_true("Name_2020" %in% names(result[[1]]))

  # Names should match original crop names
  expect_equal(result[[1]]$Name_2020, c("Wheat", "Corn"))
})

test_that("add_names returns list of same length as input", {
  fields_list <- create_test_fields()
  codierung_all <- create_test_codierung()

  result <- add_names(
    fields_list = fields_list,
    codierung_all = codierung_all,
    column = "Code",
    language = "English"
  )

  expect_equal(length(result), length(fields_list))
})

test_that("add_names preserves sf object class", {
  fields_list <- create_test_fields()
  codierung_all <- create_test_codierung()

  result <- add_names(
    fields_list = fields_list,
    codierung_all = codierung_all,
    column = "Code",
    language = "English"
  )

  # Check that each element is still an sf object
  expect_s3_class(result[[1]], "sf")
  expect_s3_class(result[[2]], "sf")
})

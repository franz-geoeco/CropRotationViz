# CropRotationViz Test Suite

This directory contains the test suite for the CropRotationViz package using the `testthat` framework.

## Structure

```
tests/
├── testthat/
│   ├── helper.R                        # Shared test utilities and helper functions
│   ├── test-add-names.R               # Tests for add_names() function
│   ├── test-aggregator.R              # Tests for aggregator() function
│   ├── test-app-launchers.R           # Tests for Shiny app launcher functions
│   ├── test-diversity-functions.R     # Tests for diversity analysis functions
│   ├── test-dummy-field-creator.R     # Tests for dummy_field_creator() function
│   ├── test-spatial-processing.R      # Tests for spatial intersection functions
│   ├── test-transform-functions.R     # Tests for data transformation functions
│   └── test-visualization-functions.R # Tests for visualization creation functions
└── testthat.R                         # Test runner configuration
```

## Running Tests

### Run all tests

```r
# From R console
devtools::test()

# Or using testthat directly
testthat::test_dir("tests/testthat")
```

### Run specific test file

```r
testthat::test_file("tests/testthat/test-add-names.R")
```

### Run tests with coverage

```r
covr::package_coverage()
covr::report()
```

## Test Coverage by Function Category

### ✅ Data Generation & Utilities (1 function)
- `dummy_field_creator()` - Full coverage with edge cases

### ✅ Spatial Processing (4 functions)
- `add_names()` - Full coverage including language options, missing codes
- `intersect_fields()` - Core functionality, parallel processing, tiling
- `intersect_fields_simple()` - Basic intersection, multi-year support
- `intersect_with_borders()` - Requires external data (skipped in automated tests)

### ✅ Data Aggregation (1 function)
- `aggregator()` - NC codes, crop names, classification logic

### ✅ Data Transformation (3 functions)
- `transform_rotation_data()` - Area filtering, crop selection, type variants
- `transform_rotation_summary()` - Summary tables, max rows, year columns
- `process_specific_transitions()` - Transition analysis, percentages

### ✅ Visualization Creation (2 functions)
- `create_crop_rotation_sankey()` - Sankey diagrams, exclusions, colors, file output
- `create_multi_year_donut()` - Donut charts, highlighting, year limits

### ✅ Diversity Analysis (3 functions)
- `diversity_mapping()` - District/EZG/AOI analysis, metrics calculation
- `diversity_mapper()` - Leaflet maps, bivariate visualization
- `diversity_soil_plotter()` - Boxplots, soil-diversity relationships

### ✅ App Launchers (4 functions)
- `run_processing_app()` - Parameter validation, basic initialization
- `run_visualization_app()` - Parameter validation, basic initialization
- `run_fast_visualization_app()` - Parameter validation, basic initialization
- `run_field_level_app()` - Basic initialization

### ⚠️ UI/Server Functions (4 functions - Limited Testing)
- `viz_ui()` - Basic structure (requires Shiny interaction for full testing)
- `viz_server()` - Basic structure (requires Shiny interaction for full testing)
- `fast_viz_ui()` - Basic structure (requires Shiny interaction for full testing)
- `fast_viz_server()` - Basic structure (requires Shiny interaction for full testing)

**Note:** Full testing of Shiny UI/Server functions requires integration testing with tools like `shinytest2`. The current tests verify basic initialization and parameter handling.

## Test Patterns Used

### 1. Unit Tests
Individual function testing with controlled inputs and expected outputs.

### 2. Integration Tests
Testing workflows that combine multiple functions (e.g., intersect → aggregate).

### 3. Edge Case Testing
- Empty inputs
- Single element inputs
- Missing values (NA)
- Boundary conditions (min/max areas)
- Invalid parameters

### 4. CRAN-Safe Testing
Tests marked with `skip_on_cran()` for resource-intensive operations:
- File I/O operations
- Large spatial computations
- Plot generation
- External data dependencies

## Test Data Helpers

The `helper.R` file provides utilities for creating test data:

- `create_simple_polygon()` - Generate test sf polygons
- `create_test_fields()` - Generate field data with crops
- `create_crop_codierung()` - Create crop code lookup tables
- `create_crop_classification()` - Create crop groupings
- `create_rotation_data()` - Generate rotation sequences
- `get_temp_test_dir()` - Create temporary test directories
- `cleanup_test_files()` - Clean up test artifacts

## Coverage Goals

- **Target:** >75% code coverage
- **Priority:** All exported functions have basic tests
- **Current Status:** Core functions fully tested, Shiny components partially tested

## Known Limitations

1. **Shiny App Testing:** Full interactive testing requires `shinytest2` (future enhancement)
2. **External Data:** Some tests (e.g., `intersect_with_borders()`) require external boundary data files
3. **Parallel Processing:** Tests use single core (`n_cores = 1`) for consistency
4. **Visual Output:** Visualization functions create plots but don't verify visual appearance

## Adding New Tests

When adding new functionality:

1. Create new test file: `test-your-function-name.R`
2. Follow naming convention: `test_that("function does what it should", { ... })`
3. Use helper functions from `helper.R` for test data
4. Mark resource-intensive tests with `skip_on_cran()`
5. Document any external dependencies with `skip_if_not_installed()`

## Continuous Integration

Tests are run automatically on:
- Local development: `devtools::test()`
- GitHub Actions: R CMD check workflow
- CRAN submission: Automated checks

## References

- [testthat documentation](https://testthat.r-lib.org/)
- [R Packages: Testing](https://r-pkgs.org/testing-basics.html)
- [Writing Tests (Hadley Wickham)](https://r-pkgs.org/testing-design.html)

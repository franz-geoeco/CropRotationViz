#' Crop Rotation Application Input Data
#'
#' A comprehensive dataset containing crop codes, color mappings, and aggregated statistics
#' for the CropRotationViz package. This dataset includes detailed information about
#' different crop types, their classifications, reference codes, and visualization parameters.
#'
#' @format A list containing several components for crop rotation visualization:
#' \describe{
#'   \item{codierung_all}{A data frame with crop codes:
#'     \itemize{
#'       \item{NC}{Numeric crop code for unique identification}
#'       \item{Klarschrift}{Crop name in clear text, original designation}
#'     }
#'   }
#'   \item{crop_codes}{List of crop code mappings:
#'     \itemize{
#'       \item Standard crop codes and their mappings
#'       \item Hierarchical structure of crop classifications
#'       \item Cross-references between different coding systems
#'     }
#'   }
#'   \item{display_names}{Named vector of display names:
#'     \itemize{
#'       \item Human-readable crop names for visualization
#'       \item Standardized naming conventions
#'       \item Multilingual support where applicable
#'     }
#'   }
#'   \item{crop_color_mapping}{Named character vector mapping crop types to hex color codes:
#'     \itemize{
#'       \item Names: Crop type identifiers (e.g., "winter oil-plant", "winter durum")
#'       \item Values: Hex color codes (e.g., "#2e8b57", "#556b2f")
#'       \item Consistent color scheme for visualization
#'     }
#'   }
#'   \item{all_wrap_number_count_small}{Data frame with crop aggregation information:
#'     \itemize{
#'       \item{Name_2023}{Character: Original crop name}
#'       \item{Aggregated_2023}{Character: Aggregated crop category}
#'       \item{count}{Integer: Category identifier}
#'       \item{number}{Integer: Count of occurrences}
#'     }
#'   }
#'   \item{loaded_env}{Environment object containing loaded runtime data}
#'   \item{EZG}{RIver Basins from Germany (Bundesanstalt für Gewässerkunde)}
#' }
#'
#' @section Crop Categories and Classification:
#' The dataset organizes crops into multiple hierarchical levels:
#' \itemize{
#'   \item Primary Categories:
#'     \itemize{
#'       \item Cereals (winter wheat, winter rye, winter barley, etc.)
#'       \item Oilseeds (winter oil-plant, summer oil-plant)
#'       \item Legumes (protein plants, clover/lutzerne)
#'       \item Root crops (potatoes, sugar beet)
#'     }
#'   \item Secondary Categories:
#'     \itemize{
#'       \item Grassland and forage
#'       \item Special crops (vegetables, fruits, ornamental plants)
#'       \item Fallow and conservation areas
#'     }
#'   \item Special Classifications:
#'     \itemize{
#'       \item Seasonal variants (winter/summer crops)
#'       \item Land use types (agricultural/conservation)
#'       \item Management intensity levels
#'     }
#' }
#'
#' @section Color Mapping System:
#' The visualization color scheme follows these principles:
#' \itemize{
#'   \item Winter Cereals: Dark shades (#556b2f, #7f0000)
#'   \item Summer Variants: Lighter complementary colors (#bdb76b, #4682b4)
#'   \item Grassland: Natural green tones (#2f4f4f)
#'   \item Special Crops: Distinct, vibrant colors
#'   \item Conservation Areas: Earth tones
#'   \item Consistent color families for related crops
#' }
#'
#' @section Data Aggregation Levels:
#' The data supports multiple aggregation levels:
#' \itemize{
#'   \item Individual Crop Level:
#'     \itemize{
#'       \item Specific varieties and cultivars
#'       \item Seasonal variants
#'       \item Management practices
#'     }
#'   \item Group Level:
#'     \itemize{
#'       \item Crop families
#'       \item Usage categories
#'       \item Growing seasons
#'     }
#'   \item Administrative Level:
#'     \itemize{
#'       \item Land use categories
#'       \item Policy relevant groupings
#'       \item Statistical reporting units
#'     }
#' }
#'
#' @section Usage Notes:
#' \itemize{
#'   \item Crop codes (NC) should be used as primary keys for database operations
#'   \item Display names are optimized for visualization and user interfaces
#'   \item Color mappings are designed for maximum differentiation in plots
#'   \item Aggregation levels support various analysis scenarios
#' }
#'
#' @source 
#' Data compiled from:
#' \itemize{
#'   \item Agricultural land use surveys
#'   \item Standardized crop classification systems
#'   \item Agricultural monitoring and reporting systems
#'   \item Expert-developed visualization schemes
#' }
#'
#' @examples
#' \dontrun{
#' # Access basic crop information
#' head(Input_App_data$codierung_all)
#'
#' # Get display name for a crop
#' Input_App_data$display_names["wheat"]
#'
#' # Access color mapping
#' Input_App_data$crop_color_mapping["winter wheat"]
#'
#' # Get aggregated statistics
#' subset(Input_App_data$all_wrap_number_count_small, 
#'        Name_2023 == "winter wheat")
#' }
#'
#' @references
#' \itemize{
#'   \item Agricultural land use classification system
#'   \item Standard color coding schemes for crop visualization
#'   \item International crop coding standards
#'   \item Agricultural reporting guidelines
#' }
#'
#' @keywords datasets agriculture visualization classification
#'
#' @usage data(Input_App_data)
"Input_App_data"
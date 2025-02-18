#' Filter Crop Classification Based on Available Codes
#' 
#' @description 
#' Takes a list of initial crop classifications and filters them based on the codes 
#' actually present in the processed spatial files. This ensures that only relevant
#' crop classes are maintained in the classification system.
#' 
#' @param initial_classes List of character vectors where each vector contains crop names
#'   belonging to a specific classification category
#' @param processed_files List of processed spatial files, where each file contains:
#'   \itemize{
#'     \item sf_object: Simple features object containing the spatial data
#'     \item selected_column: Character string indicating the column containing crop codes
#'   }
#' @param codierung_all Data frame mapping between numeric crop codes (NC) and clear text
#'   names (english_names)
#' 
#' @return List of filtered classification categories containing only crop names that
#'   exist in the processed files. Empty categories are removed.
#' 
#' @details 
#' The function performs several steps:
#' 1. Extracts all unique crop codes from the processed spatial files
#' 2. For each classification category:
#'    - Converts crop names to their corresponding numeric codes
#'    - Keeps only codes that exist in the processed files
#'    - Converts filtered codes back to crop names
#' 3. Removes any empty classification categories
#' 
#' @examples
#' \dontrun{
#' # Example initial classes
#' initial_classes <- list(
#'   cereals = c("winter wheat", "winter barley", "rye"),
#'   oilseeds = c("rapeseed", "sunflower")
#' )
#' 
#' # Filter based on available data
#' filtered_classes <- filter_initial_classes(
#'   initial_classes = initial_classes,
#'   processed_files = my_processed_files,
#'   codierung_all = crop_codes_df
#' )
#' }
#' 
#' @importFrom stats complete.cases
#' @keywords internal
filter_initial_classes <- function(initial_classes, processed_files, codierung_all) {
  # Helper function to get codes from names
  get_codes <- function(names, codierung_all) {
    if(input$language == "English"){
      codes <- codierung_all$NC[match(names, codierung_all$english_names)]
    }else{
      codes <- codierung_all$NC[match(names, codierung_all$german_names)]
    }
    return(codes[!is.na(codes)])
  }
  
  # Get all unique codes from the processed files
  unique_file_codes <- unique(unlist(lapply(processed_files, function(file) {
    if (!is.null(file)) {
      col_data <- file$sf_object[[file$selected_column]]
      if (is.character(col_data)) {
        col_data <- as.numeric(col_data)
      }
      return(unique(col_data))
    }
    return(NULL)
  })))
  
  unique_file_codes <- unique_file_codes[!is.na(unique_file_codes)]
  
  # Filter each class
  filtered_classes <- lapply(initial_classes, function(class_names) {
    class_codes <- get_codes(class_names, codierung_all)
    existing_codes <- class_codes[class_codes %in% unique_file_codes]
    if(input$language == "English"){
      existing_names <- codierung_all$english_names[match(existing_codes, codierung_all$NC)]
    }else{
      existing_names <- codierung_all$german_names[match(existing_codes, codierung_all$NC)]
    }
    return(existing_names[!is.na(existing_names)])
  })
  
  # Remove empty classes
  filtered_classes <- filtered_classes[sapply(filtered_classes, length) > 0]
  return(filtered_classes)
}

#-------------------------------------------------------------------------------------------

#' Extract Unique Crop Names from Spatial Files
#' 
#' @description 
#' Extracts all unique crop names from a collection of processed spatial files by 
#' converting numeric codes to their corresponding clear text names.
#' 
#' @param processed_files List of processed spatial files, where each file contains:
#'   \itemize{
#'     \item sf_object: Simple features object containing the spatial data
#'     \item selected_column: Character string indicating the column containing crop codes
#'   }
#' @param codierung_all Data frame mapping between numeric crop codes (NC) and clear text
#'   names (english_names)
#' 
#' @return Character vector of unique crop names present in the processed files, with
#'   NA values and invalid codes removed.
#' 
#' @examples
#' \dontrun{
#' # Get all unique crops from processed files
#' unique_crops <- get_all_crops(
#'   processed_files = my_processed_files,
#'   codierung_all = crop_codes_df
#' )
#' }
#' 
#' @keywords internal
get_all_crops <- function(processed_files, codierung_all, language) {
  # Get all unique values from files
  all_values <- unique(unlist(lapply(processed_files, function(file) {
    if (!is.null(file)) {
      col_data <- file$sf_object[[file$selected_column]]
      return(unique(col_data))
    }
    return(NULL)
  })))
  
  all_values <- all_values[!is.na(all_values)]
  
  # Check if we're dealing with codes or names
  is_numeric <- all(suppressWarnings(!is.na(as.numeric(all_values))))
  
  if (is_numeric) {
    # Convert numeric codes to names
    all_values <- as.numeric(all_values)
    if(language == "English"){
      crop_names <- codierung_all$english_names[match(all_values, codierung_all$NC)]
    }else{
      crop_names <- codierung_all$german_names[match(all_values, codierung_all$NC)]
    }
    return(crop_names[!is.na(crop_names)])
  } else {
    # If we're dealing with names
    existing_names <- all_values
    return(existing_names)
  }
}

#-------------------------------------------------------------------------------------------

#' Add Names to Fields
#' 
#' This function processes a list of spatial field data and adds names to the fields
#' based on either numeric codes or existing crop names. It can handle both English
#' and German language options when working with coded data.
#' 
#' @param fields_list A list where each element contains spatial field data with the
#'   following structure:
#'   \itemize{
#'     \item sf_object: An sf object containing the field geometries
#'     \item selected_column: Name of the column containing codes or crop names
#'     \item selected_year: The year associated with the data
#'   }
#' @param codierung_all A data frame containing the coding reference table with columns:
#'   \itemize{
#'     \item NC: Numeric codes
#'     \item german_names: German crop names
#'     \item english_names: English crop names
#'   }
#' @param column Character string specifying the type of input data. Must be either
#'   "Code" for numeric codes or any other value for direct crop names.
#' @param language Character string specifying the desired output language.
#'   Must be either "English" or "German". Only used when column = "Code".
#' 
#' @return A list of sf objects where each element contains:
#'   \itemize{
#'     \item Geometry column named "geometry"
#'     \item For coded data (column = "Code"):
#'       \itemize{
#'         \item NC_{year}: Original numeric codes
#'         \item Name_{year}: Crop names in specified language
#'       }
#'     \item For direct names:
#'       \itemize{
#'         \item Name_{year}: Original crop names
#'       }
#'   }
#' 
#' @details
#' When processing coded data (column = "Code"), the function:
#' \itemize{
#'   \item Filters out codes less than 100
#'   \item Merges with the coding reference table
#'   \item Selects the appropriate language names
#' }
#' For direct names, it simply renames the crop name column to include the year.
#' 
#' @note
#' The function assumes that the input sf objects have valid geometries and that
#' the coding reference table contains all necessary codes when working with coded data.
#' 
#' @examples
#' \dontrun{
#' # Example with coded data
#' fields <- list(
#'   list(
#'     sf_object = sf::st_read("fields_2020.shp"),
#'     selected_column = "crop_code",
#'     selected_year = "2020"
#'   )
#' )
#' 
#' codes <- data.frame(
#'   NC = c(101, 102),
#'   german_names = c("Weizen", "Gerste"),
#'   english_names = c("Wheat", "Barley")
#' )
#' 
#' # Process with German names
#' result_de <- add_names(fields, codes, "Code", "German")
#' 
#' # Process with English names
#' result_en <- add_names(fields, codes, "Code", "English")
#' 
#' # Example with direct crop names
#' fields_named <- list(
#'   list(
#'     sf_object = sf::st_read("crops_2020.shp"),
#'     selected_column = "crop_name",
#'     selected_year = "2020"
#'   )
#' )
#' 
#' result_direct <- add_names(fields_named, NULL, "Name", NULL)
#' }
#' 
#' @importFrom sf st_read st_transform st_geometry
#' @importFrom dplyr filter mutate select
#' 
#' @export
add_names <- function(fields_list, codierung_all, column, language){
  
  if(column == "Code"){
    fields_list <- lapply(fields_list, function(f) {
      file <- f$sf_object
      file <- file[,c(f$selected_column, attr(file, "sf_column"))]
      st_geometry(file) <- "geometry"
      names(file) <- c("NC", "geometry")
      file$NC <- as.numeric(file$NC)
      file <- subset(file, NC >= 100)
      
      # add the names
      file <- merge(file, codierung_all)
      
      names(file)[names(file) == "NC"] <- paste0("NC_", f$selected_year)

      if(language == "English"){
        names(file)[names(file) == "english_names"] <- paste0("Name_", f$selected_year)
        file <- file[, !colnames(file) %in% "german_names"]
      }else{
        names(file)[names(file) == "german_names"] <- paste0("Name_", f$selected_year)
        file <- file[, !colnames(file) %in% "english_names"]
      } 
      return(file)
    })
  }else{
    fields_list <- lapply(fields_list, function(f) {
      file <- f$sf_object
      file <- file[,c(f$selected_column, attr(file, "sf_column"))]
      st_geometry(file) <- "geometry"
      names(file) <- c("Crop", "geometry")
      names(file)[names(file) == "Crop"] <- paste0("Name_", f$selected_year)
      return(file)
    })
  }
  
  return(fields_list)
}

#-------------------------------------------------------------------------------------------

#' Clean Thin Polygons
#' 
#' @param sf_data SF object containing polygons
#' @param min_width Minimum width in meters
#' @return SF object with cleaned polygons
#' @importFrom sf st_buffer st_is_empty
#' @importFrom units set_units
clean_thin_polygons <- function(sf_data, min_width = 1) {
  # Convert min_width to units object in meters
  min_width <- set_units(min_width, "m")
  
  # Buffer inward then outward to remove thin features
  cleaned <- sf_data %>%
    st_buffer(-min_width/2) %>%  # Negative buffer (shrink)
    st_buffer(min_width/2)       # Positive buffer (expand)
  
  # Remove any empty geometries that resulted from the process
  cleaned <- cleaned[!st_is_empty(cleaned), ]
  
  return(cleaned)
}

#-------------------------------------------------------------------------------------------

#' Group Overlapping Polygons
#' 
#' @description Assigns group IDs to overlapping polygons in a spatial dataset
#' 
#' @param polygons_sf sf object containing polygons to be grouped
#' @return sf object with added group_id column
#' @importFrom sf st_intersects
group_overlapping_polygons <- function(polygons_sf) {
  # Create an empty list to store groups
  groups <- list()
  group_counter <- 0
  # Create a matrix of intersections
  intersects_matrix <- st_intersects(polygons_sf)
  
  # Keep track of assigned polygons
  assigned <- rep(FALSE, length(intersects_matrix))
  # Iterate through each polygon
  for(i in 1:length(intersects_matrix)) {
    if(!assigned[i]) {
      group_counter <- group_counter + 1
      
      # Get all polygons that intersect with current polygon
      current_group <- unique(unlist(intersects_matrix[i]))
      
      # Mark these polygons as assigned
      assigned[current_group] <- TRUE
      
      # Store the group
      groups[[group_counter]] <- current_group
    }
  }
  # Create a vector to store group numbers
  group_numbers <- rep(NA, nrow(polygons_sf))
  # Assign group numbers
  for(i in 1:length(groups)) {
    group_numbers[groups[[i]]] <- i
  }
  # Add the group numbers to the original sf object
  polygons_sf$group_id <- group_numbers
  return(polygons_sf)
}

#-------------------------------------------------------------------------------------------

#' Check Spatial Intersections
#' 
#' @description Performs spatial intersection checking using a tiled approach for large datasets
#' 
#' @param input_list List of sf objects to check
#' @param intersection sf object representing the intersection
#' @return sf object with intersection results
#' @importFrom sf st_bbox st_crop st_make_valid st_drop_geometry st_union
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom dplyr %>% group_by summarize across select
#' @importFrom rmapshaper ms_erase
intersecting_check_spatial <- function(input_list, intersection) {
  # Get the overall bounding box
  bbox <- st_bbox(input_list[[1]])
  
  # Calculate number of tiles needed in each direction
  x_tiles <- ceiling((bbox["xmax"] - bbox["xmin"]) / 10000)
  y_tiles <- ceiling((bbox["ymax"] - bbox["ymin"]) / 10000)
  
  # Pre-allocate list to store results
  results_list <- list()
  counter <- 1
  
  # Initialize progress bar
  total_tiles <- x_tiles * y_tiles
  pb <- txtProgressBar(min = 0, max = total_tiles, style = 3)
  
  # Loop through tiles
  for(x in 1:x_tiles) {
    for(y in 1:y_tiles) {
      setTxtProgressBar(pb, counter)
      
      incProgress(0.001, detail = paste("checking intersection", counter, "from", total_tiles))
      
      # Calculate tile bounds
      tile_xmin <- bbox["xmin"] + (x-1) * 10000
      tile_xmax <- bbox["xmin"] + x * 10000
      tile_ymin <- bbox["ymin"] + (y-1) * 10000
      tile_ymax <- bbox["ymin"] + y * 10000
      
      # Create tile polygon
      tile <- c(xmin = tile_xmin, 
                xmax = tile_xmax, 
                ymin = tile_ymin, 
                ymax = tile_ymax)
      
      names(tile) <- c("xmin", "xmax", "ymin", "ymax")
      
      counter2 <- 3
      
      intersection_subset <- sf::st_crop(intersection, tile)

      for(i in 1:length(input_list)){
        input <- input_list[[i]]
        
        # Find features that intersect with this tile
        input_subset <- st_crop(input, tile)
        
        # Only process if we have features in both datasets
        if(nrow(input_subset) > 0 && nrow(intersection_subset) > 0) {
          tryCatch({
            erase_result <- sf::st_make_valid(ms_erase(input_subset, intersection_subset))
            erase_result <- clean_thin_polygons(erase_result, 2)

            if(!is.null(erase_result) && nrow(erase_result) > 0) {
              if(i == 1){
                df_missing <- intersection[1:nrow(erase_result),]
                df_missing[,-c((i),(i+1), ncol(df_missing))] <- "Not named"
                df_missing[,c((i),(i+1))] <- st_drop_geometry(erase_result)
                df_missing$geometry <- erase_result$geometry
                
                results_list[[counter]] <- df_missing
  
                # add an intersecting group id 
                results_list[[counter]] <- results_list[[counter]]%>%
                  group_overlapping_polygons()%>% # add intersecting id 
                  group_by(group_id) %>%
                  summarize(
                    across(where(~!inherits(., "sfc")), ~na.omit(.)[1]),  # Take first non-NA value
                    geometry = st_union(geometry),
                    .groups = "drop"
                  ) %>%
                  select(-group_id)
                
              }else{
                df_missing <- intersection[1:nrow(erase_result),]
                df_missing[,-c((counter2),(counter2+1), ncol(df_missing))] <- "Not named"
                df_missing[,c((counter2),(counter2+1))] <- st_drop_geometry(erase_result)
                df_missing$geometry <- erase_result$geometry
                
                results_list[[counter]] <- rbind(results_list[[counter]], df_missing)
                
                counter2 <- counter2+2
                
                # add an intersecting group id 
                results_list[[counter]] <- results_list[[counter]]%>%
                  group_overlapping_polygons()%>% # add intersecting id 
                  group_by(group_id) %>%
                  summarize(
                    across(where(~!inherits(., "sfc")), ~na.omit(.)[1]),  # Take first non-NA value
                    geometry = st_union(geometry),
                    .groups = "drop"
                  ) %>%
                  select(-group_id)
              }
            }
          }, error = function(e) {
            warning(paste("Error processing tile", x, y, ":", e$message))
          })
        }
      }
      counter <- counter + 1
    }
  }
  
  # Close progress bar
  close(pb)
  
  # Combine results
  erase <- do.call(rbind, results_list)
  
  return(erase)
}

#-------------------------------------------------------------------------------------------

#' Intersect Multiple Field Layers
#' 
#' @description Intersects multiple spatial layers while maintaining CRS consistency
#' 
#' @param fields_list List of sf objects to intersect
#' @return sf object with intersected fields
#' @importFrom sf st_crs st_transform st_intersection st_make_valid st_geometry_type st_area
#' @importFrom dplyr %>% filter
#' @importFrom units set_units
#' @export
intersect_fields <- function(fields_list, max_area = 20000 * 1e6) {
  # Ensure at least two fields are provided
  if (length(fields_list) < 2) {
    stop("Please provide at least two spatial layers in fields_list.")
  }
  
  message("Starting intersection process...")
  
  # Get the CRS of the first layer to use for all transformations
  target_crs <- st_crs(fields_list[[1]])
  
  # Transform all fields to the CRS of the first layer
  message("Transforming CRS...")
  fields_list <- lapply(fields_list, st_transform, crs = target_crs)
  
  # Initialize with first layer
  intersected <- fields_list[[1]]
  
  # Check if tiling is needed
  total_area <- as.numeric(sum(st_area(intersected)))
  if (total_area > max_area) {
    message(sprintf("Large area detected (%.2f km²). Using tiling approach...", 
                    as.numeric(total_area)/1e6))
    
    # Create a grid of tiles based on the bounding box
    bbox <- st_bbox(intersected)
    # Calculate number of tiles needed (square root for roughly square tiles)
    n_tiles <- ceiling(sqrt(total_area / (max_area/2)))
    
    message(sprintf("Creating %d x %d grid...", n_tiles, n_tiles))
    # Create grid
    grid <- st_make_grid(intersected, n = n_tiles)
  
    # Initialize list to store results
    results <- list()
    
    # Create progress bar for tiles
    pb <- txtProgressBar(min = 0, max = length(grid), style = 3)
    
    # Process each tile
    for (tile_idx in seq_along(grid)) {
      tile <- grid[[tile_idx]]
      # Clip first layer with tile
      tile_intersect <- st_crop(intersected, tile)
      if (length(tile_intersect) > 0) {
        # Process remaining layers
        for (i in 2:length(fields_list)) {
          # Clip current field to tile
          field_clipped <- st_crop(st_make_valid(fields_list[[i]]), tile)
          if (length(field_clipped) > 0) {
            # Perform intersection
            tile_intersect <- st_intersection(sf::st_make_valid(tile_intersect),
                                              sf::st_make_valid(field_clipped))
            
            # Filter valid geometries
            tile_intersect <- tile_intersect %>%
              filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
              st_make_valid() %>%
              filter(st_area(.) >= units::set_units(1, "m^2"))
          }
        }
        
        # Add results from this tile
        if (length(tile_intersect) > 0) {
          results[[length(results) + 1]] <- tile_intersect
        }
      }
      
      # Update progress bar
      setTxtProgressBar(pb, tile_idx)
    }
    
    # Close progress bar
    close(pb)
    
    message("\nCombining results from tiles...")
    # Combine results
    if (length(results) > 0) {
      intersected <- do.call(rbind, results) %>%
        st_make_valid()
    } else {
      intersected <- NULL
    }
    
  } else {
    message("Processing Intersection without tiling...")
    # Create progress bar for normal processing
    pb <- txtProgressBar(min = 0, max = length(fields_list) - 1, style = 3)
    
    # Process normally if area is small enough
    for (i in 2:length(fields_list)) {
      intersected <- st_intersection(sf::st_make_valid(intersected), sf::st_make_valid(fields_list[[i]]))
      
      incProgress(0.0005, detail = paste("intersecting", i, "from", length(fields_list)))
      
      intersected <- intersected %>%
        filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
        st_make_valid() %>%
        filter(st_area(.) >= units::set_units(1, "m^2"))
      
      # Update progress bar
      setTxtProgressBar(pb, i - 1)
    }
    
    # Close progress bar
    close(pb)
  }
  
  message("\nChecking for non-intersecting polygons...")
  # bind the intersected with the non intersecting polygons
  if (!is.null(intersected)) {
    intersected <- rbind(intersected, intersecting_check_spatial(fields_list, intersected))
  }
  
  message("Intersection process completed!")
  return(intersected)
}

#-------------------------------------------------------------------------------------------

#' Simple Field Intersection
#' 
#' @description Performs a simplified intersection of multiple spatial layers
#' 
#' @param fields_list List of sf objects to intersect
#' @return sf object with intersected fields
#' @importFrom sf st_crs st_transform st_intersection st_make_valid st_geometry_type st_area
#' @importFrom dplyr %>% filter
#' @importFrom units set_units
#' @export

intersect_fields_simple <- function(fields_list, max_area = 20000 * 1e6) {
  # Ensure at least two fields are provided
  if (length(fields_list) < 2) {
    stop("Please provide at least two spatial layers in fields_list.")
  }
  
  message("Starting intersection process...")
  
  # Get the CRS of the first layer to use for all transformations
  target_crs <- st_crs(fields_list[[1]])
  
  # Transform all fields to the CRS of the first layer
  message("Transforming CRS...")
  fields_list <- lapply(fields_list, st_transform, crs = target_crs)
  
  # Initialize with first layer
  intersected <- fields_list[[1]]
  
  # Check if tiling is needed
  total_area <- as.numeric(sum(st_area(intersected)))
  if (total_area > max_area) {
    message(sprintf("Large area detected (%.2f km²). Using tiling approach...", 
                    as.numeric(total_area)/1e6))
    
    # Create a grid of tiles based on the bounding box
    bbox <- st_bbox(intersected)
    # Calculate number of tiles needed (square root for roughly square tiles)
    n_tiles <- ceiling(sqrt(total_area / (max_area/2)))
    
    message(sprintf("Creating %d x %d grid...", n_tiles, n_tiles))
    # Create grid
    grid <- st_make_grid(intersected, n = n_tiles)
    
    # Initialize list to store results
    results <- list()
    
    # Create progress bar for tiles
    pb <- txtProgressBar(min = 0, max = length(grid), style = 3)
    
    # Process each tile
    for (tile_idx in seq_along(grid)) {
      tile <- grid[[tile_idx]]
      # Clip first layer with tile
      tile_intersect <- st_crop(intersected, tile)
      
      if (length(tile_intersect) > 0) {
        # Process remaining layers
        for (i in 2:length(fields_list)) {
          # Clip current field to tile
          field_clipped <- st_intersection(sf::st_make_valid(fields_list[[i]]),
                                           sf::st_make_valid(tile))
          
          if (length(field_clipped) > 0) {
            # Perform intersection
            tile_intersect <- st_intersection(sf::st_make_valid(tile_intersect),
                                              sf::st_make_valid(field_clipped))
            
            # Filter valid geometries
            tile_intersect <- tile_intersect %>%
              filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
              st_make_valid() %>%
              filter(st_area(.) >= units::set_units(1, "m^2"))
          }
        }
        
        # Add results from this tile
        if (length(tile_intersect) > 0) {
          results[[length(results) + 1]] <- tile_intersect
        }
      }
      
      # Update progress bar
      setTxtProgressBar(pb, tile_idx)
    }
    
    # Close progress bar
    close(pb)
    
    message("\nCombining results from tiles...")
    # Combine results
    if (length(results) > 0) {
      intersected <- do.call(rbind, results) %>%
        st_make_valid()
    } else {
      intersected <- NULL
    }
    
  } else {
    message("Processing without tiling...")
    # Create progress bar for normal processing
    pb <- txtProgressBar(min = 0, max = length(fields_list) - 1, style = 3)
    
    # Process normally if area is small enough
    for (i in 2:length(fields_list)) {
      
      incProgress(0.005, detail = paste("intersecting", i, "from", length(fields_list)))
      
      intersected <- st_intersection(intersected, sf::st_make_valid(fields_list[[i]]))
      
      intersected <- intersected %>%
        filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
        st_make_valid() %>%
        filter(st_area(.) >= units::set_units(1, "m^2"))
      
      # Update progress bar
      setTxtProgressBar(pb, i - 1)
    }
    
    # Close progress bar
    close(pb)
  }
  message("Intersection process completed!")
  return(intersected)
}

#-------------------------------------------------------------------------------------------

#' Intersect Geometries with Administrative Borders
#' 
#' @description 
#' Performs spatial intersection between input geometries and administrative borders 
#' at a specified level. The function first creates a central point from the input 
#' geometry's bounding box, identifies the corresponding country, and then intersects 
#' the input with administrative boundaries of that country.
#' 
#' @param input An sf object containing the input geometries to be intersected
#' @param level Numeric value specifying the administrative level for intersection 
#'             (e.g., 1 for states/provinces, 2 for counties/districts)
#'@param countriesSP SpatialPolygonsDataFrame object containing world map boundaries
#'@param EZG SpatialPolygonsDataFrame object containing German river catchments
#'@param aoi SpatialPolygonsDataFrame object containing the areas of interest
#' 
#' @return An sf object containing the intersected geometries
#' 
#' @details 
#' The function follows these steps:
#' 1. Creates a bounding box and center point from input geometry
#' 2. Identifies the country using the center point
#' 3. Retrieves administrative boundaries for the identified country
#' 4. Performs spatial intersection
#' 
#' Special handling is implemented for Germany (DE) where it uses a predefined 
#' watershed boundary layer (EZG).
#'
#' @examples
#' \dontrun{
#' # Create sample input geometry
#' input_geom <- st_read("input.shp")
#' 
#' # Intersect with level 1 administrative boundaries
#' result <- intersect_with_borders(input_geom, level = 1)
#' }
#' 
#' @export
intersect_with_borders <- function(input, level, countriesSP, EZG, aoi) {
  # Create bounding box and center point
  point <- sf::st_bbox(input)
  point <- sf::st_as_sf(data.frame(
    lon = mean(c(point[1], point[3])), 
    lat = mean(c(point[2], point[4]))
  ), coords = c("lon", "lat"))
  sf::st_crs(point) <- sf::st_crs(input)

  # Convert sf point to sp
  pointsSP <- as(sf::st_transform(point, crs = sf::st_crs(countriesSP)), "Spatial")
  
  # Now over() should work as both objects are sp class
  indices <- sp::over(pointsSP, countriesSP)
  
  # Get country code and borders
  cc <- subset(geodata::country_codes(), NAME == as.character(indices$ADMIN))
  borders <- geodata::gadm(country = cc$ISO2, level = level, path = tempdir(), resolution = 2)
  
  borders <- borders[paste0("NAME_", level)]
  borders <- sf::st_as_sf(borders)
  borders <- sf::st_transform(borders, crs = sf::st_crs(input))  
  borders_inter <- borders[apply(st_intersects(borders, input, sparse = FALSE), 1, any), ]
  
  
  # intersection with administrative borders
  intersected <- sf::st_intersection(input, borders_inter)
  names(intersected)[length(names(intersected))-1] <- "District"
  
  # delete to small intersection areas
  intersected <- intersected %>%
    st_make_valid() %>%
    mutate(area = as.numeric(st_area(geometry))) %>%
    group_by(District) %>%
    filter(sum(area) >= 20000) %>%
    ungroup()
  
  # if it`s in Germany intersect with river catchments
  if(cc$ISO2 == "DE"){
    intersected <- sf::st_intersection(intersected, sf::st_transform(EZG, crs = sf::st_crs(input)))
    EZG_inter <- subset(EZG, EZG %in% unique(st_drop_geometry(intersected)$EZG))
    EZG_inter <- st_transform(EZG_inter, crs = sf::st_crs(input))
    EZG_inter <- sf::st_intersection(EZG_inter, borders_inter)
    EZG_inter <- EZG_inter[,-2] %>%
      group_by(EZG) %>%
      summarise(
        geometry = st_union(geometry)
      )
    # Prepare the output list
    out_list <- list(intersected = intersected, borders_inter = borders_inter, EZG_inter = EZG_inter)
  } else {
    # If no intersection, keep intersected as is and notify
    out_list <- list(intersected = intersected, borders_inter = borders_inter)
    message("No intersection found between German river basins and intersected features")
  }
  
  if(!is.null(aoi)){
    names(aoi) <- c("AOI", "geometry")
    intersected <- sf::st_intersection(intersected, sf::st_transform(aoi, crs = sf::st_crs(input)))
    out_list$intersected <- intersected
    # Prepare the output list with aoi
    out_list <- c(out_list,  list(AOI_inter = intersected))
  }
  return(out_list)
}

#-------------------------------------------------------------------------------------------

#' Aggregate Field Data
#' 
#' @description Aggregates field data by adding classified crop names for each year
#' 
#' @param intersected sf object containing intersected fields
#' @param years Numeric vector of years to process
#' @param crop_codes crop codes for aggregaton
#' @param display_names names corresponding to the codes
#' @return sf object with added aggregated columns
#' @export
aggregator <- function(intersected, years, crop_codes, type = "NC"){
  for(year in years) {
    nc_col <- paste0("NC_", year)
    name_col <- paste0("Name_", year)
    agg_col <- paste0("Aggregated_", year)
    
    if(type == "NC"){
      intersected[[agg_col]] <- sapply(intersected[[nc_col]], function(x) {
        if(x == "Not named") return("Not named")
        for(class_name in names(crop_codes)) {
          if(x %in% crop_codes[[class_name]]) return(class_name)
        }
        # Debug message for NA
        print(paste("No aggregation found for NC", x, year))
        return("Unclassified")
      })
    }else{
      intersected[[agg_col]] <- sapply(intersected[[name_col]], function(x) {
        if(x == "Not named") return("Not named")
        for(class_name in names(crop_codes)) {
          if(x %in% crop_codes[[class_name]]) return(class_name)
        }
        # Debug message for NA
        print(paste("No aggregation found for crop name", x, year))
        return("Unclassified")
      })
    }
  }
  return(intersected)
}

#-------------------------------------------------------------------------------------------

#' Create Crop Rotation Sankey Diagram
#' 
#' @description Creates a Sankey diagram visualizing crop rotation patterns
#' 
#' @param data sf object containing crop rotation data with Aggregated_* columns for each year
#' @param min_area Minimum area threshold in hectares (default: 1)
#' @param exclude_crops Character vector of crop types to exclude
#' @param output_path Optional file path for saving the plot
#' @param width Plot width in inches (default: 14)
#' @param height Plot height in inches (default: 10)
#' @param resolution Plot resolution in dpi (default: 200)
#' @param color Named vector of colors for each crop type
#' 
#' @return A ggplot object containing the Sankey diagram visualizing crop rotations over time
#' 
#' @details 
#' The function creates a Sankey diagram showing crop rotation patterns over time.
#' Area values are converted from square meters to hectares in the visualization.
#' 
#' @importFrom ggplot2 ggplot aes scale_fill_manual geom_text guides ylab theme_linedraw theme element_text element_rect guide_legend
#' @import ggalluvial
#' @importFrom ggalluvial geom_stratum geom_flow
#' @importFrom dplyr %>% mutate filter select if_any row_number rename
#' @importFrom tidyr gather
#' @importFrom grDevices png dev.off
#' @importFrom sf st_area
#' @importFrom tidyr pivot_longer
#' @examples
#' \dontrun{
#' library(sf)
#' # Assuming 'crop_data' is an sf object with Aggregated_* columns
#' plot <- create_crop_rotation_sankey(
#'   data = crop_data,
#'   min_area = 1,
#'   exclude_crops = c("grassland", "forest")
#' )
#' }
#' 
#' @export
create_crop_rotation_sankey <- function(data, min_area = 1, 
                                        exclude_crops = c("grassland", "forest", "flowering area", "fallow", "fruits", "permanent/tree", "Meerettich"), 
                                        output_path = NULL, 
                                        width = 14, 
                                        height = 10, 
                                        resolution = 200, 
                                        color = NULL) {
  require(ggalluvial)
  require(dplyr)
  require(stringr)
  require(forcats)
  
  # Determine transformation based on column names
  if (any(grepl("Aggregated_", names(data)))) {
    data <- data %>% 
      pivot_longer(
        cols = -c(freq, id, rotation), 
        names_to = "value", 
        values_to = "key"
      ) %>% 
      mutate(
        year = as.numeric(str_remove(value, "Aggregated_")), 
        key = factor(key), 
        key = fct_reorder(key, -key)
      ) %>% 
      rename(Area = freq) %>% 
      select(Area, id, value, key, year)
  } else {
    data <- data %>% 
      pivot_longer(
        cols = -c(freq, id, rotation), 
        names_to = "value", 
        values_to = "key"
      ) %>% 
      mutate(
        year = as.numeric(str_remove(value, "Name_")), 
        key = factor(key), 
        key = fct_reorder(key, -key)
      ) %>% 
      rename(Area = freq) %>% 
      select(Area, id, value, key, year)
  }
  
  # Get unique keys
  unique_keys <- unique(data$key)
  
  # Color generation function
  generate_random_color <- function() {
    rgb(runif(1), runif(1), runif(1))
  }
  
  # Handle color parameter
  if (is.null(color)) {
    # No colors provided, generate for all unique keys
    color <- setNames(sapply(1:length(unique_keys), function(x) generate_random_color()), 
                      unique_keys)
  } else {
    # If color is a named vector, ensure all keys have a color
    missing_keys <- setdiff(unique_keys, names(color))
    
    if (length(missing_keys) > 0) {
      # Generate colors for missing keys
      additional_colors <- setNames(
        sapply(1:length(missing_keys), function(x) generate_random_color()), 
        missing_keys
      )
      color <- c(color, additional_colors)
    }
    
    # Ensure color names match unique keys exactly
    color <- color[as.character(unique_keys)]
  }
  
  # Create plot
  sankey_plot <- ggplot(data, 
                        aes(x = year, y = Area, stratum = key, 
                            fill = key, alluvium = id, label = key)) +
    scale_fill_manual(values = color) +
    geom_stratum(alpha = .90, show.legend = TRUE, color = NA) +
    geom_flow(show.legend = FALSE) +
    geom_text(stat = "stratum", size = 4, check_overlap = TRUE) +
    guides(fill = guide_legend(ncol = 8)) +
    ylab("Area [km²]") +
    theme_linedraw() +
    theme(
      axis.text = element_text(color = "black", size = 10),
      axis.title.y = element_text(color = "black", size = 10),
      legend.background = element_rect(fill = "lightgrey", color = "black"),
      panel.background = element_rect(fill = 'lightgrey', color = 'black'),
      plot.background = element_rect(fill = "white", color = 'black'),
      legend.direction = "horizontal",
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 10)
    ) +
    scale_x_continuous(breaks = unique(data$year))
  
  # Adjust guides based on unique key count
  unique_key_count <- length(unique_keys)
  if (unique_key_count > 20) {
    sankey_plot <- sankey_plot + guides(fill = guide_legend(nrow = 8))
  } else {
    sankey_plot <- sankey_plot + guides(fill = guide_legend(nrow = 4))
  }
  
  # Save plot if output path is provided
  if (!is.null(output_path)) {
    png(output_path, width = width, height = height, units = 'in', res = resolution)
    print(sankey_plot)
    dev.off()
  } else {
    print(sankey_plot)
  }
  
  return(sankey_plot)
}

#-------------------------------------------------------------------------------------------

#' Create a Multi-Year Donut Chart
#' 
#' Creates a donut chart visualization showing crop distribution across multiple years,
#' with each year represented as a concentric ring. The function supports highlighting
#' a specific year and ensures equal ring widths for better visualization.
#' 
#' @param data A data frame containing crop distribution data with columns for each year
#'   and an 'area' column containing the area values in square meters.
#' @param year_columns Character vector of column names representing years in the data.
#'   Maximum 10 years supported.
#' @param title Character string for the chart title. Default is "Crop Distribution".
#' @param highlight_year Optional character string matching one of the year_columns to
#'   highlight that specific year's ring. Default is NULL.
#' @param colors Named vector of colors for each crop category. Names should match unique
#'   values in the year columns.
#' 
#' @return A plotly object containing the multi-year donut chart visualization.
#' 
#' @details
#' The visualization creates concentric rings where:
#' - Each ring represents one year
#' - Outer ring shows labels
#' - Ring widths are equal
#' - Gaps between rings are consistent
#' - Areas are displayed in square kilometers
#' 
#' When a highlight_year is specified, that year's ring will use full color while
#' other years are displayed in lighter shades.
#' 
#' @examples
#' \dontrun{
#' # Create sample data
#' data <- data.frame(
#'   "2020" = c("Wheat", "Corn", "Soy"),
#'   "2021" = c("Corn", "Soy", "Wheat"),
#'   "2022" = c("Soy", "Wheat", "Corn"),
#'   area = c(1000000, 1500000, 2000000)
#' )
#' 
#' # Define colors for crops
#' crop_colors <- c(
#'   "Wheat" = "#FFD700",
#'   "Corn" = "#90EE90",
#'   "Soy" = "#87CEEB"
#' )
#' 
#' # Create visualization
#' create_multi_year_donut(
#'   data = data,
#'   year_columns = c("2020", "2021", "2022"),
#'   title = "Crop Distribution 2020-2022",
#'   highlight_year = "2021",
#'   colors = crop_colors
#' )
#' }
#' 
#' @importFrom plotly plot_ly subplot layout
#' @importFrom grDevices col2rgb rgb
#' 
#' @export
create_multi_year_donut <- function(data, year_columns, title = "Crop Distribution", 
                                    highlight_year = NULL, colors) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  
  if (!all(year_columns %in% names(data))) {
    stop("All year_columns must exist in data")
  }
  
  if (!("area" %in% names(data))) {
    stop("'area' column must exist in data")
  }
  
  if (length(year_columns) > 10) {
    stop("Maximum of 10 years supported")
  }
  
  if (!is.null(highlight_year) && !(highlight_year %in% year_columns)) {
    stop("highlight_year must be NULL or one of the year_columns")
  }
  
  # Validate colors
  all_categories <- unique(unlist(data[year_columns]))
  if (!all(all_categories %in% names(colors))) {
    stop("colors must be provided for all unique categories in the data")
  }
  
  # Define constants for equal ring widths and increased gaps
  ring_width <- 0.1  
  gap_size <- 0.0003 
  outer_limit <- 0.9 
  total_width <- length(year_columns) * (ring_width + gap_size)
  inner_limit <- outer_limit - total_width
  
  # Function to lighten colors
  lighten_color <- function(color) {
    rgb_vals <- col2rgb(color)
    lighter_vals <- rgb_vals + (255 - rgb_vals) * 0.4
    rgb(lighter_vals[1], lighter_vals[2], lighter_vals[3], maxColorValue = 255)
  }
  
  # Compute hole sizes for equal ring widths + gaps
  hole_sizes <- seq(inner_limit, outer_limit - (ring_width + gap_size), 
                    length.out = length(year_columns))
  
  # Create traces for each year
  traces <- lapply(seq_along(year_columns), function(i) {
    year_col <- year_columns[i]
    
    # Adjust colors based on highlight_year
    year_colors <- if (!is.null(highlight_year)) {
      if (year_col == highlight_year) {
        colors[data[[year_col]]]
      } else {
        sapply(colors[data[[year_col]]], lighten_color)
      }
    } else {
      colors[data[[year_col]]]
    }
    
    plot_ly(data,
            labels = as.formula(paste0("~", year_col)),
            values = ~area/1e6,
            type = 'pie',
            name = year_col,
            hole = hole_sizes[i],
            domain = list(row = 0, column = 0),
            marker = list(
              colors = year_colors,
              line = list(color = 'black', width = 1)
            ),
            textposition = 'inside',
            textinfo = ifelse(i == length(year_columns), 'label', 'none'),
            insidetextfont = list(size = 12),
            hoverinfo = 'text',
            hovertemplate = paste(
              "<b>", year_col, "</b><br>",
              "Category: %{label}<br>",
              "Area: %{value:.2f} km²<extra></extra>"
            ),
            showlegend = FALSE)
  })
  
  # Combine traces and set layout
  subplot(traces, nrows = 1, shareX = TRUE, shareY = TRUE) %>%
    layout(
      title = list(
        text = title,
        font = list(color = 'white')
      ),
      xaxis = list(
        showgrid = FALSE, 
        zeroline = FALSE, 
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE, 
        zeroline = FALSE, 
        showticklabels = FALSE
      ),
      uniformtext = list(
        minsize = 12, 
        mode = 'hide'
      ),
      paper_bgcolor = '#1f1b1b',
      plot_bgcolor = '#1f1b1b'
    )
}

###############################################################################################################################################################################################################################################
#-------------------------------------------------------------------------------------------

#' Transform Rotation Data
#' 
#' @description Transforms crop rotation data for visualization and analysis purposes
#' 
#' @param All_rot_big Data frame containing crop rotation data with columns for different years
#'        and an 'area' column
#' @param input_area_range Numeric vector of length 2 specifying the minimum and maximum
#'        area range in square kilometers
#' @param choices Character vector containing all possible crop types
#' @param selected_crops Character vector containing the subset of crops to include
#'        in the analysis
#' @param type Character string specifying the type of transformation: either "basic"
#'        or "specific"
#' @param specific_crop Character string specifying a particular crop to focus on when
#'        type = "specific"
#' 
#' @return A data frame with columns:
#'   \item{Area}{Numeric, area in square kilometers}
#'   \item{id}{Integer, unique identifier for each rotation}
#'   \item{value}{Character, year identifier}
#'   \item{key}{Factor, crop type}
#'   \item{year}{Numeric, year of rotation}
#' 
#' @importFrom dplyr %>% mutate filter select if_any row_number rename
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_reorder
#' @importFrom stringr str_remove
#' @examples
#' \dontrun{
#' rotation_data <- transform_rotation_data(
#'   All_rot_big = my_rotation_data,
#'   input_area_range = c(0, 1000),
#'   choices = c("Wheat", "Corn", "Soybeans"),
#'   selected_crops = c("Wheat", "Corn"),
#'   type = "basic"
#' )
#' }
#' 
#' @export
transform_rotation_data <- function(All_rot_big, distribution_df, input_area_range, choices, selected_crops, type, specific_crop = NULL, district = NULL, EZG = NULL) {
  # Input validation with more informative messages
  if (!is.data.frame(All_rot_big) & !is.list(All_rot_big)) {
    stop("All_rot_big must be a data frame or list. Received object of class: ", class(All_rot_big))
  }
  if (!is.numeric(input_area_range) || length(input_area_range) != 2) {
    stop("input_area_range must be a numeric vector of length 2. Received: ", 
         paste(input_area_range, collapse = ", "))
  }
  if (type == "specific" && is.null(specific_crop)) {
    stop("specific_crop must be provided when type is 'specific'")
  }
  
  tryCatch({
    # Apply type-specific filtering
    pre_data <- if (type == "basic") {
      cols_to_exclude <- intersect(c("area", "District", "EZG"), names(distribution_df))
      pre_data <- distribution_df %>%
        filter(!if_any(-all_of(cols_to_exclude), ~ . %in% setdiff(choices, selected_crops)))
    } else if (type == "specific") {
      cols_to_exclude <- intersect(c("area", "District", "EZG"), names(distribution_df))
      pre_data <- distribution_df %>%
        filter(if_any(-all_of(cols_to_exclude), ~grepl(specific_crop, ., fixed = TRUE)))
    } else if (type == "district") {
      pre_data <- subset(distribution_df, District == district)
      cols_to_exclude <- intersect(c("area", "District", "EZG"), names(distribution_df))
      pre_data <- pre_data %>%
        filter(!if_any(-all_of(cols_to_exclude), ~ . %in% setdiff(choices, selected_crops)))
    } else if (type == "basin") {
      if (!"EZG" %in% names(distribution_df)) {
        stop("EZG column not found in the dataset")
      }
      pre_data <- subset(distribution_df, EZG == EZG)
      cols_to_exclude <- intersect(c("area", "District", "EZG"), names(distribution_df))
      pre_data <- pre_data %>%
        filter(!if_any(-all_of(cols_to_exclude), ~ . %in% setdiff(choices, selected_crops)))
    }
    
    pre_data <- pre_data %>%
      filter(area/1e6 >= input_area_range[1], area/1e6 <= input_area_range[2])

    rotation_data <- if (type == "basic") {
      rotation_data <- All_rot_big %>%
        filter(!if_any(-c(freq, id), ~ . %in% setdiff(choices, selected_crops)))
    } else if (type == "specific") {
      rotation_data <- All_rot_big %>%
        filter(if_any(-c(freq, id), ~grepl(specific_crop, ., fixed = TRUE)))
    } else if (type == "district") {
      rotation_data <- All_rot_big[[district]]
      rotation_data <- rotation_data %>%
        filter(!if_any(-c(freq, id), ~ . %in% setdiff(choices, selected_crops)))
    } else if (type == "basin") {
      rotation_data <- All_rot_big[[EZG]]
      rotation_data <- rotation_data %>%
        filter(!if_any(-c(freq, id), ~ . %in% setdiff(choices, selected_crops)))
    }
    
    rotation_data <- rotation_data %>%
      filter(freq >= input_area_range[1], freq <= input_area_range[2])
    
    if (any(grepl("Aggregated_", names(rotation_data)))) {
      # Final transformations
      rotation_data <- rotation_data %>%
        pivot_longer(
          cols      = -c(freq, id, rotation),
          names_to  = "value",
          values_to = "key"
        ) %>%
        mutate(
          year = as.numeric(str_remove(value, "Aggregated_")),
          key  = factor(key),
          key  = fct_reorder(key, -key)
        ) %>%
        rename(Area = freq) %>%
        select(Area, id, value, key, year)
    }else{
      # Final transformations
      rotation_data <- rotation_data %>%
        pivot_longer(
          cols      = -c(freq, id, rotation),
          names_to  = "value",
          values_to = "key"
        ) %>%
        mutate(
          year = as.numeric(str_remove(value, "Name_")),
          key  = factor(key),
          key  = fct_reorder(key, -key)
        ) %>%
        rename(Area = freq) %>%
        select(Area, id, value, key, year)
    }

    out_data <- list(rotation_data, pre_data)
    
  }, error = function(e) {
    stop("Error in data transformation: ", e$message)
  })
}

#-------------------------------------------------------------------------------------------

#' Transform Rotation Summary
#' 
#' @description Creates a detailed summary of crop rotation patterns including area calculations
#'              and crop sequences across specified years
#' 
#' @param All_rot_big Data frame containing crop rotation data with columns for different
#'        years and an 'area' column
#' @param area_range Numeric vector of length 2 specifying the minimum and maximum area
#'        range in square kilometers
#' @param type Character string specifying the type of transformation: either "basic"
#'        or "specific"
#' @param choices Character vector containing all possible crop types (required when
#'        type = "basic")
#' @param selected_crops Character vector containing the subset of crops to include
#'        (required when type = "basic")
#' @param specific_crop Character string specifying a particular crop to focus on
#'        (required when type = "specific")
#' @param max_rows Integer specifying the maximum number of rows to return in the
#'        summary (default: 3000)
#' @param years Numeric vector specifying the years to include in the analysis
#' 
#' @return A data frame with columns:
#'   \item{area_km2}{Numeric, area in square kilometers}
#'   \item{Crop_YYYY}{Character, crop type for each year YYYY in the specified range}
#' 
#' @importFrom dplyr %>% mutate filter select group_by summarize arrange
#'                      slice_head if_any all_of first across
#' @importFrom stats setNames
#' 
#' @examples
#' \dontrun{
#' summary_data <- transform_rotation_summary(
#'   All_rot_big = my_rotation_data,
#'   area_range = c(0, 1000),
#'   type = "basic",
#'   choices = c("Wheat", "Corn", "Soybeans"),
#'   selected_crops = c("Wheat", "Corn"),
#'   max_rows = 1000,
#'   years = 2015:2020
#' )
#' }
#' 
#' @export
transform_rotation_summary <- function(All_rot_big, 
                                       area_range, 
                                       type = c("basic", "specific"),
                                       choices = NULL,
                                       selected_crops = NULL,
                                       specific_crop = NULL,
                                       max_rows = 3000,
                                       years) {
  
  # Input validation
  type <- match.arg(type)
  if (!is.data.frame(All_rot_big)) {
    stop("All_rot_big must be a data frame")
  }
  if (!is.numeric(area_range) || length(area_range) != 2) {
    stop("area_range must be a numeric vector of length 2")
  }
  if (type == "basic" && (is.null(choices) || is.null(selected_crops))) {
    stop("choices and selected_crops must be provided when type is 'basic'")
  }
  if (type == "specific" && is.null(specific_crop)) {
    stop("specific_crop must be provided when type is 'specific'")
  }
  
  # Create year sequence for dynamic column creation
  crop_cols <- paste0("Name_", years)
  agg_cols <- paste0("Aggregated_", years)
  
  if (any(grepl("Aggregated_", names(All_rot_big)))) {
    setDT(All_rot_big)
    transformed_data <- All_rot_big[
      freq >= area_range[1] & freq <= area_range[2],
    ][
      ,
      c(
        .(area_km2 = round(sum(freq), 4)),
        lapply(.SD, first)
      ),
      by = rotation,
      .SDcols = agg_cols
    ][
      order(-area_km2)
    ][
      , rotation := NULL
    ]
    
    # Rename columns if needed
    setnames(transformed_data, 
             old = agg_cols, 
             new = sub("Aggregated_", "Name_", agg_cols))
  }else{
    # Data.table transformation for Crop columns
    setDT(All_rot_big)
    transformed_data <- All_rot_big[
      freq >= area_range[1] & freq <= area_range[2]
    ][
      ,
      c(
        .(area_km2 = round(sum(freq), 4)),
        lapply(.SD, first)
      ),
      by = rotation,
      .SDcols = crop_cols
    ][
      order(-area_km2)
    ][
      , rotation := NULL
    ]
  }
  
  # Apply type-specific filtering
  if (type == "basic") {
    transformed_data <- transformed_data %>%
      filter(!if_any(all_of(crop_cols), ~. %in% setdiff(choices, selected_crops)))
  } else {  # type == "specific"
    transformed_data <- transformed_data %>%
      filter(if_any(all_of(crop_cols), ~grepl(specific_crop, ., fixed = TRUE)))
  }
  
  # Apply row limit and return
  transformed_data <- transformed_data %>%
    slice_head(n = max_rows)
}

#-------------------------------------------------------------------------------------------


#' Process Crop Transitions for Specific Crop Type
#' 
#' @description Analyze and summarize temporal transitions into and out of a specific crop type
#' across multiple years using aggregated agricultural data.
#' 
#' @param data Data frame containing aggregated crop transition data with columns starting with "Aggregated_"
#' followed by year numbers, and a freq column for transition frequencies
#' @param crop Character string specifying the crop type to analyze transitions for
#' 
#' @return A data frame with columns:
#'   \itemize{
#'     \item source: Origin crop type (with "(before)" suffix for transitions into target crop)
#'     \item target: Destination crop type (with "(after)" suffix for transitions from source crop)
#'     \item value: Percentage of transitions between the crop pairs
#'   }
#' 
#' @details 
#' This function processes temporal crop rotation patterns by identifying transitions where the specified
#' crop appears as either the source or target. It handles multi-year transitions by pairing consecutive
#' years and aggregates frequencies into percentages. The output distinguishes between transitions into
#' the crop (marked as "before") and out of the crop (marked as "after").
#' 
#' @importFrom dplyr filter mutate select summarise group_by bind_rows
#' @importFrom purrr map_dfr
#' @importFrom rlang .data
#' 
#' @export
process_specific_transitions <- function(data, crop) {
  if (any(grepl("Aggregated_", names(data)))) {
    # Get column names starting with "Aggregated_"
    year_cols <- grep("^Aggregated_", names(data), value = TRUE)
    # Extract years from column names
    years <- as.numeric(sub("Aggregated_", "", year_cols))
  }else{
    # Get column names starting with "Aggregated_"
    year_cols <- grep("^Name_", names(data), value = TRUE)
    # Extract years from column names
    years <- as.numeric(sub("Name_", "", year_cols))
  }
  
  # Sort years
  years <- sort(years)
  
  # Create year pairs for transitions
  year_pairs <- list()
  for(i in 1:(length(years)-1)) {
    year_pairs[[i]] <- c(years[i], years[i+1])
  }
  
  all_transitions <- map_dfr(year_pairs, function(yp) {
    if (any(grepl("Aggregated_", names(data)))) {
      col1 <- paste0("Aggregated_", yp[1])
      col2 <- paste0("Aggregated_", yp[2])
    }else{
      col1 <- paste0("Name_", yp[1])
      col2 <- paste0("Name_", yp[2])
    }
    
    data %>%
      select(all_of(c(col1, col2)), freq) %>%
      filter(.data[[col1]] == crop | .data[[col2]] == crop) %>%
      mutate(
        source = case_when(
          .data[[col1]] == crop ~ crop,
          TRUE ~ .data[[col1]]
        ),
        target = case_when(
          .data[[col2]] == crop ~ crop,
          TRUE ~ .data[[col2]]
        )
      ) %>%
      select(source, target, freq)
  })
  
  transitions_before <- all_transitions %>%
    filter(target == crop) %>%
    mutate(
      source = paste0(source, " (before)"),
      target = crop
    )
  
  transitions_after <- all_transitions %>%
    filter(source == crop) %>%
    mutate(
      source = crop,
      target = paste0(target, " (after)")
    )
  
  bind_rows(transitions_before, transitions_after) %>%
    group_by(source, target) %>%
    summarise(value = sum(freq) * 100, .groups = 'drop')
}

#-------------------------------------------------------------------------------------------

#' Create Dummy Agricultural Fields
#' 
#' @description Generate a set of dummy agricultural fields over multiple years in a region of your choice.
#' 
#' @param output_dir Character string specifying the directory to save shapefiles
#' @param base_location Numeric vector of length 2 with longitude and latitude coordinates. Default c(2.3, 49.2) for Northern France
#' @param field_count Integer specifying number of fields to generate. Default 100
#' @param years Numeric vector specifying years to generate data for. Default 2020:2023
#' @param min_field_size Numeric value for minimum field size in degrees. Default 0.0003
#' @param max_field_size Numeric value for maximum field size in degrees. Default 0.0005
#' 
#' @return List of sf objects, one for each year
#' 
#' @details 
#' Creates a set of irregular agricultural fields with crop rotations and NC codes.
#' Fields are generated around a specified base location with realistic sizes for European agriculture.
#' 
#' @importFrom sf st_polygon st_sfc st_sf st_crs st_write
#' @importFrom stats runif
#' 
#' @export
dummy_field_creator <- function(output_dir,
                                base_location = c(2.3, 49.2),
                                field_count = 100,
                                years = 2020:2023,
                                min_field_size = 0.003,  # Increased by factor of 10
                                max_field_size = 0.005) { # Increased by factor of 10
  options(warn = -1)
  
  # Input validation
  if (!is.character(output_dir)) {
    stop("output_dir must be a character string")
  }
  if (length(base_location) != 2) {
    stop("base_location must be a vector of length 2 (longitude, latitude)")
  }
  if (!is.numeric(field_count) || field_count < 1) {
    stop("field_count must be a positive number")
  }
  
  # Create irregular field geometries with intersection checking
  create_irregular_field <- function(base_x, base_y, existing_fields = NULL, max_attempts = 10) {
    for (attempt in 1:max_attempts) {
      n_points <- sample(5:8, 1)
      angles <- sort(runif(n_points - 1, 0, 2*pi))
      angles <- c(angles, 2*pi)
      
      distances <- runif(n_points, min_field_size, max_field_size)
      
      points <- matrix(
        c(
          base_x + cos(angles) * distances,
          base_y + sin(angles) * distances
        ),
        ncol = 2
      )
      
      points <- rbind(points, points[1,])
      new_field <- st_polygon(list(points))
      
      # Check for intersections with existing fields
      if (is.null(existing_fields) || length(existing_fields) == 0) {
        return(points)
      }
      
      existing_multi <- st_sfc(existing_fields, crs = 4326)
      if (!any(st_intersects(st_sfc(new_field, crs = 4326), existing_multi, sparse = FALSE))) {
        return(points)
      }
    }
    
    # If all attempts failed, create a smaller field as fallback
    distances <- distances * 0.8
    points <- matrix(
      c(
        base_x + cos(angles) * distances,
        base_y + sin(angles) * distances
      ),
      ncol = 2
    )
    points <- rbind(points, points[1,])
    return(points)
  }
  
  # Generate fields with increased spacing
  fields_list <- list()
  grid_size <- ceiling(sqrt(field_count))
  spacing_factor <- 0.01  # Increased by factor of 10
  
  for(i in 1:field_count) {
    base_x <- base_location[1] + (i %% grid_size) * spacing_factor + runif(1, -0.001, 0.001)
    base_y <- base_location[2] + (i %/% grid_size) * spacing_factor + runif(1, -0.001, 0.001)
    
    field_points <- create_irregular_field(base_x, base_y, fields_list)
    fields_list[[i]] <- st_polygon(list(field_points))
  }
  
  base_multipolygon <- st_sfc(fields_list, crs = 4326)
  
  crops <- c("Wheat", "Barley", "Maize", "Potatoes", "Oilseed Rape")
  nc_codes <- as.integer(c(115, 131, 171, 602, 311))
  
  create_yearly_data <- function(year, base_geometry) {
    n_fields <- length(base_geometry)
    crop_distribution <- rep(crops, length.out = n_fields)
    nc_distribution <- rep(nc_codes, length.out = n_fields)
    
    data.frame(
      field_id = 1:n_fields,
      year = rep(year, n_fields),
      crop = sample(crop_distribution, n_fields),
      nc_code = sample(nc_distribution, n_fields)
    ) %>%
      st_sf(geometry = base_geometry)
  }
  
  # Generate yearly datasets
  yearly_data <- lapply(years, function(year) create_yearly_data(year, base_multipolygon))
  names(yearly_data) <- paste0("data_", years)
  
  # Create output directory and export shapefiles
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  for (i in seq_along(years)) {
    # Transform to a suitable projected CRS (UTM zone for the area - France)
    transformed_data <- st_transform(yearly_data[[i]], 32631)  # UTM zone 31N
    
    # Snap vertices in projected coordinates
    snapped_data <- st_snap(transformed_data, transformed_data, tolerance = 0.1)  # Increased tolerance
    
    # Transform back to WGS84
    yearly_data[[i]] <- st_transform(snapped_data, 4326)
    yearly_data[[i]] <- st_make_valid(yearly_data[[i]])
    st_write(yearly_data[[i]], 
             paste0(output_dir, "/fields_", years[i], ".shp"), 
             delete_layer = TRUE,
             quiet = TRUE)
  }
  
  return(yearly_data)
}

#-------------------------------------------------------------------------------------------

#' Create Bivariate Maps for Agricultural Diversity Analysis
#' 
#' @description Generate bivariate maps showing the relationship between crop diversity 
#' (unique crop count) and crop rotation transitions at district and river basin levels.
#' 
#' @param input An sf object containing agricultural field data with crop information over multiple years
#' @param agg_cols Character vector specifying the column names containing crop information for each year
#' @param districts sf object containing district-level polygons with at least "District" and "geometry" columns
#' @param EZGs Optional sf object containing river basin (EZG) polygons. Default NA
#' 
#' @return List containing two leaflet map objects:
#'   \itemize{
#'     \item District_Map: Bivariate choropleth map at district level
#'     \item EZG_Map: Bivariate choropleth map at river basin level (if EZGs provided)
#'   }
#' 
#' @details 
#' This function creates bivariate choropleth maps showing the relationship between two 
#' agricultural diversity metrics:
#' 1. Number of unique crops grown in each spatial unit
#' 2. Number of crop transitions (crop changes between consecutive years)
#' 
#' The metrics are calculated first at field level, then aggregated to district and 
#' river basin level using both simple means and area-weighted means. The resulting
#' maps use a 3x3 bivariate color scheme to show the relationship between these metrics.
#' 
#' @importFrom sf st_drop_geometry st_transform
#' @importFrom dplyr group_by summarise
#' @importFrom leaflet leaflet addProviderTiles addPolygons addControl colorFactor
#' @importFrom biscale bi_class bi_class_breaks
#' 
#' @examples
#' \dontrun{
#' # Create maps for districts only
#' maps <- diversity_mapping(
#'   input = crop_data,
#'   agg_cols = c("crop_2020", "crop_2021", "crop_2022"),
#'   districts = district_polygons
#' )
#' 
#' # Create maps for both districts and river basins
#' maps <- diversity_mapping(
#'   input = crop_data,
#'   agg_cols = c("Name_2020", "Name_2021", "Name_2022"),
#'   districts = district_polygons,
#'   EZGs = river_basin_polygons
#' )
#' }
#' 
#' @export
diversity_mapping <- function(input, agg_cols, districts, EZGs = NA, AOIs = NA){
  
  invekos_df <- st_drop_geometry(input)
  
  # Add a new column with the count of unique values
  invekos_df$unique_count <- apply(invekos_df[, agg_cols], 1, function(x) length(unique(x)))
  
  # count transitions per field
  invekos_df$transitions <- apply(invekos_df[, agg_cols], 1, function(row) {
    sum(row[-length(row)] != row[-1])
  })
  
  district_names <- c("District", "area", "unique_count", "transitions")
  BISCALE <- invekos_df[,district_names]
  
  BISCALE <- BISCALE%>%
    group_by(District)%>%
    summarise(mean_unique = mean(unique_count),
              meant_transi = mean(transitions),
              mean_unique_weight = weighted.mean(unique_count, area),
              mean_transi_weight = weighted.mean(transitions, area)
    )
  names(districts) <- c("District", "geometry")
  
  BISCALE <- merge(districts, BISCALE)
  
  
  # create classes
  BISCALE <- bi_class(BISCALE, x = mean_unique_weight, y = mean_transi_weight, style = "quantile", dim = 3)
  labels1 <- bi_class_breaks(BISCALE, x = mean_unique_weight, y = mean_transi_weight, 
                             style = "quantile", 
                             dim = 3, dig_lab = c(2,2), 
                             split = FALSE)
  
  BISCALE <- st_transform(BISCALE, crs = "EPSG:4326")
  palette <- c("#e8e8e8", "#e4acac", "#c85a5a", "#b0d5df", "#ad9ea5", "#985356", 
               "#64acbe", "#627f8c", "#574249")
  
  # Create a color palette based on the bi_class
  color_pal <- colorFactor(
    palette = palette,
    domain = BISCALE$bi_class
  )
  District_div_data <- list(BISCALE, color_pal, labels1)
  
  if(all(is.na(EZGs))) {  # Check if EZGs is entirely NA
    EZG_div_data <- NA
  }else{
    EZG_names <- c("EZG", "area", "unique_count", "transitions")
    BISCALE <- invekos_df[,EZG_names]
    
    BISCALE <- BISCALE%>%
      group_by(EZG)%>%
      summarise(mean_unique = mean(unique_count),
                meant_transi = mean(transitions),
                mean_unique_weight = weighted.mean(unique_count, area),
                mean_transi_weight = weighted.mean(transitions, area)
      )
    
    BISCALE <- merge(EZGs, BISCALE)
    
    # create classes
    BISCALE <- bi_class(BISCALE, x = mean_unique_weight, y = mean_transi_weight, style = "quantile", dim = 3)
    labels1 <- bi_class_breaks(BISCALE, x = mean_unique_weight, y = mean_transi_weight, style = "quantile", 
                               dim = 3, dig_lab = c(2,2), split = FALSE)
    
    BISCALE <- st_transform(BISCALE, crs = "EPSG:4326")
    
    # Create a color palette based on the bi_class
    color_pal <- colorFactor(
      palette = palette,
      domain = BISCALE$bi_class
    )
    
    EZG_div_data <- list(BISCALE = BISCALE, color_pal = color_pal, labels1 = labels1)
  }
  
  if(all(is.na(EZGs))) {  # Check if EZGs is entirely NA
    AOI_div_data <- NA
  }else{
    AOI_names <- c("AOI", "area", "unique_count", "transitions")
    BISCALE <- invekos_df[,AOI_names]
    
    BISCALE <- BISCALE%>%
      group_by(AOI)%>%
      summarise(mean_unique = mean(unique_count),
                meant_transi = mean(transitions),
                mean_unique_weight = weighted.mean(unique_count, area),
                mean_transi_weight = weighted.mean(transitions, area)
      )
    
    BISCALE <- merge(EZGs, BISCALE)
    
    # create classes
    BISCALE <- bi_class(BISCALE, x = mean_unique_weight, y = mean_transi_weight, style = "quantile", dim = 3)
    labels1 <- bi_class_breaks(BISCALE, x = mean_unique_weight, y = mean_transi_weight, style = "quantile", 
                               dim = 3, dig_lab = c(2,2), split = FALSE)
    
    BISCALE <- st_transform(BISCALE, crs = "EPSG:4326")
    
    # Create a color palette based on the bi_class
    color_pal <- colorFactor(
      palette = palette,
      domain = BISCALE$bi_class
    )
    
    AOI_div_data <- list(BISCALE = BISCALE, color_pal = color_pal, labels1 = labels1)
  }
  
  # list the maps
  Data <- list(District_div_data, EZG_div_data, AOI_div_data) 
  
  return(Data)
}
#-------------------------------------------------------------------------------------------


#' Calculate and Map Diversity Metrics with Error Handling 
#'
#' different administrative and ecological boundaries. It includes comprehensive error
#' handling and data validation.
#'
#' @param list_intersect_with_borders List containing intersection data with different boundary types
#'        Must contain either 'EZG_inter' or 'borders_inter' elements
#' @param CropRotViz_intersection Data frame containing crop rotation visualization data
#' @param agg_cols Character vector of column names to use for aggregation
#' @param Districts sf object containing district boundaries
#' @param EZGs sf object containing ecological zone boundaries (optional)
#' @param AOIs sf object containing areas of interest boundaries (optional)
#' @param min_rows Minimum number of rows required for sufficient data (default: 9)
#' @param min_years Minimum number of years required for sufficient data (default: 3)
#'
#' @return Returns either:
#'         - A spatial object with diversity metrics if successful
#'         - NULL if warnings occurred during processing
#'         - NA if errors occurred or insufficient data was available
#'
#' @examples
#' \dontrun{
#' # Basic usage with only Districts
#' result <- handle_diversity_mapping(
#'   list_intersect_with_borders = list(borders_inter = borders_data),
#'   CropRotViz_intersection = crop_data,
#'   agg_cols = c("year", "crop_type"),
#'   Districts = district_sf
#' )
#'
#' # Usage with all boundary types
#' result_full <- handle_diversity_mapping(
#'   list_intersect_with_borders = list(
#'     EZG_inter = ezg_data,
#'     borders_inter = borders_data
#'   ),
#'   CropRotViz_intersection = crop_data,
#'   agg_cols = c("year", "crop_type"),
#'   Districts = district_sf,
#'   EZGs = ezg_sf,
#'   AOIs = aoi_sf
#' )
#' }
#'
#' @export
handle_diversity_mapping <- function(list_intersect_with_borders, CropRotViz_intersection, 
                                     agg_cols, Districts, EZGs = NULL, AOIs = NULL) {
  
  # Function to check if there's enough data for diversity mapping
  has_sufficient_data <- function(data_list, min_rows = 9, min_years = 3) {
    return(dim(data_list)[1] > min_rows && length(years) > min_years)
  }
  
  # Helper function to handle the actual mapping with error handling
  safe_diversity_mapping <- function(...) {
    tryCatch({
      diversity_mapping(...)
    }, error = function(e) {
      warning(sprintf("Error in diversity mapping: %s", e$message))
      return(NA)
    }, warning = function(w) {
      warning(sprintf("Warning in diversity mapping: %s", w$message))
      return(NULL)
    })
  }
  
  # Helper function to log the mapping attempt
  log_mapping_attempt <- function(mapping_type) {
    message(sprintf("Attempting diversity mapping with %s", mapping_type))
  }
  
  diversity_data <- try({
    if (!is.null(EZGs)) {
      log_mapping_attempt("EZG borders")
      
      if (has_sufficient_data(list_intersect_with_borders$EZG_inter)) {
        if (!is.null(AOIs)) {
          safe_diversity_mapping(CropRotViz_intersection, agg_cols, Districts, EZGs, AOIs)
        } else {
          safe_diversity_mapping(CropRotViz_intersection, agg_cols, Districts, EZGs)
        }
      } else {
        warning("Insufficient data for EZG intersection")
        NA
      }
    } else {
      log_mapping_attempt("standard borders")
      
      if (has_sufficient_data(list_intersect_with_borders$borders_inter)) {
        if (!is.null(AOIs)) {
          safe_diversity_mapping(CropRotViz_intersection, agg_cols, Districts, AOIs)
        } else {
          safe_diversity_mapping(CropRotViz_intersection, agg_cols, Districts)
        }
      } else {
        warning("Insufficient data for borders intersection")
        NA
      }
    }
  }, silent = TRUE)
  
  # Handle any errors that occurred during the entire process
  if (inherits(diversity_data, "try-error")) {
    warning(sprintf("Error in diversity mapping process: %s", attr(diversity_data, "condition")$message))
    return(NA)
  }
  
  return(diversity_data)
}

#-------------------------------------------------------------------------------------------
#' Create a Bivariate Choropleth Map for Agricultural Diversity
#'
#' @description
#' This function generates an interactive leaflet map visualizing agricultural diversity patterns
#' using a bivariate color scheme. It displays the relationship between unique crops and
#' crop transitions across different geographical units (Districts or River Basins).
#'
#' @param data A list containing the following elements:
#'   \itemize{
#'     \item BISCALE: Spatial data frame with diversity metrics
#'     \item color_pal: Color palette function for bivariate mapping
#'     \item labels1: List with bi_x and bi_y labels for the legend
#'   }
#' @param type Character string indicating the geographic unit type ("District" or "River Basin")
#'
#' @return A leaflet map object with:
#'   \itemize{
#'     \item Choropleth layer showing bivariate relationships
#'     \item Custom legend displaying the bivariate color scheme
#'     \item Popups with detailed information for each geographic unit
#'   }
#'
#' @examples
#' \dontrun{
#' diversity_mapper(processed_data, type = "District")
#' diversity_mapper(processed_data, type = "River Basin")
#' }
#'
#' @import leaflet
#' @export
diversity_mapper <- function(data, type){
  color_pal <- data[[2]]
  label <- ifelse(type == "District", "District", "River Basin")
  palette <- c("#e8e8e8", "#e4acac", "#c85a5a", "#b0d5df", "#ad9ea5", "#985356", 
               "#64acbe", "#627f8c", "#574249")
  
  # Create the leaflet map
  Diversity_Map <- leaflet(data[[1]]) %>%
    addProviderTiles("CartoDB.Positron") %>%  # Add a light basemap
    addPolygons(
      fillColor = ~color_pal(bi_class),
      fillOpacity = 0.9,
      weight = 0.1,
      color = "black",
      popup = ~paste(
        "<strong>District:</strong>", label, "<br>",
        "<strong>Unique Crops:</strong>", round(mean_unique_weight, 2), "<br>",
        "<strong>Transitions:</strong>", round(mean_transi_weight, 2)
      )
    ) %>%
    addControl(
      html = paste(
        "<div style='background-color: white; padding: 15px; border-radius: 5px; position: relative; min-width: 150px;'>",
        # Left side labels (Transitions)
        "<div style='position: absolute; left: -78px; top: 5%; transform: rotate(-90deg) translateY(-50%); transform-origin: right; font-size: 12px; white-space: nowrap;'> More Transitions →</div>",
        sprintf("<div style='position: absolute; left: 30px; top: 100px; font-size: 11px;'>%s</div>", data[[3]]$bi_y[1]),
        sprintf("<div style='position: absolute; left: 30px; top: 62px; font-size: 11px;'>%s</div>", data[[3]]$bi_y[2]),
        sprintf("<div style='position: absolute; left: 30px; top: 25px; font-size: 11px;'>%s</div>", data[[3]]$bi_y[3]),
        
        # Color grid
        "<div style='margin-left: 60px; margin-right: -30px;'>",
        "<div style='display: grid; grid-template-columns: repeat(3, 35px); gap: 0;'>",
        paste(sprintf(
          "<div style='width: 35px; height: 35px; background-color: %s; border: 0.5px solid rgba(0,0,0,0.1);'></div>",
          palette[c(7,8,9,4,5,6,1,2,3)]
        ), collapse = ""),
        "</div>",
        
        # Bottom labels (Unique crops)
        "<div style='margin-left: -1px; display: flex; justify-content: space-between; margin-top: 2px; font-size: 11px;width: 108px;'>",
        sprintf("<span>%s</span>", data[[3]]$bi_x[1]),
        sprintf("<span>%s</span>", data[[3]]$bi_x[2]),
        sprintf("<span>%s</span>", data[[3]]$bi_x[3]),
        "</div>",
        "<div style='margin-left: -30px; margin-top: 5px; text-align: center; font-size: 12px;'>More unique crops →</div>",
        "</div>"
      ),
      position = "topright"
    )
  return(Diversity_Map)
}

#-------------------------------------------------------------------------------------------

#' Function to Generate a Random Hex Color
#
# This function creates a random color by generating random values 
# for red, green, and blue components using a uniform distribution.
#'
#' @return A hex color code representing a randomly generated color
#' @examples
#' Generate a single random color
#' random_color <- generate_hex_color()
#' print(random_color)
#' 
#' @examples
#' Generate a single random color
#' random_color <- generate_hex_color()
#' print(random_color)
#' 
#' Generate multiple random colors
#' multiple_colors <- replicate(5, generate_hex_color())
#' print(multiple_colors)
#' 
generate_hex_color <- function() {
  rgb(
    red = runif(1, 0, 1),
    green = runif(1, 0, 1),
    blue = runif(1, 0, 1),
    maxColorValue = 1
  )
}


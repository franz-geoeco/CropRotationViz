#' Add Names to Fields
#' 
#' @param fields_list List of field data
#' @return List of field data with added names
#' @importFrom sf st_read st_transform
#' @importFrom dplyr filter mutate select
#' @export
add_names <- function(fields_list, codierung_all, column){
  
  if(column == "NC"){
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
      names(file)[names(file) == "Klarschrift"] <- paste0("Name_", f$selected_year)
      
      return(file)
    })
  }else{
    fields_list <- lapply(fields_list, function(f) {
      file <- f$sf_object
      file <- file[,c(f$selected_column, attr(file, "sf_column"))]
      st_geometry(file) <- "geometry"
      names(file) <- c("Crop", "geometry")
      names(file)[names(file) == "Crop"] <- paste0("Crop_", f$selected_year)
      return(file)
    })
  }
  
  return(fields_list)
}

#' Extract Aggregation Status from Processing Summary
#' 
#' @param text Character string containing the processing summary text
#' @return Character string indicating the aggregation status ("Yes" or "No")
#' @description
#' This function extracts the aggregation status from a processing summary text.
#' It searches for a line beginning with "Aggregation:" and returns its value.
#' The function assumes the text contains a properly formatted Options section
#' with an Aggregation field.
#' 
#' @examples
#' text <- "Options\nAggregation: Yes\nIntersection: complete"
#' extract_aggregation(text)  # Returns "Yes"
#' 
extract_aggregation <- function(text) {
  # Split text into lines
  lines <- strsplit(text, "\n")[[1]]
  
  # Find the line containing "Aggregation:"
  agg_line <- grep("^Aggregation:", lines, value = TRUE)
  
  # Extract the value after the colon and trim whitespace
  aggregation <- gsub("^Aggregation:\\s*", "", agg_line)
  
  return(aggregation)
}


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
      
      intersection_subset <- st_crop(intersection, tile)
      
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
                df_missing[,-c((counter2),(+counter2+1), ncol(df_missing))] <- "Not named"
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
            tile_intersect <- st_intersection(tile_intersect, st_make_valid(field_clipped))
            
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
      intersected <- st_intersection(intersected, st_make_valid(fields_list[[i]]))
      
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
          field_clipped <- st_intersection(st_make_valid(fields_list[[i]]), tile)
          
          if (length(field_clipped) > 0) {
            # Perform intersection
            tile_intersect <- st_intersection(tile_intersect, st_make_valid(field_clipped))
            
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
      intersected <- st_intersection(intersected, st_make_valid(fields_list[[i]]))
      
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

#' Classify Crops
#' 
#' @description Classifies crops based on numeric codes and names using a lookup table
#' 
#' @param nc_codes Numeric vector of crop codes
#' @param names Character vector of crop names
#' @return Character vector of classified crop names
classify_crops <- function(nc_codes, names, crop_codes, display_names) {
  # Create a complete lookup table
  lookup_df <- data.frame(
    code = unlist(crop_codes),
    category = rep(names(crop_codes), sapply(crop_codes, length))
  )
  
  # Create a lookup vector where index is the code and value is the category
  all_codes <- sort(unique(lookup_df$code))
  code_to_category <- lookup_df$category[match(all_codes, lookup_df$code)]
  names(code_to_category) <- all_codes
  
  # Get categories for the input codes
  categories <- code_to_category[as.character(nc_codes)]
  
  # Replace NA categories with original names
  result <- ifelse(is.na(categories), names, display_names[categories])
  return(result)
}

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
intersect_with_borders <- function(input, level, EZG) {
  # Create bounding box and center point
  point <- sf::st_bbox(input)
  point <- sf::st_as_sf(data.frame(
    lon = mean(c(point[1], point[3])), 
    lat = mean(c(point[2], point[4]))
  ), coords = c("lon", "lat"))
  sf::st_crs(point) <- sf::st_crs(input)

  # Get world map
  countriesSP <- rworldmap::getMap(resolution = 'low')
  
  # Convert sf point to sp
  pointsSP <- as(sf::st_transform(point, crs = sf::st_crs(countriesSP)), "Spatial")
  
  # Now over() should work as both objects are sp class
  indices <- sp::over(pointsSP, countriesSP)
  
  # Get country code and borders
  cc <- subset(geodata::country_codes(), NAME == as.character(indices$ADMIN))
  borders <- geodata::gadm(country = cc$ISO2, level = level, path = tempdir(), resolution = 2)
  borders <- borders[paste0("NAME_", level)]
  borders <- sf::st_as_sf(borders)
  borders <- st_transform(borders, crs = sf::st_crs(input))  
  borders_inter <- borders[apply(st_intersects(borders, input, sparse = FALSE), 1, any), ]
  
  # intersection with administrative borders
  intersected <- sf::st_intersection(input, borders_inter)
  names(intersected)[length(names(intersected))-1] <- "District"
  
  if(cc$ISO2 == "DE"){
    intersected <- sf::st_intersection(intersected, sf::st_transform(EZG, crs = sf::st_crs(input))  )
    EZG_inter <- subset(EZG, EZG %in% unique(st_drop_geometry(intersected)$EZG))
    EZG_inter <- st_transform(EZG_inter, crs = sf::st_crs(input))
    EZG_inter <- sf::st_crop(EZG_inter, borders_inter)
    # Prepare the output list
    out_list <- list(intersected, borders_inter, EZG_inter)
  } else {
    # If no intersection, keep intersected as is and notify
    out_list <- list(intersected, borders_inter)
    message("No intersection found between German river basins and intersected features")
  }
  return(out_list)
}

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
aggregator <- function(intersected, years, crop_codes, display_names){
  for(year in years) {
    nc_col <- paste0("NC_", year)
    name_col <- paste0("Name_", year)
    agg_col <- paste0("Aggregated_", year)
    
    intersected[[agg_col]] <- classify_crops(
      intersected[[nc_col]], 
      intersected[[name_col]],
      crop_codes,
      display_names
    )
  }
  return(intersected)
}

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
#' @importFrom dplyr %>% mutate filter select row_number
#' @importFrom tidyr gather
#' @importFrom stringr str_remove
#' @importFrom plyr count
#' @importFrom grDevices png dev.off
#' @importFrom sf st_area
#' 
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
create_crop_rotation_sankey <- function(data, 
                                        min_area = 1,
                                        exclude_crops = c("grassland", "forest", "flowering area",
                                                          "fallow", "fruits", "permanent/tree", 
                                                          "Meerettich"),
                                        output_path = NULL,
                                        width = 14,
                                        height = 10,
                                        resolution = 200) {
  # Define color palette
  color_palette <- c("#2e8b57","darkgrey","#2f4f4f", "#556b2f",  "#6b8e23","#a0522d","#7f0000", "#708090","#483d8b","#008000","#bc8f8f","#b8860b","#bdb76b","#4682b4","#d2691e", "#9acd32","#20b2aa","#4b0082" , "#32cd32", "#8fbc8f", "#8b008b","#9932cc",  "#ff0000",  "#ffa500",  "#ffd700","#ffff00","#0000cd", "grey", "pink", "red", "purple", 
                     "#00ff00","#00fa9a","#dc143c","#00ffff","#00bfff","#f4a460","#0000ff", "#a020f0","#adff2f","#ff6347", "#da70d6","#ff00ff","#1e90ff",  "#db7093", "#fa8072",  "#dda0dd", "#87ceeb","#ff1493", "#7b68ee","#98fb98",  "#7fffd4","#ff69b4","#ffe4c4","#ffc0cb" ,"black", "white","brown","darkgreen", "darkred", "lightgrey")
  
  require(ggalluvial)
  
  data <- do.call(rbind, data)
  data$id <- 1:nrow(data)
  
  # filter
  rotation_data <- data %>%
    filter(freq > min_area)
  
  rotation_data <- rotation_data %>%
    pivot_longer(
      cols = -c(freq, id, rotation),
      names_to = "value",
      values_to = "key"
    ) %>%
    mutate(
      year = as.numeric(str_remove(value, "Aggregated_")),
      key = factor(key),
      key = fct_reorder(key, -freq)
    ) %>%
    rename(Area = freq) %>%
    select(Area, id, value, key, year)
  
  # Create color mapping
  keys <- unique(rotation_data$key)
  color_mapping <- setNames(color_palette[1:length(keys)], keys)
  
  # Create plot
  sankey_plot <- ggplot(rotation_data, 
                        aes(x = year, 
                            y = Area, 
                            stratum = key, 
                            fill = key,
                            alluvium = id, 
                            label = key)) +
    scale_fill_manual(values = color_mapping) +
    geom_stratum(alpha = .90, show.legend = TRUE, color = NA) +
    geom_flow(show.legend = FALSE) +
    geom_text(stat = "stratum", size = 4, check_overlap = TRUE) +
    guides(fill = guide_legend(ncol = 8)) +
    ylab("Area [ha]") +
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
    )
  
  # Save plot if output path is provided
  if (!is.null(output_path)) {
    png(output_path,
        width = width,
        height = height,
        units = 'in',
        res = resolution)
    print(sankey_plot)
    dev.off()
  }else{
    print(sankey_plot)
  }
  
  return(sankey_plot)
}

###############################################################################################################################################################################################################################################

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
#' @importFrom plyr count
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_reorder
#' @importFrom stringr str_remove
#' 
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
      pre_data <- subset(distribution_df, District = district)
      cols_to_exclude <- intersect(c("area", "District", "EZG"), names(distribution_df))
      pre_data <- pre_data %>%
        filter(!if_any(-all_of(cols_to_exclude), ~ . %in% setdiff(choices, selected_crops)))
    } else if (type == "basin") {
      if (!"EZG" %in% names(distribution_df)) {
        stop("EZG column not found in the dataset")
      }
      pre_data <- subset(distribution_df, EZG = EZG)
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
          year = as.numeric(str_remove(value, "Crop_")),
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
  crop_cols <- paste0("Crop_", years)
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
             new = sub("Aggregated_", "Crop_", agg_cols))
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
  # Get column names starting with "Aggregated_"
  year_cols <- grep("^Aggregated_", names(data), value = TRUE)
  
  # Extract years from column names
  years <- as.numeric(sub("Aggregated_", "", year_cols))
  
  # Sort years
  years <- sort(years)
  
  # Create year pairs for transitions
  year_pairs <- list()
  for(i in 1:(length(years)-1)) {
    year_pairs[[i]] <- c(years[i], years[i+1])
  }
  
  all_transitions <- map_dfr(year_pairs, function(yp) {
    col1 <- paste0("Aggregated_", yp[1])
    col2 <- paste0("Aggregated_", yp[2])
    
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
                                min_field_size = 0.0003,
                                max_field_size = 0.0005) {
  
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
  
  # Create irregular field geometries
  create_irregular_field <- function(base_x, base_y, max_points = 8) {
    n_points <- sample(5:max_points, 1)
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
    return(points)
  }
  
  # Generate fields
  fields_list <- list()
  grid_size <- ceiling(sqrt(field_count))
  spacing_factor <- 0.001
  
  for(i in 1:field_count) {
    base_x <- base_location[1] + (i %% grid_size) * spacing_factor + runif(1, -0.0001, 0.0001)
    base_y <- base_location[2] + (i %/% grid_size) * spacing_factor + runif(1, -0.0001, 0.0001)
    
    field_points <- create_irregular_field(base_x, base_y)
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
    snapped_data <- st_snap(transformed_data, transformed_data, tolerance = 0.01)  # tolerance in meters
    
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
#'   agg_cols = c("crop_2020", "crop_2021", "crop_2022"),
#'   districts = district_polygons,
#'   EZGs = river_basin_polygons
#' )
#' }
#' 
#' @export
diversity_mapping <- function(input, agg_cols, districts, EZGs = NA){
  
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
  labels1 <- bi_class_breaks(BISCALE, x = mean_unique, y = meant_transi, style = "quantile", 
                             dim = 3, dig_lab = c(2,2), split = FALSE)
  
  BISCALE <- st_transform(BISCALE, crs = "EPSG:4326")
  
  # Create a color palette based on the bi_class
  color_pal <- colorFactor(
    palette = c("#e8e8e8", "#e4acac", "#c85a5a", "#b0d5df", "#ad9ea5", "#985356", 
                "#64acbe", "#627f8c", "#574249"),
    domain = BISCALE$bi_class
  )
  
  # Create the leaflet map
  District_Map <- leaflet(BISCALE) %>%
      addProviderTiles("CartoDB.Positron") %>%  # Add a light basemap
      addPolygons(
        fillColor = ~color_pal(bi_class),
        fillOpacity = 0.7,
        weight = 0.1,
        color = "white",
        popup = ~paste(
          "<strong>District:</strong>", District, "<br>",
          "<strong>Unique Crops:</strong>", round(mean_unique, 2), "<br>",
          "<strong>Transitions:</strong>", round(meant_transi, 2)
        )
      ) %>%
      addControl(
        html = paste(
          "<div style='background-color: white; padding: 15px; border-radius: 5px; position: relative; min-width: 150px;'>",
          # Left side labels (Transitions)
          "<div style='position: absolute; left: -78px; top: 5%; transform: rotate(-90deg) translateY(-50%); transform-origin: right; font-size: 12px; white-space: nowrap;'> More Transitions →</div>",
          sprintf("<div style='position: absolute; left: 30px; top: 100px; font-size: 11px;'>%s</div>", labels1$bi_y[1]),
          sprintf("<div style='position: absolute; left: 30px; top: 62px; font-size: 11px;'>%s</div>", labels1$bi_y[2]),
          sprintf("<div style='position: absolute; left: 30px; top: 25px; font-size: 11px;'>%s</div>", labels1$bi_y[3]),
          
          # Color grid
          "<div style='margin-left: 60px; margin-right: -30px;'>",
          "<div style='display: grid; grid-template-columns: repeat(3, 35px); gap: 0;'>",
          paste(sprintf(
            "<div style='width: 35px; height: 35px; background-color: %s; border: 0.5px solid rgba(0,0,0,0.1);'></div>",
            bivariate_colors
          ), collapse = ""),
          "</div>",
          
          # Bottom labels (Unique crops)
          "<div style='margin-left: -1px; display: flex; justify-content: space-between; margin-top: 2px; font-size: 11px;width: 108px;'>",
          sprintf("<span>%s</span>", labels1$bi_x[1]),
          sprintf("<span>%s</span>", labels1$bi_x[2]),
          sprintf("<span>%s</span>", labels1$bi_x[3]),
          "</div>",
          "<div style='margin-left: -30px; margin-top: 5px; text-align: center; font-size: 12px;'>More unique crops →</div>",
          "</div>"
        ),
        position = "topright"
      )
  
  if(!is.na(EZGs)){
    EZG_names <- c("EZG", "area", "unique_count", "transitions")
    BISCALE <- invekos_df[,EZG_names]
    
    BISCALE <- BISCALE%>%
      group_by(EZG)%>%
      summarise(mean_unique = mean(unique_count),
                meant_transi = mean(transitions),
                mean_unique_weight = weighted.mean(unique_count, area),
                mean_transi_weight = weighted.mean(transitions, area)
      )
    
    BISCALE <- merge(EZGs, BISCALE_EZG)
    
    
    # create classes
    BISCALE <- bi_class(BISCALE, x = mean_unique_weight, y = mean_transi_weight, style = "quantile", dim = 3)
    labels1 <- bi_class_breaks(BISCALE, x = mean_unique, y = meant_transi, style = "quantile", 
                               dim = 3, dig_lab = c(2,2), split = FALSE)
    
    BISCALE <- st_transform(BISCALE, crs = "EPSG:4326")
    palette <- c("#e8e8e8", "#e4acac", "#c85a5a", "#b0d5df", "#ad9ea5", "#985356", 
                 "#64acbe", "#627f8c", "#574249")
    # Create a color palette based on the bi_class
    color_pal <- colorFactor(
      palette = palette,
      domain = BISCALE$bi_class
    )
    
    # Create the leaflet map
    EZG_Map <- leaflet(BISCALE) %>%
      addProviderTiles("CartoDB.Positron") %>%  # Add a light basemap
      addPolygons(
        fillColor = ~color_pal(bi_class),
        fillOpacity = 0.7,
        weight = 0.1,
        color = "white",
        popup = ~paste(
          "<strong>River Basin:</strong>", EZG, "<br>",
          "<strong>Unique Crops:</strong>", round(mean_unique, 2), "<br>",
          "<strong>Transitions:</strong>", round(meant_transi, 2)
        )
      ) %>%
      addControl(
        html = paste(
          "<div style='background-color: white; padding: 15px; border-radius: 5px; position: relative; min-width: 150px;'>",
          # Left side labels (Transitions)
          "<div style='position: absolute; left: -78px; top: 5%; transform: rotate(-90deg) translateY(-50%); transform-origin: right; font-size: 12px; white-space: nowrap;'> More Transitions →</div>",
          sprintf("<div style='position: absolute; left: 30px; top: 100px; font-size: 11px;'>%s</div>", labels1$bi_y[1]),
          sprintf("<div style='position: absolute; left: 30px; top: 62px; font-size: 11px;'>%s</div>", labels1$bi_y[2]),
          sprintf("<div style='position: absolute; left: 30px; top: 25px; font-size: 11px;'>%s</div>", labels1$bi_y[3]),
          
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
          sprintf("<span>%s</span>", labels1$bi_x[1]),
          sprintf("<span>%s</span>", labels1$bi_x[2]),
          sprintf("<span>%s</span>", labels1$bi_x[3]),
          "</div>",
          "<div style='margin-left: -30px; margin-top: 5px; text-align: center; font-size: 12px;'>More unique crops →</div>",
          "</div>"
        ),
        position = "topright"
      )
  }else{
    EZG_Map <- NA
  }
  
  # list the maps
  Maps <- list(District_Map, EZG_Map)
  
  return(Maps)
}

# Function to generate a random hex color
generate_hex_color <- function() {
  rgb(
    red = runif(1, 0, 1),
    green = runif(1, 0, 1),
    blue = runif(1, 0, 1),
    maxColorValue = 1
  )
}


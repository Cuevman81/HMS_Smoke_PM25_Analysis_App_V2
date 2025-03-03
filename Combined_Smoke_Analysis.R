# Load required libraries
library(shiny)
library(dplyr)
library(readr)
library(sf)
library(maps)
library(ggplot2)
library(lubridate)
library(DT)
library(lwgeom)
library(httr)
library(png)
library(magick)
library(gridExtra)
library(grid)
library(mapdata)
library(splitr)
library(ggrepel)
library(purrr)
library(memoise)
library(foreach)
library(doParallel)
library(furrr)
library(future)
library(rvest)
library(stringr)

# Global variables and functions
state_code_to_name <- c(
  "01" = "alabama", "02" = "alaska", "04" = "arizona", "05" = "arkansas", 
  "06" = "california", "08" = "colorado", "09" = "connecticut", "10" = "delaware", 
  "11" = "district of columbia", "12" = "florida", "13" = "georgia", "15" = "hawaii", 
  "16" = "idaho", "17" = "illinois", "18" = "indiana", "19" = "iowa", 
  "20" = "kansas", "21" = "kentucky", "22" = "louisiana", "23" = "maine", 
  "24" = "maryland", "25" = "massachusetts", "26" = "michigan", "27" = "minnesota", 
  "28" = "mississippi", "29" = "missouri", "30" = "montana", "31" = "nebraska", 
  "32" = "nevada", "33" = "new hampshire", "34" = "new jersey", "35" = "new mexico", 
  "36" = "new york", "37" = "north carolina", "38" = "north dakota", "39" = "ohio", 
  "40" = "oklahoma", "41" = "oregon", "42" = "pennsylvania", "44" = "rhode island", 
  "45" = "south carolina", "46" = "south dakota", "47" = "tennessee", "48" = "texas", 
  "49" = "utah", "50" = "vermont", "51" = "virginia", "53" = "washington", 
  "54" = "west virginia", "55" = "wisconsin", "56" = "wyoming", "84" = "wyoming"
)

# Define concentration ranges and corresponding colors globally
colors_ozone <- c("green", "yellow", "orange", "red")
colors_pm25 <- c("green", "yellow", "orange", "red")
concentration_labels_ozone <- c("Good", "Moderate", "USG", "Unhealthy")
concentration_labels_pm25 <- c("Good", "Moderate", "USG", "Unhealthy")

state_agency_lookup <- c(
  "Alabama" = "Alabama Department of Environmental Management",
  "Alaska" = "Alaska Department of Environmental Conservation",
  "Arizona" = "Arizona Department of Environmental Quality",
  "Arkansas" = "Arkansas Department of Environmental Quality",
  "California" = "California Air Resources Board",
  "Colorado" = "Colorado Department of Public Health and Environment",
  "Connecticut" = "Connecticut Department of Energy and Environmental Protection",
  "Delaware" = "Delaware Department of Natural Resources and Environmental Control",
  "Florida" = "Florida Department of Environmental Protection",
  "Georgia" = "Georgia Environmental Protection Division",
  "Hawaii" = "Hawaii State Department of Health",
  "Idaho" = "Idaho Department of Environmental Quality",
  "Illinois" = "Illinois Environmental Protection Agency",
  "Indiana" = "Indiana Department of Environmental Management",
  "Iowa" = "Iowa Department of Natural Resources",
  "Kansas" = "Kansas Department of Health and Environment",
  "Kentucky" = "Kentucky Department for Environmental Protection",
  "Louisiana" = "Louisiana Department of Environmental Quality",
  "Maine" = "Maine Department of Environmental Protection",
  "Maryland" = "Maryland Department of the Environment",
  "Massachusetts" = "Massachusetts Department of Environmental Protection",
  "Michigan" = "Michigan Department of Environment, Great Lakes, and Energy",
  "Minnesota" = "Minnesota Pollution Control Agency",
  "Mississippi" = "Mississippi Department of Environmental Quality",
  "Missouri" = "Missouri Department of Natural Resources",
  "Montana" = "Montana Department of Environmental Quality",
  "Nebraska" = "Nebraska Department of Environment and Energy",
  "Nevada" = "Nevada Division of Environmental Protection",
  "New Hampshire" = "New Hampshire Department of Environmental Services",
  "New Jersey" = "New Jersey Department of Environmental Protection",
  "New Mexico" = "New Mexico Environment Department",
  "New York" = "New York State Department of Environmental Conservation",
  "North Carolina" = "North Carolina Department of Environmental Quality",
  "North Dakota" = "North Dakota Department of Environmental Quality",
  "Ohio" = "Ohio Environmental Protection Agency",
  "Oklahoma" = "Oklahoma Department of Environmental Quality",
  "Oregon" = "Oregon Department of Environmental Quality",
  "Pennsylvania" = "Pennsylvania Department of Environmental Protection",
  "Rhode Island" = "Rhode Island Department of Environmental Management",
  "South Carolina" = "South Carolina Department of Health and Environmental Control",
  "South Dakota" = "South Dakota Department of Environment and Natural Resources",
  "Tennessee" = "Tennessee Department of Environment and Conservation",
  "Texas" = "Texas Commission on Environmental Quality",
  "Utah" = "Utah Department of Environmental Quality",
  "Vermont" = "Vermont Department of Environmental Conservation",
  "Virginia" = "Virginia Department of Environmental Quality",
  "Washington" = "Washington State Department of Ecology",
  "West Virginia" = "West Virginia Department of Environmental Protection",
  "Wisconsin" = "Wisconsin Department of Natural Resources",
  "Wyoming" = "Wyoming Department of Environmental Quality"
)

get_latest_tiering_csv_url <- function() {
  base_url <- "https://www.epa.gov/air-quality-analysis/pm25-tiering-tool-exceptional-events-analysis"
  
  tryCatch({
    # Read the webpage
    page <- read_html(base_url)
    
    # Find all links on the page
    links <- page %>% html_nodes("a") %>% html_attr("href")
    
    # Filter for CSV files related to tiering
    csv_links <- links[str_detect(links, "r_fire_excluded_tiers.*\\.csv$")]
    
    if (length(csv_links) == 0) {
      warning("No tiering CSV files found on the EPA page.")
      return(NULL)
    }
    
    # Get the most recent file (assuming the date in the filename is the most recent)
    latest_csv <- csv_links[which.max(str_extract(csv_links, "\\d{8}"))]
    
    # Construct the full URL
    if (startsWith(latest_csv, "http")) {
      full_url <- latest_csv
    } else if (startsWith(latest_csv, "/")) {
      full_url <- paste0("https://www.epa.gov", latest_csv)
    } else {
      full_url <- paste0("https://www.epa.gov/", latest_csv)
    }
    
    return(full_url)
  }, error = function(e) {
    warning(paste("Error fetching latest CSV URL:", e$message))
    return(NULL)
  })
}


# Function to clean geometry
clean_geometry <- function(geom) {
  tryCatch({
    if (sf::st_is_longlat(geom)) {
      geom <- sf::st_transform(geom, 3857)
    }
    if (any(sf::st_geometry_type(geom) == "GEOMETRYCOLLECTION")) {
      geom <- sf::st_collection_extract(geom, "POLYGON")
    }
    geom <- sf::st_cast(geom, "MULTIPOLYGON")
    geom <- sf::st_simplify(geom, dTolerance = 1e-8, preserveTopology = TRUE)
    geom <- sf::st_make_valid(geom)
    geom <- geom[sf::st_is_valid(geom),]
    geom <- sf::st_simplify(geom, dTolerance = 0.01, preserveTopology = TRUE)
    geom <- sf::st_buffer(geom, dist = 0.01)
    geom <- sf::st_buffer(geom, dist = -0.01)
    geom <- sf::st_make_valid(geom)
    geom <- sf::st_transform(geom, 4326)
    return(geom)
  }, error = function(e) {
    warning(paste("Error in clean_geometry:", e$message))
    return(sf::st_sfc(sf::st_multipolygon()))
  })
}

# Function to read KML data
read_kml <- function(date, layer, state_sf_transformed, sites_sf) {
  url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/KML/", 
                format(date, "%Y/%m/hms_smoke"), format(date, "%Y%m%d"), ".kml")
  
  tryCatch({
    kml_data <- sf::st_read(url, layer = layer, quiet = TRUE)
    kml_data_cleaned <- clean_geometry(kml_data)
    
    state_sf_transformed_local <- sf::st_transform(state_sf_transformed, sf::st_crs(kml_data_cleaned))
    state_sf_transformed_local <- clean_geometry(state_sf_transformed_local)
    
    state_smoke <- sf::st_intersection(kml_data_cleaned, state_sf_transformed_local)
    
    if (nrow(state_smoke) == 0) return(NULL)
    
    sites_sf_transformed <- sf::st_transform(sites_sf, sf::st_crs(state_smoke))
    
    # Convert to planar coordinate system for distance calculation
    state_smoke_planar <- sf::st_transform(state_smoke, 3857)
    sites_sf_planar <- sf::st_transform(sites_sf_transformed, 3857)
    
    # Find nearest smoke polygon for each site
    nearest_smoke <- st_nearest_feature(sites_sf_planar, state_smoke_planar)
    
    # Calculate distances
    distances <- st_distance(sites_sf_planar, state_smoke_planar[nearest_smoke,], by_element = TRUE)
    
    # Convert distances to numeric (meters) and filter
    max_distance <- 11000  # approximately 0.1 degrees
    sites_data <- sites_sf_transformed[as.numeric(distances) <= max_distance,]
    
    if (nrow(sites_data) == 0) return(NULL)
    
    sites_data$Smoke_Intensity <- layer
    sites_data$date <- as.Date(date)
    
    print(paste("Date:", date, "Layer:", layer, "Sites with smoke:", nrow(sites_data)))
    
    return(sites_data)
  }, error = function(e) {
    warning(paste("Failed to process layer", layer, "for date", date, ":", e$message))
    return(NULL)
  })
}

check_tiering_csv <- function(url) {
  tryCatch({
    data <- read.csv(url)
    print(paste("Successfully read data from:", url))
    print(paste("Number of rows:", nrow(data)))
    print("Column names:")
    print(names(data))
    print("Sample data (first 3 rows):")
    print(head(data, 3))
    
    # Check for expected columns or similar named columns
    expected_columns <- c("SITE_ID", "month", "Tier.1", "Tier.2")
    alt_expected_columns <- c("SITE_ID", "month", "Tier1", "Tier2")
    
    for (col in expected_columns) {
      if (col %in% names(data)) {
        print(paste("Column", col, "found."))
      } else if (gsub("\\.", "", col) %in% names(data)) {
        print(paste("Column", gsub("\\.", "", col), "found instead of", col))
      } else {
        print(paste("Column", col, "not found."))
      }
    }
    
    return(TRUE)
  }, error = function(e) {
    print(paste("Error checking tiering CSV:", e$message))
    return(FALSE)
  })
}

# Function to generate combined plot for daily analysis
generate_combined_plot <- function(selectedDate, selectedState, ASOS_Stations, aqStateCode) {
  tryCatch({
    selected_year <- format(selectedDate, "%Y")
    
    base_url <- paste0("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/", selected_year, "/")
    formatted_date <- format(selectedDate, "%Y%m%d")
    daily_data_files <- readLines(paste0(base_url, formatted_date, "/daily_data_v2.dat"))
    
    # Use aqStateCode for air quality data filtering
    state_lines <- daily_data_files[grep(paste0("^", aqStateCode), daily_data_files)]
    
    if (length(state_lines) == 0) {
      return(list(error = "No data found for the selected state code and date"))
    }
    
    # Ozone data processing
    filtered_lines_ozone <- state_lines[grep("OZONE-8HR", state_lines)]
    data_ozone <- strsplit(filtered_lines_ozone, "\\|")
    lat_ozone <- as.numeric(sapply(data_ozone, function(x) x[11]))
    long_ozone <- as.numeric(sapply(data_ozone, function(x) x[12]))
    ozone_8hr <- as.numeric(sapply(data_ozone, function(x) x[6]))
    df_ozone <- data.frame(Latitude = lat_ozone, Longitude = long_ozone, Ozone_8HR = ozone_8hr)
    
    concentration_ranges_ozone <- c(0, 50, 70, 86, Inf)
    df_ozone$Concentration_Category <- cut(df_ozone$Ozone_8HR, breaks = concentration_ranges_ozone, labels = concentration_labels_ozone, right = FALSE)
    
    # PM2.5 data processing
    filtered_lines_pm25 <- state_lines[grep("PM2.5-24hr", state_lines)]
    data_pm25 <- strsplit(filtered_lines_pm25, "\\|")
    lat_pm25 <- as.numeric(sapply(data_pm25, function(x) x[11]))
    long_pm25 <- as.numeric(sapply(data_pm25, function(x) x[12]))
    pm25_24hr <- as.numeric(sapply(data_pm25, function(x) x[6]))
    df_pm25 <- data.frame(Latitude = lat_pm25, Longitude = long_pm25, PM25_24HR = pm25_24hr)
    
    concentration_ranges_pm25 <- c(0, 9, 35.4, 55.4, Inf)
    df_pm25$Concentration_Label <- cut(df_pm25$PM25_24HR, breaks = concentration_ranges_pm25, labels = concentration_labels_pm25, right = FALSE)
    
    # Meteorological data retrieval
    print("ASOS_Stations structure:")
    print(str(ASOS_Stations))
    
    ASOS_Stations <- ASOS_Stations %>%
      filter(!is.na(ICAO) & ICAO != "") %>%
      mutate(ICAO = gsub("^K", "", trimws(ICAO)))
    
    weather_data <- data.frame()
    
    for (icao in ASOS_Stations$ICAO) {
      url <- paste0("https://mesonet.agron.iastate.edu/cgi-bin/request/daily.py?network=", 
                    state.abb[which(state.name == selectedState)], "_ASOS&stations=", 
                    URLencode(as.character(icao)),
                    "&year1=", format(selectedDate, "%Y"), 
                    "&month1=", format(selectedDate, "%m"),
                    "&day1=", format(selectedDate, "%d"), 
                    "&year2=", format(selectedDate, "%Y"),
                    "&month2=", format(selectedDate, "%m"), 
                    "&day2=", format(selectedDate, "%d"))
      
      print(paste("Requesting data for station:", icao))
      print(paste("URL:", url))
      
      retry_count <- 0
      max_retries <- 1
      
      while (retry_count < max_retries) {
        tryCatch({
          response <- GET(url)
          if (status_code(response) == 200) {
            station_data <- read.csv(text = content(response, "text", encoding = "UTF-8"))
            if (nrow(station_data) > 0) {
              weather_data <- rbind(weather_data, station_data)
              print(paste("Successfully retrieved data for station:", icao))
              break  # Successful, exit the retry loop
            } else {
              print(paste("No data returned for station:", icao))
            }
          } else {
            print(paste("HTTP error for station", icao, "- Status code:", status_code(response)))
          }
          retry_count <- retry_count + 1
          if (retry_count == max_retries) {
            warning(paste("Failed to retrieve data for station", icao, "after", max_retries, "attempts"))
          }
        }, error = function(e) {
          print(paste("Error for station", icao, ":", conditionMessage(e)))
          retry_count <- retry_count + 1
        })
        Sys.sleep(1)  # Add a small delay between retries
      }
    }
    
    if (nrow(weather_data) == 0) {
      warning("No weather data could be retrieved for the selected date and state.")
      return(list(error = "No weather data available"))
    }
    
    weather_data <- weather_data %>% 
      rename(ICAO = station, ws = avg_wind_speed_kts, wd = avg_wind_drct, date = day) %>%
      mutate(across(c(wd, ws), ~as.numeric(as.character(.))))
    
    joined_data <- left_join(
      distinct(weather_data), 
      distinct(ASOS_Stations[, c("ICAO", "LAT", "LON")]), 
      by = "ICAO"
    )
    
    print("Joined data structure:")
    print(str(joined_data))
    
    numeric_cols <- c("max_temp_f", "min_temp_f", "max_dewpoint_f", "min_dewpoint_f", "precip_in", "min_rh")
    joined_data[numeric_cols] <- lapply(joined_data[numeric_cols], function(x) as.numeric(as.character(x)))
    joined_data$ws <- round(joined_data$ws, 1)
    
    # Get map data
    map_data_state <- map_data("state", region = tolower(selectedState))
    map_data_county <- map_data("county", region = tolower(selectedState))
    
    # Load state boundary data
    state_map <- map_data("state", region = tolower(selectedState))
    
    # Check which points are in state
    point_in_polygon <- function(point, poly) {
      point.x <- point[1]
      point.y <- point[2]
      poly.x <- poly$long
      poly.y <- poly$lat
      
      nvert <- length(poly.x)
      inside <- FALSE
      
      j <- nvert
      for (i in 1:nvert) {
        if (((poly.y[i] > point.y) != (poly.y[j] > point.y)) &&
            (point.x < (poly.x[j] - poly.x[i]) * (point.y - poly.y[i]) / (poly.y[j] - poly.y[i]) + poly.x[i])) {
          inside <- !inside
        }
        j <- i
      }
      
      return(inside)
    }
    
    points_in_state <- function(df, state_map) {
      sapply(1:nrow(df), function(i) {
        point_in_polygon(c(df$Longitude[i], df$Latitude[i]), state_map)
      })
    }
    
    df_ozone$in_state <- points_in_state(df_ozone, state_map)
    df_pm25$in_state <- points_in_state(df_pm25, state_map)
    
    # Generate plots
    map_ozone <- ggplot() +
      geom_polygon(data = state_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
      geom_polygon(data = map_data("county", region = tolower(selectedState)), aes(x = long, y = lat, group = group), fill = NA, color = "gray", size = 0.2) +
      geom_point(data = df_ozone[df_ozone$in_state,], aes(x = Longitude, y = Latitude, color = Concentration_Category), size = 5, alpha = 0.7) +
      geom_text(data = df_ozone[df_ozone$in_state,], aes(x = Longitude, y = Latitude, label = Ozone_8HR), vjust = -0.5, size = 5, color = "black") +
      scale_color_manual(values = setNames(colors_ozone, levels(df_ozone$Concentration_Category))) +
      labs(title = paste(selectedState, "Map of Ozone 8HR Concentrations"),
           subtitle = paste("Date: ", format(selectedDate, "%Y-%m-%d")),
           x = "Longitude", y = "Latitude",
           color = "Daily 8HR Ozone Concentration") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 18),
            plot.subtitle = element_text(size = 14)) +
      coord_fixed(ratio = 1.2)
    
    map_pm25 <- ggplot() +
      geom_polygon(data = state_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
      geom_polygon(data = map_data("county", region = tolower(selectedState)), aes(x = long, y = lat, group = group), fill = NA, color = "gray", size = 0.2) +
      geom_point(data = df_pm25[df_pm25$in_state,], aes(x = Longitude, y = Latitude, color = Concentration_Label), size = 5, alpha = 0.7) +
      geom_text(data = df_pm25[df_pm25$in_state,], aes(x = Longitude, y = Latitude, label = PM25_24HR), vjust = -0.5, size = 5, color = "black") +
      scale_color_manual(values = setNames(colors_pm25, levels(df_pm25$Concentration_Label))) +
      labs(title = paste(selectedState, "Map of PM2.5 24HR Concentrations"),
           subtitle = paste("Date: ", format(selectedDate, "%Y-%m-%d")),
           x = "Longitude", y = "Latitude",
           color = "Daily PM2.5 Concentration") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 18),
            plot.subtitle = element_text(size = 14)) +
      coord_fixed(ratio = 1.2)
    
    # Helper functions
    round_to_nearest <- function(x, base) {
      base * round(x / base)
    }
    
    get_nice_breaks <- function(min_val, max_val, n = 5) {
      range <- max_val - min_val
      if (range == 0) return(min_val)
      magnitude <- 10^floor(log10(range))
      step <- ceiling(range / (n * magnitude)) * magnitude
      seq(floor(min_val / step) * step, ceiling(max_val / step) * step, by = step)
    }
    
    # Generic plotting function for meteorological data
    plot_met_data <- function(joined_data, state_map, selectedState, selectedDate, var_name, plot_title, color_palette) {
      var_range <- range(joined_data[[var_name]], na.rm = TRUE)
      breaks <- get_nice_breaks(var_range[1], var_range[2])
      
      ggplot() +
        geom_polygon(data = state_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
        geom_polygon(data = map_data("county", region = tolower(selectedState)), aes(x = long, y = lat, group = group), fill = NA, color = "gray", size = 0.2) +
        geom_point(data = joined_data, aes(x = LON, y = LAT, color = .data[[var_name]]), size = 4, alpha = 0.7) +
        geom_text_repel(data = joined_data, aes(x = LON, y = LAT, label = sprintf("%.1f", .data[[var_name]])), 
                        size = 4, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) +
        scale_color_gradientn(
          name = plot_title, 
          colors = color_palette,
          breaks = breaks,
          labels = sprintf("%.0f", breaks),
          limits = range(breaks)
        ) +
        labs(title = paste(selectedState, plot_title),
             subtitle = paste("Date: ", format(selectedDate, "%Y-%m-%d")),
             x = "Longitude", y = "Latitude",
             caption = "Data Source: IEMCOW") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom",
              plot.title = element_text(size = 16),
              plot.subtitle = element_text(size = 12),
              legend.key.width = unit(2, "cm")) +
        coord_fixed(ratio = 1.0)
    }
    
    # Define plot parameters for each meteorological variable
    met_plot_params <- list(
      max_temp = list(var_name = "max_temp_f", plot_title = "Max Temperature (F)", 
                      color_palette = c("blue", "green", "yellow", "red")),
      min_temp = list(var_name = "min_temp_f", plot_title = "Min Temperature (F)", 
                      color_palette = c("darkblue", "blue", "lightblue", "white")),
      max_dewpoint = list(var_name = "max_dewpoint_f", plot_title = "Max Dewpoint (F)", 
                          color_palette = c("lightyellow", "yellow", "orange", "red")),
      min_dewpoint = list(var_name = "min_dewpoint_f", plot_title = "Min Dewpoint (F)", 
                          color_palette = c("lightyellow", "yellow", "orange", "red")),
      wind_speed = list(var_name = "ws", plot_title = "Wind Speed (knots)", 
                        color_palette = c("blue", "green", "yellow", "red"))
    )
    
    
    degrees_to_cardinal <- function(degrees) {
      directions <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", 
                      "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
      index <- round((degrees + 11.25) / 22.5) %% 16 + 1
      return(directions[index])
    }
    
    joined_data$cardinal_direction <- degrees_to_cardinal(joined_data$wd)
    
    # Function to generate all meteorological plots
    generate_met_plots <- function(joined_data, state_map, selectedState, selectedDate) {
      lapply(met_plot_params, function(params) {
        plot_met_data(joined_data, state_map, selectedState, selectedDate, 
                      params$var_name, params$plot_title, params$color_palette)
      })
    }
    
    # Wind direction plot (separate due to its unique nature)
    plot_wind_direction <- function(joined_data, state_map, selectedState, selectedDate) {
      ggplot() +
        geom_polygon(data = state_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
        geom_polygon(data = map_data("county", region = tolower(selectedState)), aes(x = long, y = lat, group = group), fill = NA, color = "gray", size = 0.2) +
        geom_point(data = joined_data, aes(x = LON, y = LAT, color = cardinal_direction), size = 4, alpha = 0.7) +
        geom_text_repel(data = joined_data, aes(x = LON, y = LAT, label = cardinal_direction), 
                        size = 4, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) +
        scale_color_manual(
          name = "Wind Direction",
          values = c("N" = "blue", "NNE" = "cyan", "NE" = "deepskyblue", "ENE" = "lightblue",
                     "E" = "green", "ESE" = "lightgreen", "SE" = "yellowgreen", "SSE" = "yellow",
                     "S" = "orange", "SSW" = "darkorange", "SW" = "orangered", "WSW" = "red",
                     "W" = "darkred", "WNW" = "firebrick", "NW" = "maroon", "NNW" = "darkmagenta")
        ) +
        labs(title = paste(selectedState, "Wind Directions"),
             subtitle = paste("Date: ", format(selectedDate, "%Y-%m-%d")),
             x = "Longitude", y = "Latitude",
             caption = "Data Source: IEMCOW") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom",
              plot.title = element_text(size = 16),
              plot.subtitle = element_text(size = 12),
              legend.key.width = unit(0.5, "cm")) +
        coord_fixed(ratio = 1.0) +
        guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4)))
    }
    
    # Generate weather map image URLs
    weather_map_date <- as.Date(selectedDate) + 1
    weather_map_year <- format(weather_map_date, "%Y")
    weather_map_url <- paste0("https://www.wpc.ncep.noaa.gov/archives/sfc/", weather_map_year, "/namussfc", format(weather_map_date, "%Y%m%d00"), ".gif")
    
    # Generate upper air map image URLs
    date_format <- format(weather_map_date, "%y%m%d")
    upper_air_levels <- c("925", "850", "700", "500", "300")
    upper_air_urls <- sapply(upper_air_levels, function(level) {
      paste0("https://www.spc.noaa.gov/obswx/maps/", level, "_", date_format, "_00.gif")
    })
    
    # Download and process all maps
    download_and_process_map <- function(url) {
      temp_file <- tempfile(fileext = ".gif")
      download.file(url = url, destfile = temp_file, mode = "wb", quiet = TRUE)
      map <- tryCatch({
        magick::image_read(temp_file)
      }, error = function(e) {
        # If an error occurs, return an error message
        return(list(error = paste("An error occurred:", conditionMessage(e))))
      })
      if (!is.null(map)) {
        grid::rasterGrob(as.raster(map), interpolate = TRUE)
      } else {
        NULL
      }
    }
    
    weather_map_grob <- download_and_process_map(weather_map_url)
    upper_air_map_grobs <- lapply(upper_air_urls, download_and_process_map)
    
    # Generate meteorological plots
    met_plots <- generate_met_plots(joined_data, state_map, selectedState, selectedDate)
    wind_direction_plot <- plot_wind_direction(joined_data, state_map, selectedState, selectedDate)
    
    # Combine all plots
    air_quality_plot <- arrangeGrob(
      map_ozone, map_pm25, met_plots$max_temp,
      met_plots$min_temp, met_plots$max_dewpoint, met_plots$min_dewpoint,
      met_plots$wind_speed, wind_direction_plot, 
      ggplot() + theme_void() + ggtitle("Reserved for future use"),
      ncol = 3,
      widths = c(1, 1, 1),
      heights = c(1, 1, 1)
    )
    
    # Increase the size of the plot
    air_quality_plot <- ggplotGrob(ggplot() + annotation_custom(air_quality_plot) + 
                                     theme(plot.margin = margin(20, 20, 20, 20, "pt")))
    
    # Return all plots and maps
    return(list(
      air_quality_plot = air_quality_plot, 
      weather_map_plot = weather_map_grob,
      upper_air_925_plot = upper_air_map_grobs[[1]],
      upper_air_850_plot = upper_air_map_grobs[[2]],
      upper_air_700_plot = upper_air_map_grobs[[3]],
      upper_air_500_plot = upper_air_map_grobs[[4]],
      upper_air_300_plot = upper_air_map_grobs[[5]]
    ))
  }, error = function(e) {
    # If an error occurs, return an error message
    return(list(error = paste("An error occurred:", conditionMessage(e))))
  })
}

# Function to parse HMS Smoke data
parse_hms_smoke_data <- function(selectedDate, selectedState) {
  tryCatch({
    formatted_date <- format(selectedDate, "%Y%m%d")
    year <- format(selectedDate, "%Y")
    month <- format(selectedDate, "%m")
    
    kml_url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/KML/", year, "/", month, "/hms_smoke", formatted_date, ".kml")
    
    temp_file <- tempfile(fileext = ".kml")
    download.file(kml_url, temp_file, mode = "wb")
    
    layers <- st_layers(temp_file)
    
    smoke_data_list <- lapply(layers$name, function(layer_name) {
      tryCatch({
        suppressWarnings({
          layer_data <- st_read(temp_file, layer = layer_name, quiet = TRUE)
        })
        layer_data$Density <- sub("Smoke \\((.+)\\)", "\\1", layer_name)
        return(layer_data)
      }, error = function(e) {
        warning(paste("Error reading layer:", layer_name, "-", conditionMessage(e)))
        return(NULL)
      })
    })
    
    smoke_data <- do.call(rbind, Filter(Negate(is.null), smoke_data_list))
    
    if (is.null(smoke_data) || nrow(smoke_data) == 0) {
      warning("No valid smoke data found in the KML file.")
      return(NULL)
    }
    
    smoke_data <- st_zm(smoke_data, drop = TRUE, what = "ZM")
    
    # Clean and fix geometries
    smoke_data <- clean_geometry(smoke_data)
    
    if (nrow(smoke_data) == 0) {
      warning("All geometries were invalid and could not be fixed.")
      return(NULL)
    }
    
    # Create national smoke data
    national_smoke_data <- smoke_data
    
    # Create state-specific smoke data
    state_boundary <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
      filter(ID == tolower(selectedState)) %>%
      st_transform(st_crs(smoke_data))
    
    state_boundary <- clean_geometry(state_boundary)
    
    state_smoke_data <- st_intersection(smoke_data, state_boundary)
    
    return(list(national = national_smoke_data, state = state_smoke_data))
  }, error = function(e) {
    warning(paste("Error in parse_hms_smoke_data:", conditionMessage(e)))
    return(NULL)
  })
}

# Function to generate HMS Smoke plots
generate_hms_smoke_plots <- function(selectedDate, selectedState) {
  tryCatch({
    smoke_data <- parse_hms_smoke_data(selectedDate, selectedState)
    
    if (is.null(smoke_data)) {
      return(list(
        state = ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid smoke data available") + theme_void(),
        national = ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid smoke data available") + theme_void()
      ))
    }
    
    # State plot
    state_plot <- ggplot() +
      geom_polygon(data = map_data("state", region = tolower(selectedState)), 
                   aes(x = long, y = lat, group = group), 
                   fill = "white", color = "black") +
      geom_sf(data = smoke_data$state, aes(fill = Density), alpha = 0.5) +
      scale_fill_manual(values = c("Light" = "lightblue", "Medium" = "grey", "Heavy" = "darkgrey")) +
      coord_sf() +
      labs(title = paste("HMS Smoke Data for", selectedState),
           subtitle = paste("Date:", format(selectedDate, "%Y-%m-%d")),
           fill = "Smoke Density") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # National plot with state and province boundaries
    us_states <- map_data("state")
    canada_provinces <- map_data("worldHires", "Canada")
    
    national_plot <- ggplot() +
      geom_polygon(data = us_states, aes(x = long, y = lat, group = group), 
                   fill = "white", color = "darkgray", size = 0.2) +
      geom_polygon(data = canada_provinces, aes(x = long, y = lat, group = group), 
                   fill = "white", color = "darkgray", size = 0.2) +
      geom_sf(data = smoke_data$national, aes(fill = Density), alpha = 0.5) +
      geom_path(data = us_states, aes(x = long, y = lat, group = group), 
                color = "black", size = 0.3) +
      geom_path(data = canada_provinces, aes(x = long, y = lat, group = group), 
                color = "black", size = 0.3) +
      scale_fill_manual(values = c("Light" = "lightblue", "Medium" = "grey", "Heavy" = "darkgrey")) +
      coord_sf(xlim = c(-140, -50), ylim = c(25, 70), expand = FALSE) +
      labs(title = "HMS Smoke Data for US and Canada",
           subtitle = paste("Date:", format(selectedDate, "%Y-%m-%d")),
           fill = "Smoke Density") +
      theme_minimal() +
      theme(legend.position = "bottom",
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
    return(list(state = state_plot, national = national_plot))
  }, error = function(e) {
    warning(paste("Error in generate_hms_smoke_plots:", conditionMessage(e)))
    return(list(
      state = ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                  label = paste("Error generating HMS Smoke plots:", conditionMessage(e))) + theme_void(),
      national = ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                     label = paste("Error generating HMS Smoke plots:", conditionMessage(e))) + theme_void()
    ))
  })
}

# New function to run trajectory model
run_trajectory_model <- function(lat, lon, heights, duration, start_date, daily_hours, direction, met_type) {
  tryCatch({
    # Use absolute paths
    project_dir <- normalizePath(".", winslash = "/")
    met_dir <- file.path(project_dir, "met")
    exec_dir <- file.path(project_dir, "out")
    
    # Create directories if they don't exist
    dir.create(met_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(exec_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Get the path to hyts_std.exe
    hysplit_exe <- normalizePath(system.file("win", "hyts_std.exe", package = "splitr"), winslash = "/")
    
    # Create and run the trajectory model for each height
    results <- map(heights, function(height) {
      create_trajectory_model() %>% 
        add_trajectory_params(
          lat = lat,
          lon = lon,
          height = height,
          duration = duration,
          days = as.character(start_date),
          daily_hours = daily_hours,
          direction = direction,
          met_type = met_type,
          met_dir = met_dir,
          exec_dir = exec_dir
        ) %>% 
        run_model()
    })
    
    return(list(status = "success", data = results))
  }, error = function(e) {
    return(list(status = "error", message = conditionMessage(e)))
  })
}


# UI Definition
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)


ui <- tagList(
  # Modify the CSS to add more positioning control
  tags$head(
    tags$style(HTML("
      #logo {
        position: fixed;
        top: 60px;
        right: 10px;
        z-index: 1000;
      }
      #logo img {
        max-height: 80px;
        width: auto;
      }
      .contact-info {
        font-size: 0.9em;
        color: #666;
        margin-top: 20px;
        border-top: 1px solid #ddd;
        padding-top: 10px;
      }
    "))
  ),
  
  # Adjust the absolutePanel settings
  absolutePanel(
    id = "logo",
    top = 60,
    right = 10, 
    width = "auto", 
    height = "auto",
    img(src = "MDEQ_Logo.png", height = "80px")
  ),
  
  # Your existing UI code starts here
  navbarPage(
    theme = shinytheme("flatly"),
    title = "Air Quality Analysis Tools",
    
    tabPanel("HMS Smoke PM2.5 Analysis",
             fluidRow(
               column(3,
                      wellPanel(
                        dateRangeInput("dateRange", "Date Range", start = Sys.Date() - 30, end = Sys.Date()),
                        actionButton("downloadData", "Download Airnow Data", icon("download")),
                        pickerInput("stateCode", "State Code", choices = NULL, options = list(`live-search` = TRUE)),
                        pickerInput("countyCode", "County Code", choices = NULL, options = list(`live-search` = TRUE)),
                        pickerInput("siteId", "Site ID", choices = NULL, options = list(`live-search` = TRUE)),
                        checkboxGroupButtons("smokeIntensity", "Smoke Intensity", 
                                             choices = c("Light", "Medium", "Heavy"),
                                             selected = c("Light", "Medium", "Heavy")),
                        actionButton("processHMSData", "Process HMS Smoke Data", icon("play")),
                        # Add contact information
                        tags$div(class = "contact-info",
                                 "Questions, comments, or bug reports?",
                                 tags$br(),
                                 "Email Rodney Cuevas:",
                                 tags$a("RCuevas@mdeq.ms.gov", href = "mailto:RCuevas@mdeq.ms.gov")
                        )
                      )
               ),
               column(9,
                      tabBox(
                        width = 12,
                        tabPanel("PM2.5 Data", withSpinner(DTOutput("dataTable"))),
                        tabPanel("Combined Data", withSpinner(DTOutput("combinedDataTable"))),
                        tabPanel("HMS Plot with Tiering", 
                                 fluidRow(
                                   column(12, 
                                          pickerInput("selectedSitenames", "Select Sitenames", 
                                                      choices = NULL, multiple = TRUE,
                                                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                          downloadButton("downloadHMSPlot", "Download Plot"),
                                          withSpinner(plotOutput("hmsPlot", height = "600px"))
                                   )
                                 )
                        ),
                        tabPanel("Filtered Data",
                                 fluidRow(
                                   column(3,
                                          sliderInput("pm25Threshold", "PM2.5 Threshold (μg/m³)", 
                                                      min = 0, max = 100, value = 15, step = 0.1),
                                          checkboxGroupButtons("filteredSmokeIntensity", "Smoke Intensity", 
                                                               choices = c("Light", "Medium", "Heavy"),
                                                               selected = c("Medium", "Heavy")),
                                          radioButtons("tierFilter", "Filter by Tier",
                                                       choices = c("None" = "none",
                                                                   "≥ Tier 2" = "tier2",
                                                                   "≥ Tier 1" = "tier1"),
                                                       selected = "none")
                                   ),
                                   column(9,
                                          withSpinner(DTOutput("filteredDataTable")),
                                          downloadButton("downloadFilteredData", "Download Filtered Data", icon("file-download"))
                                   )
                                 )
                        )
                      )
               )
             )
    ),
    
    tabPanel("Daily AQ, Met, HMS Smoke, and Back Trajectory Analysis",
             fluidRow(
               column(2,
                      wellPanel(
                        id = "analysis_params",
                        h4("Analysis Parameters", icon("sliders-h")),
                        dateInput("date", "Select Date:", value = Sys.Date()),
                        pickerInput("state", "Select State:", choices = state.name, options = list(`live-search` = TRUE)),
                        actionButton("generate", "Generate Plots", icon("chart-line"), 
                                     class = "btn-primary btn-block"),
                        # Add contact information
                        tags$div(class = "contact-info",
                                 "Questions, comments, or bug reports?",
                                 tags$br(),
                                 "Email Rodney Cuevas:",
                                 tags$a("RCuevas@mdeq.ms.gov", href = "mailto:RCuevas@mdeq.ms.gov")
                        )
                      )
               ),
               column(10,
                      tabsetPanel(
                        tabPanel("Air Quality and Meteorological Data", 
                                 withSpinner(plotOutput("aqPlot", height = "800px", width = "100%"))
                        ),
                        tabPanel("Weather Maps", 
                                 fluidRow(
                                   column(2,
                                          selectInput("mapLevel", "Select Map Level:",
                                                      choices = c("Surface", "925mb", "850mb", "700mb", "500mb", "300mb"))
                                   ),
                                   column(10,
                                          withSpinner(plotOutput("weatherMap", height = "800px", width = "100%"))
                                   )
                                 )
                        ),
                        tabPanel("HMS Smoke", 
                                 fluidRow(
                                   column(6, withSpinner(plotOutput("hmsSmokePlotState", height = "600px", width = "100%"))),
                                   column(6, withSpinner(plotOutput("hmsSmokePlotNational", height = "600px", width = "100%")))
                                 )
                        ),
                        tabPanel("Back Trajectory",
                                 fluidRow(
                                   column(3,
                                          wellPanel(
                                            h4("Back Trajectory Analysis Instructions"),
                                            p("1. Select a date for the trajectory analysis."),
                                            p("2. Choose a state and a specific site within that state."),
                                            p("3. Set the starting heights for the trajectories."),
                                            p("4. Adjust the duration and other parameters as needed."),
                                            p("5. Click 'Generate Trajectory & Smoke' to create the plots.")
                                          ),
                                          wellPanel(
                                            h4("Trajectory Parameters", icon("route")),
                                            dateInput("trajectoryDate", "Select Date:", value = Sys.Date()),
                                            pickerInput("trajectoryStateCode", "State", choices = NULL, options = list(`live-search` = TRUE)),
                                            pickerInput("trajectorySitename", "Sitename", choices = NULL, options = list(`live-search` = TRUE)),
                                            numericInput("height1", "Height 1 (m)", value = 50),
                                            numericInput("height2", "Height 2 (m)", value = 100),
                                            numericInput("height3", "Height 3 (m)", value = 200),
                                            sliderInput("duration", "Duration (hours)", min = 1, max = 240, value = 24),
                                            checkboxGroupButtons(
                                              "daily_hours", "Daily Hours",
                                              choices = c(0:23), 
                                              selected = c(0),  # Set a default selection
                                              status = "primary",
                                              checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                              width = "100%",
                                              size = "sm"
                                            ),
                                            radioGroupButtons(
                                              "direction", "Direction",
                                              choices = c("backward", "forward"),
                                              status = "primary"
                                            ),
                                            selectInput("met_type", "Meteorology Type", 
                                                        choices = c("reanalysis", "gdas1"), 
                                                        selected = "reanalysis"),
                                            tags$style(HTML(".btn-generate-trajectory { font-size: 0.9em; white-space: normal; height: auto; }")),
                                            actionButton("generateTrajectory", "Generate Trajectory & Smoke", 
                                                         icon("play"), 
                                                         class = "btn-success btn-block btn-generate-trajectory")
                                          )
                                   ),
                                   column(9,
                                          htmlOutput("lastTrajectoryUpdate"),
                                          fluidRow(
                                            box(
                                              width = 12,
                                              title = "Trajectory Plot",
                                              status = "primary",
                                              solidHeader = TRUE,
                                              withSpinner(plotOutput("trajectoryPlot", height = "600px"))
                                            )
                                          ),
                                          fluidRow(
                                            style = "margin-top: 20px;",
                                            box(
                                              width = 12,
                                              title = "Cross Section Plot",
                                              status = "info",
                                              solidHeader = TRUE,
                                              withSpinner(plotOutput("crossSectionPlot", height = "300px"))
                                            )
                                          )
                                   )
                                 )
                        )
                      )
               )
             )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Set a higher timeout value
  options(timeout = 300)  # Set timeout to 300 seconds (5 minutes)
  
  # Add the validation function here
  validateTrajectoryInputs <- function(input) {
    if (length(input$daily_hours) == 0) {
      return("Please select at least one daily hour to run the trajectory model.")
    }
    return(NULL)
  }
  
  tiering_csv_url <- reactiveVal(NULL)
  update_timer <- reactiveTimer(24 * 60 * 60 * 1000)  # 24 hours in milliseconds
  
  
  
  # Add this function to safely get the latest URL
  safely_get_latest_url <- function() {
    tryCatch({
      url <- get_latest_tiering_csv_url()
      if (!is.null(url)) {
        print(paste("Found latest tiering CSV URL:", url))
        if (check_tiering_csv(url)) {
          return(url)
        } else {
          print("Using fallback URL instead.")
          fallback_url <- "https://www.epa.gov/system/files/other-files/2025-02/r_fire_excluded_tiers2019_2023_20250218.csv"
          check_tiering_csv(fallback_url)
          return(fallback_url)
        }
      } else {
        print("Could not find latest tiering CSV URL.")
        fallback_url <- "https://www.epa.gov/system/files/other-files/2025-02/r_fire_excluded_tiers2019_2023_20250218.csv"
        check_tiering_csv(fallback_url)
        return(fallback_url)
      }
    }, error = function(e) {
      warning("Error fetching latest tiering CSV URL: ", conditionMessage(e))
      fallback_url <- "https://www.epa.gov/system/files/other-files/2025-02/r_fire_excluded_tiers2019_2023_20250218.csv"
      check_tiering_csv(fallback_url)
      return(fallback_url)
    })
  }
  
  # Add this near the top of your server function
  trajectoryGenerated <- reactiveVal(FALSE)
  
  # Add this near the top of your server function
  values <- reactiveValues(
    lastSelectedState = NULL,
    lastSelectedSitename = NULL
  )
  
  # Add this function at the top of your server logic
  get_date_breaks <- function(date_range) {
    days_diff <- as.numeric(diff(date_range))
    
    if (days_diff <= 14) {
      list(breaks = "1 day", labels = "%Y-%m-%d", angle = 90)
    } else if (days_diff <= 60) {
      list(breaks = "1 week", labels = "%Y-%m-%d", angle = 90)
    } else if (days_diff <= 365) {
      list(breaks = "1 month", labels = "%Y-%m", angle = 45)
    } else {
      list(breaks = "3 months", labels = "%Y-%m", angle = 45)
    }
  }
  
  # Add the new fetch_asos_data function here
  fetch_asos_data <- function(state) {
    state_abbr <- state.abb[which(state.name == state)]
    url <- paste0("https://mesonet.agron.iastate.edu/sites/networks.php?network=", state_abbr, "_ASOS&format=csv&nohtml=on")
    
    tryCatch({
      asos_data <- read.csv(url, stringsAsFactors = FALSE)
      asos_data <- asos_data %>%
        select(stid, station_name, lat, lon) %>%
        rename(ICAO = stid, NAME = station_name, LAT = lat, LON = lon) %>%
        mutate(STATE = state,
               CTRY = "US")
      return(asos_data)
    }, error = function(e) {
      warning(paste("Error fetching ASOS data for", state, ":", conditionMessage(e)))
      return(NULL)
    })
  }
  
  # Load ASOS_Stations data
  ASOS_Stations <- reactive({
    req(input$state)
    withProgress(message = 'Fetching ASOS data...', value = 0, {
      fetch_asos_data(input$state)
    })
  })
  
  # Load ASOS_Stations data
  # Replace the ASOS_Stations reactive with this:
  ASOS_Stations <- reactive({
    req(input$state)
    withProgress(message = 'Fetching ASOS data...', value = 0, {
      fetch_asos_data(input$state)
    })
  })
  
  
  observe({
    update_timer()  # This will invalidate the observer every 24 hours
    
    new_url <- safely_get_latest_url()
    
    if (!is.null(new_url)) {
      current_url <- tiering_csv_url()
      if (is.null(current_url) || new_url != current_url) {
        tiering_csv_url(new_url)
        showNotification("Updated tiering data is available and will be used.", type = "message")
      }
    }
  })
  
  # Modified tier_data reactive
  # Modified tier_data reactive
  # Modified tier_data reactive to handle R's automatic space-to-dot conversion
  tier_data <- reactive({
    # Check if we already have a valid URL
    if (is.null(tiering_csv_url())) {
      # If not, fetch the latest URL
      url <- safely_get_latest_url()
      if (is.null(url)) {
        showNotification("Failed to fetch the latest tiering data URL. Using the last known URL.", type = "warning")
        url <- "https://www.epa.gov/system/files/other-files/2025-02/r_fire_excluded_tiers2019_2023_20250218.csv"
      }
      tiering_csv_url(url)
    }
    
    # Try to read the data
    tryCatch({
      print(paste("Loading tiering data from:", tiering_csv_url()))
      
      # Read the CSV - spaces in column names will be converted to dots automatically
      data <- read.csv(tiering_csv_url(), stringsAsFactors = FALSE)
      
      # Print column names to debug
      print("Original Tier Data Column Names (after R's auto-conversion):")
      print(names(data))
      
      # Check for "tier.1" and "tier.2" (converted from "tier 1" and "tier 2")
      if ("tier.1" %in% names(data) && "tier.2" %in% names(data)) {
        data <- data %>%
          rename(Tier.1 = tier.1, Tier.2 = tier.2)
        
        print("Renamed tier.1/tier.2 to Tier.1/Tier.2")
      } else {
        # Try other patterns if the expected columns aren't found
        tier1_col <- grep("tier.*1", names(data), ignore.case = TRUE, value = TRUE)
        tier2_col <- grep("tier.*2", names(data), ignore.case = TRUE, value = TRUE)
        
        print("Found other tier columns:")
        print(c(tier1_col, tier2_col))
        
        if (length(tier1_col) > 0 && length(tier2_col) > 0) {
          data <- data %>%
            rename(Tier.1 = tier1_col[1], Tier.2 = tier2_col[1])
          
          print("Renamed using pattern matching")
        } else {
          print("No tier columns found using any method")
          return(NULL)
        }
      }
      
      # Print updated column names
      print("Updated Tier Data Column Names:")
      print(names(data))
      
      # Print sample of data
      print("Sample of tier data (first 3 rows):")
      print(head(data, 3))
      
      # Check if the renaming was successful
      if (!all(c("Tier.1", "Tier.2") %in% names(data))) {
        warning("Failed to rename tier columns correctly.")
        return(NULL)
      }
      
      data %>%
        mutate(
          SITE_ID = as.character(SITE_ID),
          month = as.integer(month),
          Tier.1 = as.numeric(Tier.1),
          Tier.2 = as.numeric(Tier.2)
        )
    }, error = function(e) {
      showNotification(paste("Error reading tiering data:", e$message), type = "error")
      print(paste("Error reading tiering data:", e$message))
      return(NULL)
    })
  })
  
  
  # Reactive values
  pm25Data <- reactiveVal(NULL)
  combinedData <- reactiveVal(NULL)
  state_sf_transformed <- reactiveVal(NULL)
  
  # Add the new observe block here, after the reactive values
  observe({
    req(pm25Data())
    state_codes <- unique(pm25Data()$State_Code)
    updatePickerInput(session, "aqStateCode", choices = state_codes)
  })
  
  
  # HMS Smoke PM2.5 Analysis Server Logic
  download_airnow_data_bulk <- function(start_date, end_date) {
    date_sequence <- seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day")
    num_days <- length(date_sequence)
    
    # Create all URLs at once
    urls <- sapply(date_sequence, function(date) {
      year <- format(date, "%Y")
      yyyymmdd <- format(date, "%Y%m%d")
      paste0("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/", year, "/", yyyymmdd, "/daily_data_v2.dat")
    })
    
    # Use futures for parallel processing
    plan(multisession)  # This works on both Windows and Mac
    
    # Download data in parallel
    all_data <- future_map_dfr(urls, function(url) {
      tryCatch({
        response <- GET(url)
        if (status_code(response) == 200) {
          data <- read_delim(content(response, "text"), 
                             delim = "|", 
                             col_names = c("Valid_date", "AQSID", "Sitename", "Parameter_name", "Reporting_units", "Value", "Averaging_period", "Data_Source", "AQI_Value", "AQI_Category", "Latitude", "Longitude", "Full_AQSID"),
                             col_types = cols(
                               Valid_date = col_character(),
                               AQSID = col_character(),
                               Sitename = col_character(),
                               Parameter_name = col_character(),
                               Reporting_units = col_character(),
                               Value = col_double(),
                               Averaging_period = col_character(),
                               Data_Source = col_character(),
                               AQI_Value = col_integer(),
                               AQI_Category = col_character(),
                               Latitude = col_double(),
                               Longitude = col_double(),
                               Full_AQSID = col_character()
                             ))
          return(data)
        } else {
          return(NULL)
        }
      }, error = function(e) {
        warning(paste("Error downloading data for URL:", url, "-", conditionMessage(e)))
        return(NULL)
      })
    }, .progress = TRUE)
    
    # Process combined data
    if (!is.null(all_data) && nrow(all_data) > 0) {
      all_data <- all_data %>%
        mutate(
          Valid_date = mdy(Valid_date),
          Latitude = as.numeric(Latitude),
          Longitude = as.numeric(Longitude),
          Value = as.numeric(Value),
          State_Code = substr(AQSID, 1, 2),
          County_Code = substr(AQSID, 3, 5),
          Site_ID = substr(AQSID, 6, 9)
        ) %>%
        filter(Parameter_name == "PM2.5-24hr")
      
      if (nrow(all_data) == 0) {
        warning("No PM2.5-24hr data found after filtering.")
        return(NULL)
      }
    } else {
      warning("No data was downloaded or processed.")
      return(NULL)
    }
    
    return(all_data)
  }
  
  # Add the new observeEvent right here, after the function definition
  observeEvent(input$downloadData, {
    withProgress(message = 'Downloading AirNow data', value = 0, {
      tryCatch({
        all_data <- download_airnow_data_bulk(input$dateRange[1], input$dateRange[2])
        if (!is.null(all_data) && nrow(all_data) > 0) {
          pm25Data(all_data)
          updatePickerInput(session, "stateCode", choices = c("ALL", sort(unique(all_data$State_Code))))
          print("Data downloaded successfully")
          print(paste("Number of rows:", nrow(all_data)))
          print("Unique State Codes:")
          print(unique(all_data$State_Code))
        } else {
          showNotification("No data was downloaded. Please check your date range and try again.", type = "error")
        }
      }, error = function(e) {
        showNotification(paste("Error downloading data:", conditionMessage(e)), type = "error")
        print(paste("Error in download_airnow_data_bulk:", conditionMessage(e)))
      })
    })
  })
  
  # Usage in Shiny app
  # Event handler for downloading data
  # Reactive for trajectory AirNow data
  
  trajectoryAirNowData <- reactive({
    req(input$trajectoryDate)
    downloadTrajectoryAirNowData(input$trajectoryDate)
  })
  
  observe({
    req(pm25Data())
    tryCatch({
      state_code <- input$stateCode
      
      county_choices <- if(!is.null(state_code) && state_code != "") {
        if(state_code == "ALL") {
          c("ALL", sort(unique(pm25Data()$County_Code)))
        } else {
          c("ALL", sort(unique(pm25Data()[pm25Data()$State_Code == state_code, ]$County_Code)))
        }
      } else {
        c("ALL")
      }
      
      updatePickerInput(session, "countyCode", choices = county_choices)
    }, error = function(e) {
      warning(paste("Error in observe block:", e$message))
    })
  })
  
  observe({
    req(pm25Data(), input$stateCode, input$countyCode)
    state_code <- input$stateCode
    county_code <- input$countyCode
    
    site_choices <- if(state_code == "ALL" && county_code == "ALL") {
      c("ALL", sort(unique(pm25Data()$Site_ID)))
    } else if(state_code != "ALL" && county_code == "ALL") {
      c("ALL", sort(unique(pm25Data()[pm25Data()$State_Code == state_code, ]$Site_ID)))
    } else if(state_code != "ALL" && county_code != "ALL") {
      c("ALL", sort(unique(pm25Data()[pm25Data()$State_Code == state_code & pm25Data()$County_Code == county_code, ]$Site_ID)))
    } else {
      c("ALL")
    }
    
    updatePickerInput(session, "siteId", choices = site_choices)
  })
  
  observe({
    req(combinedData())
    updatePickerInput(session, "selectedSitenames", 
                      choices = unique(combinedData()$Sitename),
                      selected = NULL)
  })
  
  # Filtered PM2.5 Data
  filteredPM25Data <- reactive({
    req(pm25Data())
    
    tryCatch({
      data <- pm25Data()
      
      # Add debugging output
      print("PM2.5 Data Structure:")
      str(data)
      print("Number of rows in PM2.5 Data:")
      print(nrow(data))
      print("Unique State Codes:")
      print(unique(data$State_Code))
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      if (!is.null(input$stateCode) && input$stateCode != "ALL") {
        data <- data[data$State_Code == input$stateCode, ]
      }
      if (!is.null(input$countyCode) && input$countyCode != "ALL") {
        data <- data[data$County_Code == input$countyCode, ]
      }
      if (!is.null(input$siteId) && input$siteId != "ALL") {
        data <- data[data$Site_ID == input$siteId, ]
      }
      
      # Add more debugging output
      print("Filtered Data Structure:")
      str(data)
      print("Number of rows in Filtered Data:")
      print(nrow(data))
      
      if (nrow(data) == 0) {
        return(NULL)
      }
      
      data
    }, error = function(e) {
      print(paste("Error in filteredPM25Data:", conditionMessage(e)))
      NULL
    })
  })
  
  # Render data tables
  output$dataTable <- renderDT({
    req(filteredPM25Data())
    filteredPM25Data() %>%
      mutate(Valid_date = format(Valid_date, "%Y-%m-%d")) %>%
      datatable(options = list(pageLength = 15, lengthMenu = c(15, 30, 50), scrollX = TRUE, autoWidth = TRUE)) %>%
      formatStyle(columns = names(filteredPM25Data()), width = '100%')
  })
  
  output$combinedDataTable <- renderDT({
    req(combinedData())
    combinedData() %>%
      mutate(date = format(date, "%Y-%m-%d")) %>%
      select(AQSID, date, Smoke_Intensity, Averaging_period, Value, Sitename) %>%
      datatable(options = list(pageLength = 15, lengthMenu = c(15, 30, 50), scrollX = TRUE))
  })
  
  # HMS Plot
  filteredHMSData <- reactiveVal(NULL)
  
  # Add this observe block
  observe({
    req(input$dateRange, combinedData())
    filtered_data <- combinedData() %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    filteredHMSData(filtered_data)
  })
  
  # Modified output$hmsPlot
  # Update the plot function
  output$hmsPlot <- renderPlot({
    createHMSPlot()
  }, res = 96)
  
  
  # Add this new output for downloading the HMS plot
  output$downloadHMSPlot <- downloadHandler(
    filename = function() {
      # Get the start and end dates from the dateRange input
      start_date <- format(input$dateRange[1], "%Y-%m-%d")
      end_date <- format(input$dateRange[2], "%Y-%m-%d")
      
      # Create the filename using the date range
      paste0("hms_plot_", start_date, "_", end_date, ".png")
    },
    content = function(file) {
      # Capture the plot
      plot <- createHMSPlot()
      
      # Save the plot as a PNG file
      ggsave(file, plot = plot, device = "png", width = 12, height = 8, dpi = 300)
    }
  )
  
  # Create a function to generate the HMS plot
  createHMSPlot <- reactive({
    req(filteredHMSData(), input$selectedSitenames)
    
    tryCatch({
      filtered_data <- filteredHMSData() %>%
        filter(!is.na(Sitename), Sitename %in% input$selectedSitenames) %>%
        mutate(
          Averaging_period = factor(Averaging_period, levels = c(24), labels = c("24-hour")),
          Smoke_Intensity = factor(Smoke_Intensity, levels = c("Light", "Medium", "Heavy")),
          Site_ID = sub("^0", "", AQSID),
          date = as.Date(date),
          month = lubridate::month(date)
        )
      
      # Get tiering data
      tier_data_local <- tier_data()
      
      # Check if tier data is available
      if (is.null(tier_data_local)) {
        # Return a plot without tiering if tier data is unavailable
        showNotification("Tiering data is unavailable. Showing plot without tier thresholds.", type = "warning")
        
        color_mapping <- c("Light" = "lightblue", "Medium" = "darkgrey", "Heavy" = "black")
        
        date_info <- get_date_breaks(range(filtered_data$date))
        
        return(ggplot(filtered_data, aes(x = date, y = Value)) +
                 geom_point(aes(color = Smoke_Intensity), size = 3, na.rm = TRUE) +
                 geom_text(aes(label = round(Value, 1)), vjust = -1, size = 3, na.rm = TRUE) +
                 scale_color_manual(values = color_mapping) +
                 theme_minimal() +
                 theme(
                   axis.text.x = element_text(angle = date_info$angle, hjust = 1, vjust = 0.5, size = 8),
                   axis.text.y = element_text(size = 8),
                   strip.text = element_text(size = 10),
                   axis.title = element_text(size = 12),
                   legend.title = element_text(size = 10),
                   legend.text = element_text(size = 8),
                   plot.margin = margin(5, 5, 5, 5),
                   panel.spacing = unit(1, "lines")
                 ) +
                 labs(x = "Date", y = "PM2.5 (μg/m³)", color = "Smoke Intensity") +
                 facet_wrap(~ Sitename, scales = "free_y", ncol = min(3, length(input$selectedSitenames))) +
                 scale_x_date(date_breaks = date_info$breaks, 
                              date_labels = date_info$labels,
                              date_minor_breaks = "1 day") +
                 coord_cartesian(clip = "off") +
                 labs(subtitle = paste("Averaging Period:", unique(filtered_data$Averaging_period))))
      }
      
      # Check for required column names in tier data
      tier_columns <- c("SITE_ID", "month", "Tier.1", "Tier.2")
      missing_columns <- tier_columns[!tier_columns %in% names(tier_data_local)]
      
      if (length(missing_columns) > 0) {
        # Try to find alternative column names
        alt_columns <- gsub("\\.", "", missing_columns)
        for (i in seq_along(missing_columns)) {
          if (alt_columns[i] %in% names(tier_data_local)) {
            tier_data_local <- tier_data_local %>%
              rename(!!missing_columns[i] := !!alt_columns[i])
          }
        }
        
        # Check again for required columns
        missing_columns <- tier_columns[!tier_columns %in% names(tier_data_local)]
        if (length(missing_columns) > 0) {
          showNotification(paste("Missing required columns in tiering data:", 
                                 paste(missing_columns, collapse = ", ")), 
                           type = "warning")
          return(ggplot() + 
                   annotate("text", x = 0.5, y = 0.5, 
                            label = paste("Error: Missing required columns in tiering data:", 
                                          paste(missing_columns, collapse = ", "))) +
                   theme_void())
        }
      }
      
      tier_data_prepared <- tier_data_local %>%
        mutate(
          SITE_ID = as.character(SITE_ID),
          month = as.integer(month)
        )
      
      print("Filtered data structure:")
      print(str(filtered_data))
      print("Tier data structure:")
      print(str(tier_data_prepared))
      
      # Join the data
      merged_data <- filtered_data %>%
        left_join(tier_data_prepared, by = c("Site_ID" = "SITE_ID", "month"))
      
      print("Merged data structure:")
      print(str(merged_data))
      print("Sample of merged data:")
      print(head(merged_data))
      
      color_mapping <- c("Light" = "lightblue", "Medium" = "darkgrey", "Heavy" = "black")
      
      # Create a summary dataset for tier labels that handles missing values
      tier_labels <- merged_data %>%
        group_by(Sitename) %>%
        summarise(
          Tier1 = ifelse(all(is.na(Tier.1)), NA, mean(Tier.1, na.rm = TRUE)),
          Tier2 = ifelse(all(is.na(Tier.2)), NA, mean(Tier.2, na.rm = TRUE)),
          max_date = max(date)
        )
      
      date_info <- get_date_breaks(range(merged_data$date))
      
      p <- ggplot(merged_data, aes(x = date, y = Value)) +
        geom_point(aes(color = Smoke_Intensity), size = 3, na.rm = TRUE) +
        geom_text(aes(label = round(Value, 1)), vjust = -1, size = 3, na.rm = TRUE)
      
      # Only add tier lines if they exist
      if (any(!is.na(merged_data$Tier.1))) {
        p <- p + geom_line(aes(y = Tier.1), color = "red", linetype = "dashed")
      }
      
      if (any(!is.na(merged_data$Tier.2))) {
        p <- p + geom_line(aes(y = Tier.2), color = "blue", linetype = "dashed")
      }
      
      # Only add tier labels if they exist
      tier_labels_filtered <- tier_labels %>% filter(!is.na(Tier1) | !is.na(Tier2))
      
      if (nrow(tier_labels_filtered) > 0) {
        if (any(!is.na(tier_labels_filtered$Tier1))) {
          p <- p + geom_text(data = tier_labels_filtered %>% filter(!is.na(Tier1)), 
                             aes(x = max_date, y = Tier1, label = paste("Tier 1:", round(Tier1, 1))),
                             color = "red", hjust = 1, vjust = -0.5, size = 3)
        }
        
        if (any(!is.na(tier_labels_filtered$Tier2))) {
          p <- p + geom_text(data = tier_labels_filtered %>% filter(!is.na(Tier2)), 
                             aes(x = max_date, y = Tier2, label = paste("Tier 2:", round(Tier2, 1))),
                             color = "blue", hjust = 1, vjust = 1.5, size = 3)
        }
      }
      
      p + scale_color_manual(values = color_mapping) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = date_info$angle, hjust = 1, vjust = 0.5, size = 8),
          axis.text.y = element_text(size = 8),
          strip.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          plot.margin = margin(5, 5, 5, 5),
          panel.spacing = unit(1, "lines")
        ) +
        labs(x = "Date", y = "PM2.5 (μg/m³)", color = "Smoke Intensity") +
        facet_wrap(~ Sitename, scales = "free_y", ncol = min(3, length(input$selectedSitenames))) +
        scale_x_date(date_breaks = date_info$breaks, 
                     date_labels = date_info$labels,
                     date_minor_breaks = "1 day") +
        coord_cartesian(clip = "off") +
        labs(subtitle = paste("Averaging Period:", unique(merged_data$Averaging_period)))
    }, error = function(e) {
      print(paste("Error in HMS Plot:", conditionMessage(e)))
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = paste("Error:", conditionMessage(e))) +
               theme_void())
    })
  })
  
  # Filtered Data
  filteredData <- reactive({
    req(combinedData(), input$pm25Threshold, input$filteredSmokeIntensity, input$tierFilter)
    
    combined_data <- combinedData() %>%
      filter(Value >= input$pm25Threshold, Smoke_Intensity %in% input$filteredSmokeIntensity)
    
    if (input$tierFilter == "none") {
      return(combined_data %>% 
               select(AQSID, date, Smoke_Intensity, Averaging_period, Value, Sitename))
    }
    
    tier_data_local <- tier_data()
    
    if (is.null(tier_data_local)) {
      showNotification("Tiering data not available. Filtering without tier thresholds.", type = "warning")
      return(combined_data %>% 
               select(AQSID, date, Smoke_Intensity, Averaging_period, Value, Sitename))
    }
    
    # Check for required column names in tier data
    print("Column names in tier_data_local:")
    print(names(tier_data_local))
    
    # Check if Tier.1 and Tier.2 columns exist
    has_tier1 <- "Tier.1" %in% names(tier_data_local)
    has_tier2 <- "Tier.2" %in% names(tier_data_local)
    
    if (!has_tier1 || !has_tier2) {
      # Look for columns with spaces
      space_tier1_col <- grep("tier\\s+1", names(tier_data_local), ignore.case = TRUE, value = TRUE)
      space_tier2_col <- grep("tier\\s+2", names(tier_data_local), ignore.case = TRUE, value = TRUE)
      
      print("Columns with spaces found:")
      print(c(space_tier1_col, space_tier2_col))
      
      # Rename columns with spaces if found
      if (length(space_tier1_col) > 0) {
        tier_data_local <- tier_data_local %>%
          rename("Tier.1" = space_tier1_col[1])
        has_tier1 <- TRUE
      }
      
      if (length(space_tier2_col) > 0) {
        tier_data_local <- tier_data_local %>%
          rename("Tier.2" = space_tier2_col[1])
        has_tier2 <- TRUE
      }
    }
    
    if (!has_tier1 || !has_tier2) {
      showNotification("Tier columns not found in the data. Filtering without tier thresholds.", type = "warning")
      return(combined_data %>% 
               select(AQSID, date, Smoke_Intensity, Averaging_period, Value, Sitename))
    }
    
    # Prepare tier data
    tier_data_prepared <- tier_data_local %>%
      mutate(
        SITE_ID = as.character(SITE_ID),
        month = as.integer(month),
        Tier.1 = as.numeric(Tier.1),
        Tier.2 = as.numeric(Tier.2)
      )
    
    # Print some debugging info
    print("Sample of tier_data_prepared:")
    print(head(tier_data_prepared))
    
    # Join the data
    filtered_data <- combined_data %>%
      mutate(
        Site_ID = sub("^0", "", AQSID),
        month = lubridate::month(date)
      ) %>%
      left_join(tier_data_prepared, by = c("Site_ID" = "SITE_ID", "month"))
    
    # Print sample of joined data to debug
    print("Sample of filtered_data after join:")
    print(head(filtered_data))
    
    # Select columns, making sure Tier.1 and Tier.2 exist
    if (all(c("Tier.1", "Tier.2") %in% names(filtered_data))) {
      filtered_data <- filtered_data %>%
        select(AQSID, date, Smoke_Intensity, Averaging_period, Value, Sitename, Tier.1, Tier.2)
      
      # Apply tier filtering
      if (input$tierFilter == "tier2") {
        filtered_data <- filtered_data %>% 
          filter(!is.na(Tier.2) & Value >= Tier.2)
      } else if (input$tierFilter == "tier1") {
        filtered_data <- filtered_data %>% 
          filter(!is.na(Tier.1) & Value >= Tier.1)
      }
    } else {
      # In case something went wrong with the join
      showNotification("Tier columns not available after data join. Filtering without tier thresholds.", type = "warning")
      filtered_data <- combined_data %>% 
        select(AQSID, date, Smoke_Intensity, Averaging_period, Value, Sitename)
    }
    
    return(filtered_data)
  })
  
  output$filteredDataTable <- renderDT({
    req(filteredData())
    
    data_to_display <- filteredData() %>%
      mutate(date = format(date, "%Y-%m-%d"))
    
    # Check which columns are available
    available_columns <- names(data_to_display)
    numeric_columns <- intersect(c("Value", "Tier.1", "Tier.2"), available_columns)
    
    print("Columns in filtered data table:")
    print(available_columns)
    print("Numeric columns to format:")
    print(numeric_columns)
    
    # Create the datatable
    dt <- datatable(data_to_display, 
                    options = list(pageLength = 15, 
                                   lengthMenu = c(15, 30, 50), 
                                   scrollX = TRUE))
    
    # Apply formatting only if numeric columns exist
    if (length(numeric_columns) > 0) {
      dt <- formatRound(dt, columns = numeric_columns, digits = 1)
    }
    
    return(dt)
  })
  
  # Download handler for filtered data
  output$downloadFilteredData <- downloadHandler(
    filename = function() {
      start_date <- format(input$dateRange[1], "%Y-%m-%d")
      end_date <- format(input$dateRange[2], "%Y-%m-%d")
      paste0("filtered_data_", start_date, "_", end_date, ".csv")
    },
    content = function(file) {
      write.csv(filteredData() %>% 
                  mutate(date = format(date, "%Y-%m-%d")), 
                file, row.names = FALSE)
    }
  )
  
  # Process HMS Data
  observeEvent(input$processHMSData, {
    req(filteredPM25Data(), input$dateRange)
    
    withProgress(message = 'Processing HMS Smoke data', value = 0, {
      state_pm25_data <- filteredPM25Data()
      
      # Debug print
      print("PM2.5 Data before processing:")
      print(state_pm25_data %>% filter(Sitename == "GPORT YC"))
      
      if (!all(c("Latitude", "Longitude") %in% names(state_pm25_data))) {
        showNotification("Latitude and Longitude columns not found in PM2.5 data.", type = "error")
        return(NULL)
      }
      
      if (input$stateCode == "ALL") {
        state_names <- unique(state_code_to_name[unique(state_pm25_data$State_Code)])
      } else {
        state_names <- state_code_to_name[input$stateCode]
      }
      
      if (any(is.na(state_names))) {
        showNotification(paste("Invalid state code(s) found:", 
                               paste(input$stateCode[is.na(state_names)], collapse = ", ")), 
                         type = "error")
        return(NULL)
      }
      
      us_states <- maps::map("state", fill = TRUE, plot = FALSE)
      us_states_sf <- sf::st_as_sf(us_states)
      state_sf <- us_states_sf[tolower(us_states_sf$ID) %in% state_names, ]
      
      if (nrow(state_sf) == 0) {
        showNotification(paste("State(s) not found:", paste(state_names, collapse = ", ")), type = "error")
        return(NULL)
      }
      
      state_sf <- clean_geometry(state_sf)
      state_sf_transformed(sf::st_transform(state_sf, 4326))
      
      sites_sf <- st_as_sf(state_pm25_data, coords = c("Longitude", "Latitude"), crs = 4326)
      
      dates <- seq.Date(input$dateRange[1], input$dateRange[2], by = "day")
      layers <- paste0("Smoke (", input$smokeIntensity, ")")
      
      all_data <- lapply(dates, function(date) {
        incProgress(1/length(dates), detail = paste("Processing", date))
        lapply(layers, function(layer) read_kml(date, layer, state_sf_transformed(), sites_sf))
      })
      
      all_data <- do.call(rbind, Filter(Negate(is.null), unlist(all_data, recursive = FALSE)))
      
      if (is.null(all_data) || nrow(all_data) == 0) {
        showNotification(paste("No HMS Smoke data available for the selected area, dates, and smoke intensities."), type = "warning")
        return(NULL)
      }
      
      pm25_join <- state_pm25_data %>%
        select(AQSID, Valid_date, Averaging_period, Value, Sitename) %>%
        distinct()
      
      combined_data <- all_data %>%
        sf::st_drop_geometry() %>%
        select(AQSID, date, Smoke_Intensity) %>%
        distinct() %>%
        left_join(pm25_join, by = c("AQSID", "date" = "Valid_date")) %>%
        filter(!is.na(Averaging_period)) %>%
        mutate(Smoke_Intensity = gsub("Smoke \\((.*)\\)", "\\1", Smoke_Intensity))
      
      # Keep all intensities, arranged by AQSID, date, and Smoke_Intensity
      combined_data <- combined_data %>%
        arrange(AQSID, date, factor(Smoke_Intensity, levels = c("Light", "Medium", "Heavy")))
      
      print("Combined data after processing:")
      print(combined_data %>% filter(Sitename == "GPORT YC"))
      
      combinedData(combined_data)
    })
  })
  
  # Daily AQ, Met, HMS Smoke Analysis Server Logic
  # Modify the dailyData reactive
  dailyData <- eventReactive(input$generate, {
    withProgress(message = 'Generating data...', value = 0, {
      tryCatch({
        asos_data <- ASOS_Stations()
        if (is.null(asos_data)) {
          return(list(error = "Failed to fetch ASOS data for the selected state."))
        }
        
        list(
          air_quality = generate_combined_plot(input$date, input$state, asos_data, input$aqStateCode),
          hms_smoke = generate_hms_smoke_plots(input$date, input$state)
        )
      }, error = function(e) {
        list(error = paste("Error generating data:", conditionMessage(e)))
      })
    })
  })
  
  
  # Render AQ Plot
  output$aqPlot <- renderPlot({
    req(dailyData())
    if (!is.null(dailyData()$error)) {
      plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
      text(0.5, 0.5, dailyData()$error, cex = 1.5)
    } else if (is.null(dailyData()$air_quality) || !is.null(dailyData()$air_quality$error)) {
      plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
      text(0.5, 0.5, dailyData()$air_quality$error %||% "No air quality data available", cex = 1.5)
    } else {
      grid.draw(dailyData()$air_quality$air_quality_plot)
    }
  }, height = 1600, width = 1600)  # Adjust these values as needed
  
  # Render Weather Map
  output$weatherMap <- renderPlot({
    req(dailyData())
    if (!is.null(dailyData()$error)) {
      plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
      text(0.5, 0.5, dailyData()$error, cex = 1.5)
    } else if (is.null(dailyData()$air_quality) || !is.null(dailyData()$air_quality$error)) {
      plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
      text(0.5, 0.5, dailyData()$air_quality$error %||% "No weather data available", cex = 1.5)
    } else {
      selected_map <- switch(input$mapLevel,
                             "Surface" = dailyData()$air_quality$weather_map_plot,
                             "925mb" = dailyData()$air_quality$upper_air_925_plot,
                             "850mb" = dailyData()$air_quality$upper_air_850_plot,
                             "700mb" = dailyData()$air_quality$upper_air_700_plot,
                             "500mb" = dailyData()$air_quality$upper_air_500_plot,
                             "300mb" = dailyData()$air_quality$upper_air_300_plot)
      
      if (is.null(selected_map)) {
        plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
        text(0.5, 0.5, paste("No map available for", input$mapLevel), cex = 1.5)
      } else {
        map_date <- format(as.Date(input$date) + 1, "%Y-%m-%d")
        grid.arrange(selected_map, 
                     top = textGrob(paste(input$mapLevel, "Analysis -", map_date, "00Z"), 
                                    gp = gpar(fontsize = 20, font = 2)))
      }
    }
  }, height = 1200, width = 1200)  # Adjust these values as needed
  
  # Render HMS Smoke Plots
  output$hmsSmokePlotState <- renderPlot({
    req(dailyData())
    dailyData()$hms_smoke$state
  }, height = 1000, width = 1000)  # Adjust these values as needed
  
  output$hmsSmokePlotNational <- renderPlot({
    req(dailyData())
    dailyData()$hms_smoke$national
  }, height = 1000, width = 1000)  # Adjust these values as needed
  
  # Back Trajectory Analysis Server Logic
  trajectoryAirNowData <- reactiveVal(NULL)
  
  # Function to download AirNow data for a specific date
  downloadTrajectoryAirNowData <- memoise(function(date) {
    year <- format(date, "%Y")
    yyyymmdd <- format(date, "%Y%m%d")
    url <- paste0("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/", year, "/", yyyymmdd, "/daily_data_v2.dat")
    
    tryCatch({
      response <- GET(url)
      if (status_code(response) == 200) {
        data <- read_delim(content(response, "text"), 
                           delim = "|", 
                           col_names = c("Valid_date", "AQSID", "Sitename", "Parameter_name", "Reporting_units", "Value", "Averaging_period", "Data_Source", "AQI_Value", "AQI_Category", "Latitude", "Longitude", "Full_AQSID"),
                           col_types = cols(
                             Valid_date = col_character(),
                             AQSID = col_character(),
                             Sitename = col_character(),
                             Parameter_name = col_character(),
                             Reporting_units = col_character(),
                             Value = col_double(),
                             Averaging_period = col_character(),
                             Data_Source = col_character(),
                             AQI_Value = col_integer(),
                             AQI_Category = col_character(),
                             Latitude = col_double(),
                             Longitude = col_double(),
                             Full_AQSID = col_character()
                           ))
        
        data <- data %>%
          mutate(
            Valid_date = mdy(Valid_date),
            State_Code = substr(AQSID, 1, 2),
            County_Code = substr(AQSID, 3, 5),
            Site_ID = substr(AQSID, 6, 9)
          )
        
        return(data)
      } else {
        warning(paste("Failed to download data for", yyyymmdd, "- Status code:", status_code(response)))
        return(NULL)
      }
    }, error = function(e) {
      warning(paste("Error downloading data for", yyyymmdd, ":", conditionMessage(e)))
      return(NULL)
    })
  })
  
  # Update trajectoryAirNowData when date changes
  observe({
    req(input$trajectoryDate)
    data <- downloadTrajectoryAirNowData(input$trajectoryDate)
    trajectoryAirNowData(data)
  })
  
  # Update State choices for trajectory
  observe({
    req(trajectoryAirNowData())
    data <- trajectoryAirNowData()
    
    if (!"State_Code" %in% names(data)) {
      showNotification("State_Code column not found in the data.", type = "error")
      return()
    }
    
    state_codes <- unique(data$State_Code[!is.na(data$State_Code) & data$State_Code != ""])
    
    if (length(state_codes) == 0) {
      showNotification("No valid State Codes found in the data.", type = "warning")
      updatePickerInput(session, "trajectoryStateCode", choices = character(0))
      return()
    }
    
    state_names <- state_code_to_name[state_codes]
    valid_indices <- !is.na(state_names)
    
    if (sum(valid_indices) == 0) {
      showNotification("No matching state names found for the State Codes.", type = "warning")
      updatePickerInput(session, "trajectoryStateCode", choices = character(0))
      return()
    }
    
    state_choices <- setNames(state_codes[valid_indices], tools::toTitleCase(state_names[valid_indices]))
    
    # Update choices without changing the selection
    updatePickerInput(session, "trajectoryStateCode", 
                      choices = state_choices, 
                      selected = if (is.null(values$lastSelectedState)) state_choices[1] else values$lastSelectedState)
  })
  
  # Update Sitename choices for trajectory
  observe({
    req(trajectoryAirNowData(), input$trajectoryStateCode)
    data <- trajectoryAirNowData()
    
    sitenames <- data %>%
      filter(State_Code == input$trajectoryStateCode) %>%
      distinct(Sitename) %>%
      filter(!is.na(Sitename) & Sitename != "") %>%
      pull(Sitename)
    
    if (length(sitenames) == 0) {
      showNotification("No valid Sitenames found for the selected State.", type = "warning")
    }
    
    # Update choices without changing the selection if possible
    updatePickerInput(session, "trajectorySitename", 
                      choices = sitenames, 
                      selected = if (is.null(values$lastSelectedSitename) || !(values$lastSelectedSitename %in% sitenames)) sitenames[1] else values$lastSelectedSitename)
  })
  
  # Add these new observe blocks right here, after the existing ones
  observeEvent(input$trajectoryStateCode, {
    values$lastSelectedState <- input$trajectoryStateCode
  })
  
  observeEvent(input$trajectorySitename, {
    values$lastSelectedSitename <- input$trajectorySitename
  })
  
  # Get selected site information
  selectedSite <- reactive({
    req(trajectoryAirNowData(), input$trajectoryStateCode, input$trajectorySitename)
    data <- trajectoryAirNowData()
    
    site_data <- data %>%
      filter(State_Code == input$trajectoryStateCode,
             Sitename == input$trajectorySitename) %>%
      select(Latitude, Longitude) %>%
      distinct()
    
    if (nrow(site_data) > 0) {
      return(site_data)
    }
    return(NULL)
  })
  
  # Generate trajectory data
  trajectoryData <- reactiveVal(NULL)
  
  observeEvent(input$generateTrajectory, {
    req(selectedSite())
    
    # Validate inputs
    validation_message <- validateTrajectoryInputs(input)
    if (!is.null(validation_message)) {
      showNotification(validation_message, type = "error", duration = NULL)
      return()
    }
    
    site <- selectedSite()
    
    withProgress(message = 'Generating back trajectory and smoke overlay...', value = 0, {
      # Run the trajectory model for each selected hour
      trajectory_results <- lapply(as.numeric(input$daily_hours), function(hour) {
        run_trajectory_model(site$Latitude, site$Longitude, 
                             c(input$height1, input$height2, input$height3),
                             input$duration, input$trajectoryDate, 
                             hour,
                             input$direction, input$met_type)
      })
      
      # Check if any of the runs resulted in an error
      if (any(sapply(trajectory_results, function(x) x$status == "error"))) {
        showNotification(
          "Error generating one or more trajectories. Please check your parameters.",
          type = "error",
          duration = NULL
        )
        trajectoryData(NULL)
      } else {
        smoke_data <- parse_hms_smoke_data(input$trajectoryDate, "National")
        
        # Combine all trajectory results
        all_trajectories <- map_dfr(trajectory_results, function(result) {
          map_dfr(result$data, get_output_tbl, .id = "height_index")
        }, .id = "hour_index")
        
        all_trajectories <- all_trajectories %>%
          mutate(
            starting_height = c(input$height1, input$height2, input$height3)[as.numeric(height_index)],
            starting_hour = as.numeric(input$daily_hours)[as.numeric(hour_index)]
          )
        
        trajectoryData(list(
          trajectory = all_trajectories, 
          smoke = smoke_data, 
          timestamp = Sys.time(),
          date = input$trajectoryDate,
          sitename = input$trajectorySitename
        ))
        trajectoryGenerated(TRUE)
      }
    })
  })
  
  trajectoryHMSData <- eventReactive(input$generateTrajectory, {
    req(input$trajectoryDate)
    parse_hms_smoke_data(input$trajectoryDate, "National")
  })
  
  
  # Render trajectory plot with national HMS Smoke overlay
  output$trajectoryPlot <- renderPlot({
    if (!trajectoryGenerated()) {
      plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "Welcome to the Back Trajectory Analysis!\nPlease set your parameters and click 'Generate Trajectory & Smoke' to begin.", cex = 1.2, col = "blue", adj = 0.5)
    } else if (is.null(trajectoryData())) {
      plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "Error generating trajectory. Please try different parameters.", cex = 1.2, col = "red")
    } else {
      data <- trajectoryData()
      plot_date <- data$date
      all_trajectories <- data$trajectory
      
      bbox <- all_trajectories %>%
        summarize(
          lon_min = min(lon) - 5,
          lon_max = max(lon) + 5,
          lat_min = min(lat) - 5,
          lat_max = max(lat) + 5
        )
      
      states_map <- map_data("state")
      
      p <- ggplot() +
        geom_polygon(data = states_map, aes(x = long, y = lat, group = group), 
                     fill = "white", color = "black", size = 0.6)
      
      if (!is.null(data$smoke$national)) {
        smoke_data_cropped <- st_crop(data$smoke$national, 
                                      xmin = bbox$lon_min, xmax = bbox$lon_max, 
                                      ymin = bbox$lat_min, ymax = bbox$lat_max)
        
        p <- p + geom_sf(data = smoke_data_cropped, aes(fill = Density), alpha = 0.5) +
          scale_fill_manual(values = c("Light" = "lightblue", "Medium" = "grey", "Heavy" = "darkgrey"),
                            name = "Smoke Density")
      }
      
      p <- p +
        geom_path(data = all_trajectories, 
                  aes(x = lon, y = lat, 
                      color = factor(starting_height),
                      group = interaction(starting_height, starting_hour)),
                  size = 1) +
        geom_point(data = all_trajectories, 
                   aes(x = lon, y = lat, 
                       color = factor(starting_height),
                       shape = factor(starting_hour)),
                   size = 3) +
        coord_sf(xlim = c(bbox$lon_min, bbox$lon_max),
                 ylim = c(bbox$lat_min, bbox$lat_max)) +
        theme_minimal() +
        labs(title = paste("Trajectory Plot with National HMS Smoke Overlay\n",
                           "Date:", format(plot_date, "%Y-%m-%d"),
                           "- Sitename:", data$sitename),
             x = "Longitude", 
             y = "Latitude", 
             color = "Starting Height (m)",
             shape = "Starting Hour") +
        scale_color_brewer(palette = "Set1") +
        theme(
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.position = "bottom",
          plot.title = element_text(size = 16, hjust = 0.5, lineheight = 1.2)
        )
      
      state_centers <- states_map %>%
        group_by(region) %>%
        summarize(long = mean(long), lat = mean(lat))
      
      p + geom_text_repel(data = state_centers %>% 
                            filter(long >= bbox$lon_min, long <= bbox$lon_max,
                                   lat >= bbox$lat_min, lat <= bbox$lat_max),
                          aes(x = long, y = lat, label = region),
                          size = 4, alpha = 0.7)
    }
  }, height = 1000, width = 1000)
  
  output$crossSectionPlot <- renderPlot({
    if (!trajectoryGenerated()) {
      plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "Cross-section plot will appear here after generating the trajectory.", cex = 1.2, col = "blue")
    } else if (is.null(trajectoryData())) {
      plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "Error generating trajectory. Please try different parameters.", cex = 1.2, col = "red")
    } else {
      data <- trajectoryData()
      plot_date <- data$date
      all_trajectories <- data$trajectory
      
      all_trajectories <- all_trajectories %>%
        group_by(starting_height, starting_hour) %>%
        mutate(
          time_hours = as.numeric(difftime(traj_dt, first(traj_dt), units = "hours"))
        ) %>%
        ungroup()
      
      ggplot(all_trajectories, aes(x = time_hours, y = height, 
                                   color = factor(starting_height),
                                   linetype = factor(starting_hour))) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        theme_minimal() +
        labs(title = paste("Trajectory Cross-section -", format(plot_date, "%Y-%m-%d")), 
             x = "Time (hours)", 
             y = "Height (m)", 
             color = "Starting Height (m)",
             linetype = "Starting Hour") +
        scale_color_brewer(palette = "Set1") +
        theme(
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          legend.position = "bottom",
          plot.title = element_text(size = 14, hjust = 0.5),
          aspect.ratio = 0.4
        ) +
        scale_x_reverse() +
        coord_cartesian(expand = FALSE)
    }
  }, height = 400, width = 800)
  
  # In the UI, adjust the height of the crossSectionPlot:
  
  fluidRow(
    plotOutput("crossSectionPlot", height = "250px")
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

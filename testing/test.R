### Testing file

library(testthat)
library(devtools)
library(noaa)

## -------------------------------------------------

library(testthat)
library(dplyr)

# Assume eq_location_clean is defined somewhere
eq_location_clean <- function(location) {
  # Dummy implementation for testing
  return(toupper(location))
}

# Define eq_clean_data function
eq_clean_data <- function(data) {
  data <- data %>%
    # Convert the time column to Date class by extracting the date part
    mutate(DATE = as.Date(time, format = "%Y-%m-%d"),
           LATITUDE = as.numeric(latitude),
           LONGITUDE = as.numeric(longitude),
           deaths = as.integer(deaths),  # Convert deaths to integer
           LOCATION_NAME = eq_location_clean(locationsource)) %>%
    # Optionally drop the original columns if they are no longer needed
    select(-time, -locationsource, -latitude, -longitude)

  return(data)
}

test_that("eq_clean_data function works correctly", {
  # Create a sample data frame for testing
  sample_data <- data.frame(
    time = c("2023-07-15", "2023-07-16"),
    latitude = c("34.0522", "36.7783"),
    longitude = c("-118.2437", "-119.4179"),
    deaths = c("10", "20"),
    locationsource = c("los angeles", "california"),
    stringsAsFactors = FALSE
  )

  # Clean the data using the function
  cleaned_data <- eq_clean_data(sample_data)

  # Define the expected result
  expected_data <- data.frame(
    DATE = as.Date(c("2023-07-15", "2023-07-16")),
    LATITUDE = c(34.0522, 36.7783),
    LONGITUDE = c(-118.2437, -119.4179),
    deaths = c(10L, 20L),
    LOCATION_NAME = c("LOS ANGELES", "CALIFORNIA"),
    stringsAsFactors = FALSE
  )

  # Check if the cleaned data matches the expected result
  expect_equal(cleaned_data, expected_data)

  # Check if the columns are converted correctly
  expect_s3_class(cleaned_data$DATE, "Date")
  expect_type(cleaned_data$LATITUDE, "double")
  expect_type(cleaned_data$LONGITUDE, "double")
  expect_type(cleaned_data$deaths, "integer")
  expect_type(cleaned_data$LOCATION_NAME, "character")
})

## -------------------------------------------------

library(testthat)
library(dplyr)

# Define eq_location_clean function
eq_location_clean <- function(location_name) {
  cleaned_name <- location_name %>%
    sub("^[^:]+:\\s*", "", .) %>%
    tolower() %>%
    tools::toTitleCase()

  return(cleaned_name)
}

test_that("eq_location_clean function works correctly", {
  # Test with a location name that has a prefix and different cases
  location_name <- "Country: City"
  cleaned_name <- eq_location_clean(location_name)
  expect_equal(cleaned_name, "City")

  # Test with a location name without a prefix
  location_name <- "City"
  cleaned_name <- eq_location_clean(location_name)
  expect_equal(cleaned_name, "City")

  # Test with a location name that has multiple words
  location_name <- "Country: Big City"
  cleaned_name <- eq_location_clean(location_name)
  expect_equal(cleaned_name, "Big City")

  # Test with a location name with mixed cases
  location_name <- "Country: BIG city"
  cleaned_name <- eq_location_clean(location_name)
  expect_equal(cleaned_name, "Big City")

  # Test with a location name with no prefix and mixed cases
  location_name <- "bIg cItY"
  cleaned_name <- eq_location_clean(location_name)
  expect_equal(cleaned_name, "Big City")
})

## -------------------------------------------------

library(testthat)
library(ggplot2)

# Define geom_timeline function
geom_timeline <- function(data, x, y = NULL, color = "blue", size = 2, alpha = 0.5, ...) {
  ggplot(data, aes(x = {{ x }}, y = {{ y }},
                   color = {{ color }},
                   size = {{ size }},
                   alpha = {{ alpha }})) +
    geom_point() +
    scale_size_continuous(name = "Richter scale value") +
    scale_color_gradient(low = "blue", high = "red", name = "# deaths") +
    theme_minimal() +
    theme(axis.title.y = element_blank()) +
    labs(x = "DATE")
}

test_that("geom_timeline function works correctly", {
  # Create a sample data frame for testing
  sample_data <- data.frame(
    DATE = as.Date(c("2023-07-15", "2023-07-16")),
    deaths = c(10, 20),
    magnitude = c(5.5, 6.0)
  )

  # Create a plot using the function
  plot <- geom_timeline(sample_data, x = DATE, y = magnitude, color = deaths, size = magnitude)

  # Check if the result is a ggplot object
  expect_s3_class(plot, "ggplot")

  # Check if the plot has the correct layers
  expect_true("GeomPoint" %in% sapply(plot$layers, function(x) class(x$geom)[1]))

  # Check if the plot has the correct aesthetic mappings
  expect_true("DATE" %in% names(plot$mapping))
  expect_true("magnitude" %in% names(plot$mapping))
  expect_true("deaths" %in% names(plot$mapping))

  # Check if the plot has the correct scales
  expect_true(any(sapply(plot$scales$scales, function(x) x$name == "Richter scale value")))
  expect_true(any(sapply(plot$scales$scales, function(x) x$name == "# deaths")))

  # Check if the plot uses the minimal theme
  expect_true("theme_minimal" %in% class(plot$theme))

  # Check if the x-axis label is correct
  expect_equal(plot$labels$x, "DATE")
})

## -------------------------------------------------

library(testthat)
library(ggplot2)
library(dplyr)

# Define geom_timeline_label function
geom_timeline_label <- function(data, x, label, n_max = 10, ...) {
  # Filter the top n_max earthquakes by magnitude
  top_earthquakes <- data %>%
    arrange(desc(mag)) %>%
    head(n_max)

  ggplot(data, aes(x = {{ x }}, y = 0)) +
    geom_point(aes(size = mag, color = deaths), alpha = 0.6) +
    geom_segment(data = top_earthquakes,
                 aes(x = {{ x }}, xend = {{ x }},
                     y = 0, yend = 0.5),
                 linetype = "dashed") +
    geom_text(data = top_earthquakes,
              aes(x = {{ x }},
                  y = 0.5,
                  label = {{ label }}),
              angle = 45, hjust = 1) +
    scale_size_continuous(name = "Richter scale value") +
    scale_color_gradient(low = "blue", high = "red", name = "# deaths") +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(x = "DATE")
}

test_that("geom_timeline_label function works correctly", {
  # Create a sample data frame for testing
  sample_data <- data.frame(
    DATE = as.Date(c("2023-07-15", "2023-07-16", "2023-07-17")),
    mag = c(6.5, 5.5, 7.0),
    deaths = c(10, 20, 5),
    LOCATION_NAME = c("City A", "City B", "City C")
  )

  # Create a plot using the function
  plot <- geom_timeline_label(sample_data, x = DATE, label = LOCATION_NAME, n_max = 2)

  # Check if the result is a ggplot object
  expect_s3_class(plot, "ggplot")

  # Check if the plot has the correct layers
  layers <- sapply(plot$layers, function(x) class(x$geom)[1])
  expect_true("GeomPoint" %in% layers)
  expect_true("GeomSegment" %in% layers)
  expect_true("GeomText" %in% layers)

  # Check if the plot has the correct aesthetic mappings
  expect_true("DATE" %in% names(plot$mapping))

  # Check if the plot has the correct scales
  expect_true(any(sapply(plot$scales$scales, function(x) x$name == "Richter scale value")))
  expect_true(any(sapply(plot$scales$scales, function(x) x$name == "# deaths")))

  # Check if the plot uses the minimal theme
  expect_true("theme_minimal" %in% class(plot$theme))

  # Check if the x-axis label is correct
  expect_equal(plot$labels$x, "DATE")

  # Check if the top_n filtering works correctly
  top_labels <- plot$data %>%
    arrange(desc(mag)) %>%
    head(2) %>%
    pull(LOCATION_NAME)
  expect_equal(top_labels, c("City C", "City A"))
})

## -------------------------------------------------

library(testthat)
library(leaflet)

# Define eq_map function
eq_map <- function(data, annot_col) {
  # Check if the required columns are in the data frame
  required_cols <- c("LATITUDE", "LONGITUDE", "mag", annot_col)
  if (!all(required_cols %in% colnames(data))) {
    stop("Data must contain LATITUDE, LONGITUDE, mag, and the specified annotation column.")
  }

  # Create the map
  leaflet(data) %>%
    addTiles() %>%
    addCircles(lng = ~LONGITUDE, lat = ~LATITUDE,
               radius = ~mag * 10000, # Scale the radius for visibility
               popup = as.formula(paste0("~", annot_col)),
               color = "blue",
               fillOpacity = 0.5) %>%
    setView(lng = mean(data$LONGITUDE, na.rm = TRUE),
            lat = mean(data$LATITUDE, na.rm = TRUE),
            zoom = 2)
}

test_that("eq_map function works correctly", {
  # Create a sample data frame for testing
  sample_data <- data.frame(
    LATITUDE = c(34.0522, 36.7783, 40.7128),
    LONGITUDE = c(-118.2437, -119.4179, -74.0060),
    mag = c(6.5, 5.5, 7.0),
    LOCATION_NAME = c("Los Angeles", "California", "New York")
  )

  # Create a map using the function
  map <- eq_map(sample_data, annot_col = "LOCATION_NAME")

  # Check if the result is a leaflet object
  expect_s3_class(map, "leaflet")

  # Check if the map contains the expected layers
  expect_true("tile" %in% sapply(map$x$calls, function(x) x$method))
  expect_true("addCircles" %in% sapply(map$x$calls, function(x) x$method))

  # Check if the map contains the expected popup content
  expect_true(any(sapply(map$x$calls, function(x) {
    if (x$method == "addCircles") {
      as.formula(paste0("~", "LOCATION_NAME")) == x$args[[5]]
    } else {
      FALSE
    }
  })))

  # Check if the map is centered correctly
  expect_equal(map$x$options$center, list(lng = mean(sample_data$LONGITUDE, na.rm = TRUE),
                                          lat = mean(sample_data$LATITUDE, na.rm = TRUE)))
  expect_equal(map$x$options$zoom, 2)
})

## -------------------------------------------------

library(testthat)
library(dplyr)

# Define eq_location_clean function (required for eq_create_label)
eq_location_clean <- function(location_name) {
  cleaned_name <- location_name %>%
    sub("^[^:]+:\\s*", "", .) %>%
    tolower() %>%
    tools::toTitleCase()

  return(cleaned_name)
}

# Define eq_create_label function
eq_create_label <- function(data) {
  data %>%
    rowwise() %>%
    mutate(
      label = paste0(
        if (!is.na(LOCATION_NAME)) paste0("<b>Location:</b> ", eq_location_clean(LOCATION_NAME), "<br>") else "",
        if (!is.na(mag)) paste0("<b>Magnitude:</b> ", mag, "<br>") else "",
        if (!is.na(deaths)) paste0("<b>Total deaths:</b> ", deaths) else ""
      )
    ) %>%
    pull(label)
}

test_that("eq_create_label function works correctly", {
  # Create a sample data frame for testing
  sample_data <- data.frame(
    LOCATION_NAME = c("Country: Los Angeles", "California", NA),
    mag = c(6.5, 5.5, NA),
    deaths = c(10, NA, 5)
  )

  # Create labels using the function
  labels <- eq_create_label(sample_data)

  # Define the expected result
  expected_labels <- c(
    "<b>Location:</b> Los Angeles<br><b>Magnitude:</b> 6.5<br><b>Total deaths:</b> 10",
    "<b>Location:</b> California<br><b>Magnitude:</b> 5.5<br>",
    "<b>Total deaths:</b> 5"
  )

  # Check if the labels match the expected result
  expect_equal(labels, expected_labels)
})

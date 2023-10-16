
transform_metadata_to_df <- function(stations_metadata) {
  df <- 
    stations_metadata[[1]] %>%
    map(as_tibble) %>%  
    list_rbind() %>%  
    mutate(latestData = map_chr(latestData, 1, .default = "")) %>%  
    mutate(latestData = as_datetime(latestData)) %>% 
    mutate(latestData = force_tz(latestData, tzone = "UTC")) %>%
    mutate(location = map(location, unlist)) %>%   
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>% 
    select(-location)
  
  return(df)
}


stations_metadata <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)

# Function to convert an input value into ISO8601 format with specific offset
to_iso8601 <- function(datetime, offset){
  # Convert input into UTC timezone
  datetime <- as_datetime(datetime, tz = "UTC")
  
  # Check for valid inputs for conversion of datetime
  if(is.na(datetime)) stop("The conversion failed. \nEnsure your input has the correct format.")
  
  # Apply offset in days to function input
  offset_datetime <- datetime + lubridate::days(offset) 
  
  # Check that the converted datetime is not invalid
  if(is.na(offset_datetime)) stop("Your input returned invalid results. Please try again.")
  
  # Converting checked values into ISO8601 format
  time <- paste0(anytime::iso8601(offset_datetime), "Z")
  return(time)
}

transform_volumes <- function(traffic_data) {
  volume_data <- traffic_data$trafficData$volume$byHour$edges
  
  df <- volume_data %>% 
    map(function(x) {
      tibble(
        from = force_tz(as_datetime(x$node$from), tzone = "UTC"),
        to = force_tz(as_datetime(x$node$to), tzone = "UTC"),
        volume = x$node$total$volumeNumbers$volume
      )
    }) %>% 
    list_rbind()
  
  return(df)
}

test <- GQL(
  vol_qry(
    id=stations_metadata$id[1], 
    from=to_iso8601(stations_metadata$latestData[1],-4),
    to=to_iso8601(stations_metadata$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)

transform_volumes(test)

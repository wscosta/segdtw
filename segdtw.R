# load libraries
library(rgdal)
library(raster)
library(dtw)

# dont forget to change the path of your workspace
setwd("C:/segdtw")

#Sys.time()->start

threshold = 0.061


input_raster <- stack("images/example.tif")
input_vector <- as.vector(input_raster)


print(paste("Threshold =", threshold))

bands <- nlayers(input_raster)
rows <- nrow(input_raster)
cols <- ncol(input_raster)
total_size <- ncell(input_raster)

output_raster <- input_raster[[1]]
output_raster <- output_raster*0

num_seeds <- ncell(input_raster)
set.seed(001)
seeds <- sample(num_seeds)

seeds_position <- rep(list(c(0,0)), num_seeds)

for (row in seq(rows)){
  for (col in seq(cols)){
    nseed = seeds[(row-1)*cols + col] 
    seeds_position[[nseed]][1] = row
    seeds_position[[nseed]][2] = col
  }
}

neighborhood <- list()
neighborhood[[1]] <- c(1,0)
neighborhood[[2]] <- c(0,1)
neighborhood[[3]] <- c(-1,0)
neighborhood[[4]] <- c(0,-1)

visited_pixel <- matrix(FALSE, nrow = rows, ncol = cols)

seed_value = 1

for (seed in seq(num_seeds)){
  seed_pixel <- c(seeds_position[[seed]][1], seeds_position[[seed]][2])
  vector_position <- c(seed_pixel[1], seed_pixel[2])
  row <- vector_position[1]
  col <- vector_position[2]
  pos <- (row-1)*cols + col
  
  if (output_raster[seed_pixel[1], seed_pixel[2]] == 0) {
    
    current_pixel <- seed_pixel
    
    region_size <- 0
    dist <- 0.0
    
    current_region <- list()
    current_region_values <- vector()
    
    #first_band <- 1
    #last_band <- bands
    range_bands = c(1:bands)
    
    time_series_seed <- rep(0.0, length(range_bands))
    
    index <- 1
    for (band in range_bands) {
      time_series_seed[index] <- input_vector[pos + (band-1)*ncell(input_raster)]
      index <- index + 1
    }
    
    while ((dist < threshold) & (region_size < total_size)){
      region_size <- region_size + 1
      for (i in seq(length(neighborhood))){
        #new candidate
        temp_pixel <- c(current_pixel[1] + neighborhood[[i]][1],
                        current_pixel[2] + neighborhood[[i]][2])
        row <- temp_pixel[1]
        col <- temp_pixel[2]
        pos <- (row-1)*cols + col
        
        #check if candidate belongs to the image
        is_in_img <- (temp_pixel[1] >= 1) & (temp_pixel[1] <= rows) & (temp_pixel[2] >= 1) & (temp_pixel[2] <= cols)
        #check if the pixel was not visited before
        is_not_visited <- FALSE
        if (is_in_img) {
          is_not_visited <- !(visited_pixel[temp_pixel[1],temp_pixel[2]])
        }
        # candidate is analyzed only if it is not already selected before
        if (is_not_visited) {
          current_region <- c(current_region, list(temp_pixel))
          # Get time series for candidate pixel
          time_series_candidate <- rep(0.0, length(range_bands))
          index <- 1
          for (band in range_bands) {
            time_series_candidate[index] <- input_vector[pos + (band-1)*ncell(input_raster)]
            index <- index + 1
          }
          
          dtw_distance <- dtw(time_series_candidate, time_series_seed, dist.method="Manhattan", keep.internals=FALSE)$normalizedDistance
          
          current_region_values <- c(current_region_values, dtw_distance)
          # Mark candidate as visited 
          visited_pixel[temp_pixel[1], temp_pixel[2]] = TRUE
        }
      }
      
      dist_list <- current_region_values
      # Get minimum distance
      if (length(dist_list) > 0) {
        dist <- min(dist_list)
      }
      else {
        dist <- threshold
      }
      
      # Label current_pixel
      output_raster[current_pixel[1], current_pixel[2]] <- seed_value
      visited_pixel[current_pixel[1], current_pixel[2]] <- TRUE
      
      #Update seed
      if (length(dist_list) > 0) {
        index_dist <- which(dist_list == min(dist_list))[1]
        current_pixel <- current_region[[index_dist]]
      }
      current_region <- current_region[-index_dist]
      current_region_values <- current_region_values[-index_dist]
      
    }
    
    #Mark the currentPixel as not visited if the distance is greater than threshold
    if (dist > threshold) {
      visited_pixel[current_pixel[1], current_pixel[2]] <- FALSE
    }
    if (length(current_region) > 0) {
      for (i in seq(current_region)) {
        visited_pixel[current_region[[i]][1], current_region[[i]][2]] <- FALSE
      }
    }
    seed_value <- seed_value + 1
    
  }
}


title = paste("Segmentation Result\n Threshold = ",threshold)

output_name = paste("outputexample",threshold,".tiff", sep="")

writeRaster(output_raster, output_name,
            format="GTiff",  # specify output format - GeoTIFF
            overwrite=TRUE, # CAUTION: if this is true, it will overwrite an
            # existing file
            NAflag=-9999) # set no data value to -9999

#print(Sys.time()-start)

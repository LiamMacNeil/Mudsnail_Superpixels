################# ################# #################
#
# CAVEAT EMPTOR.
#
# Image segmentation developed by Liam MacNeil and fit-for-purpose of a specific research project. 
# All methods build on existing open-source packages cited below. 
#
################# ################# #################

library(SuperpixelImageSegmentation)
library(autothresholdr)
library(tidyverse)
library(OpenImageR)
library(jpeg)
library(EBImage)
library(profvis)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###########
 

#flatfieldImage <- readImage("../Datasets/Background/SFempty_Hydrobia_02.tif")
#correctionMatrix = mean(flatfieldImage) / flatfieldImage

#flatfieldImage_grey <- EBImage::channel(flatfieldImage, "red")
#correctionMatrix_grey = mean(flatfieldImage_grey) / flatfieldImage_grey


dir.create("../Datasets/Hydrobia_full/Outputs/Processed/Hydrobia_SF71_amb_1_C3_superpix_processed")
dir.create("../Datasets/Hydrobia_full/Outputs/Segments/Hydrobia_SF71_amb_1_C3_superpix_segments")
dir.create("../Datasets/Hydrobia_full/Outputs/Features/Hydrobia_SF71_amb_1_C3_superpix_segment_features")

### Full function
convert_batch <- function(x){
  
  ### Read in images by x
  mask <- readImage(paste0("../Datasets/Hydrobia_full/Inputs/Hydrobia_SF71_amb_1_C3/", x))
  # mask <- mask*correctionMatrix
  
  
  #####
  # Stage 1 : Image Processing
  #####
  
  mask <- gblur(mask, sigma = 8)
  mask <- EBImage::normalize(mask)
  
  set.seed(seed = 1)
  # Write processed images
  writeJPEG(mask, target = paste0("../Datasets/Hydrobia_full/Outputs/Processed/Hydrobia_SF71_amb_1_C3_superpix_processed/", 
                                  strsplit(x, ".", fixed=T)[[1]][[1]], ".tif"), quality = 1)
  
  #####
  # Stage 2 : Segmentation
  #####
  
  init = Image_Segmentation$new()
  
  spx = init$spixel_segmentation(input_image = mask, 
                                 superpixel = 1500, 
                                 AP_data = TRUE,
                                 use_median = TRUE, 
                                 sim_wL = 3, 
                                 sim_wA = 10, 
                                 sim_wB = 10,
                                 sim_color_radius = 20, 
                                 kmeans_method = "mini_batch_kmeans",
                                 kmeans_initializer = "kmeans++",
                                 kmeans_num_init = 3, 
                                 kmeans_max_iters = 100,
                                 minib_kmeans_batch = 10, 
                                 minib_kmeans_init_fraction = 0.75,
                                 return_labels_2_dimensionsional = TRUE,
                                 adjust_centroids_and_return_masks = TRUE,
                                 verbose = TRUE)
  
  clusters = spx$spix_labels
  
  # This is the key: Correctly choosing which clusters contain segments 
  # If only 2 clusters are defined by spixel_segmentation, usually 1 is background (set to 0) and 2 is segments
  # When >2 are defined (Try to reduce with high value of sim_color_radius), then this needs to be checked carefully and multiple clusters should be set to background, 
  # sometimes features are composites of clusters. They are beautifully captured when this is the case.
  
  #clusters[clusters %in% 1:2] = 0
  clusters[clusters %in% c(1,2,3)] = 0
  
  # and the remaining segments to 1
  clusters[clusters != 0] = 1
  
  img_segs <- EBImage::Image(clusters)
  
  final = fillHull(watershed(distmap(img_segs), ext = 1, tolerance = 1000))
  
  plot(colorLabels(final))
  
  writeJPEG(colorLabels(final), target = paste0("../Datasets/Hydrobia_full/Outputs/Segments/Hydrobia_SF71_amb_1_C3_superpix_segments/", 
                                                strsplit(x, ".", fixed=T)[[1]][[1]], ".tif"), quality = 1)
  
  #####
  # Stage 3 : Saving features as .csv, ensure filename contains metadata to match master (test) file
  #####
  
  gc()
  
  write.csv(as.data.frame(computeFeatures.shape(final)) %>%
              mutate(across(all_of(names(as.data.frame(computeFeatures.shape(final)))), ~ .x /83)) %>%
              mutate(pixels.size = (s.radius.max*2)*83) %>% 
              mutate(size..mm. = s.radius.max*2) %>% 
              filter(size..mm. < 7.5) %>% 
              filter(size..mm. > 0.9) %>% 
              mutate(Sample = paste0(x)), 
            paste0("../Datasets/Hydrobia_full/Outputs/Features/Hydrobia_SF71_amb_1_C3_superpix_segment_features/", x, ".csv"))
  
}


sapply(dir("../Datasets/Hydrobia_full/Inputs/Hydrobia_SF71_amb_1_C3/"), convert_batch)

# profvis() function to retrieve memory stats

########## Applying to single image where errors occur

## A * note is present for each unique directory 

# *
wd_in <- "../Datasets/Hydrobia_full/Inputs/Hydrobia_SF71_amb_1_C3/Hydrobia_SF71_amb_1_C3_16.tif"
img <- EBImage::readImage(wd_in)

# Ensure same seetings as batch, otherwise make note
img_grey <- gblur(img, sigma = 8)
img_grey_norm <- EBImage::normalize(img_grey)

init = Image_Segmentation$new()

set.seed(seed = 123)

spx = init$spixel_segmentation(input_image = img_grey_norm, 
                               superpixel = 1500, 
                               AP_data = TRUE,
                               use_median = TRUE, 
                               sim_wL = 3, 
                               sim_wA = 10, 
                               sim_wB = 10,
                               sim_color_radius = 20, 
                               kmeans_method = "mini_batch_kmeans",
                               kmeans_initializer = "kmeans++",
                               kmeans_num_init = 3, 
                               kmeans_max_iters = 100,
                               minib_kmeans_batch = 10, 
                               minib_kmeans_init_fraction = 0.75,
                               return_labels_2_dimensionsional = TRUE,
                               adjust_centroids_and_return_masks = TRUE,
                               verbose = TRUE)

clusters = spx$spix_labels

# set these to background (or to the value of 0)
clusters[clusters %in% c(1,2)] = 0

# and the remaining segments to 1 to delimit ROIs
clusters[clusters != 0] = 1

img_segs <- EBImage::Image(clusters)

# Check!
final = fillHull(watershed(distmap(img_segs), ext = 1, tolerance = 1000))

plot(colorLabels(final))

# Write corrected segments to folder 
# *
writeJPEG(colorLabels(final), 
          target = paste0("../Datasets/Hydrobia_full/Outputs/Segments/Hydrobia_SF71_amb_1_C3_superpix_segments/Hydrobia_SF71_amb_1_C3_16.tif"),
          quality = 1)

# Write features
write.csv(as.data.frame(computeFeatures.shape(final)) %>%
            mutate(across(all_of(names(as.data.frame(computeFeatures.shape(final)))), ~ .x /83)) %>%
            mutate(pixels.size = (s.radius.max*2)*83) %>% 
            mutate(size..mm. = s.radius.max*2) %>% 
            filter(size..mm. < 7.5) %>% 
            filter(size..mm. > 0.9) %>% 
            # *
            mutate(Sample = paste0("Hydrobia_SF71_amb_1_C3_16.tif")), 
          # *
          paste0("../Datasets/Hydrobia_full/Outputs/Features/Hydrobia_SF71_amb_1_C3_superpix_segment_features/Hydrobia_SF71_amb_1_C3_16.tif.csv"))


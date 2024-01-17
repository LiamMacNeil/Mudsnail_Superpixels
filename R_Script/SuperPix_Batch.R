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
Segmentation <- function(x){
  
  ### Read in images by x
  mask <- readImage(paste0("../Datasets/Hydrobia_full/Inputs/Hydrobia_SF49_three_0_B4/", x))
  # mask <- mask*correctionMatrix
  
  
  #####
  # Stage 1 : Image Processing
  #####
  
  mask <- gblur(mask, sigma = 8)
  mask <- EBImage::normalize(mask)
  
  set.seed(seed = 1)
  # Write processed images
  writeJPEG(mask, target = paste0("../Datasets/Hydrobia_full/Outputs/Processed/Hydrobia_SF49_three_0_B4_superpix_processed/", 
                                  strsplit(x, ".", fixed=T)[[1]][[1]], ".tif"), quality = 1)
  
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
  
  # In some image profiles (high background illumination), it is helpful to lower sim_color_radius or reduce number of superpixels  
  # It is optional to subselect clusters to refine  segments as features are composites of clusters
  # If the background is not properly delimited by the segmentation algorithm:
  
  # This option is here (choose clusters to define as zero)
  #clusters[clusters %in% c(1,2)] = 0
  #clusters[clusters != 0] = 1
  
  clusters = spx$spix_labels
  img_segs <- EBImage::Image(clusters)
  
  # Edge-based (edge detection)
  #final <- fillHull(bwlabel(EBImage::Image(edge_detection(clusters, method = "Frei_chen"))))
  
  # Threshold-based (binary)
  #final = fillHull(bwlabel(distmap(img_segs)))
  
  # Region-based (watershed)
  final = fillHull(watershed(distmap(img_segs), ext = 1, tolerance = 1000))
  
  plot(colorLabels(final))
  
  writeJPEG(colorLabels(final), target = paste0("../Datasets/Hydrobia_full/Outputs/Segments/Hydrobia_SF49_three_0_B4_superpix_segments/", 
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
            paste0("../Datasets/Hydrobia_full/Outputs/Features/Hydrobia_SF49_three_0_B4_superpix_segment_features/", x, ".csv"))
  
}


sapply(dir("../Datasets/Hydrobia_full/Inputs/Hydrobia_SF49_three_0_B4/"), Segmentation)

# profvis() function to retrieve memory stats

########## Applying to single image where errors occur

## A * note is present for each unique directory 

# *
wd_in <- "../Datasets/Hydrobia_full/Inputs/Hydrobia_SF49_three_0_B4/Hydrobia_SF49_three_0_B4_8.tif"
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
clusters[clusters %in% c(1,3,6)] = 0

# and the remaining segments to 1 to delimit ROIs
clusters[clusters != 0] = 1

img_segs <- EBImage::Image(clusters)

# Check!
final = fillHull(watershed(distmap(img_segs), ext = 1, tolerance = 1000))

plot(colorLabels(final))

# Write corrected segments to folder 
# *
writeJPEG(colorLabels(final), 
          target = paste0("../Datasets/Hydrobia_full/Outputs/Segments/Hydrobia_SF49_three_0_B4_superpix_segments/Hydrobia_SF49_three_0_B4_8.tif"),
          quality = 1)

# Write features
write.csv(as.data.frame(computeFeatures.shape(final)) %>%
            mutate(across(all_of(names(as.data.frame(computeFeatures.shape(final)))), ~ .x /83)) %>%
            mutate(pixels.size = (s.radius.max*2)*83) %>% 
            mutate(size..mm. = s.radius.max*2) %>% 
            filter(size..mm. < 7.5) %>% 
            filter(size..mm. > 0.9) %>% 
            # *
            mutate(Sample = paste0("Hydrobia_SF49_three_0_B4_8.tif")), 
          # *
          paste0("../Datasets/Hydrobia_full/Outputs/Features/Hydrobia_SF49_three_0_B4_superpix_segment_features/Hydrobia_SF49_three_0_B4_8.tif.csv"))

######
###### Sandbox
###### 
clusters <- if_else(clusters[clusters] > 2 ,  
                    matrix(clusters[clusters %in% 1] <- 0), 
                    matrix(clusters[clusters %in% 1:2] <- 0))

if (clusters[clusters] < 3) {
  (clusters[clusters %in% 1] = 0 & clusters[clusters != 0] = 1) 
} else if (clusters[clusters] > 2) {
  clusters[clusters %in% 1:2] = 0 & clusters[clusters != 0] = 1
}

######
###### 
######  

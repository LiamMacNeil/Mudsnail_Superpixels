# Mudsnail image segmentation with superpixels

[![DOI](https://zenodo.org/badge/642758934.svg)](https://zenodo.org/doi/10.5281/zenodo.10522503)

<p float="left">
  <img src="https://github.com/LiamMacNeil/Mudsnail_Superpixels/blob/main/Ex/Github_ex.png" width="900" />
</p>


This is an accompanying respository (R script and raw data files) for our [paper] describing an application for image segmentation, based on clustering superpixels, to enumerate and size (length) mudsnails from common stereomicroscopy images. This method expedited counting and measuring tremendously and yielded data-rich, reproducible data products to assess the growth response of a common mudsnail under experimental warming treatments.

<b>R_Script folder:</b>


<i>Superpix_Batch.R</i> - Running segmentation on entire folders

<i>Evaluation_MultiMethod.R</i> - % overlap of kernel density estimations across segmentation algorithms (threshold-based, edge-based, region-based)

<i>Evaluation_*spx.R</i> - % overlap of kernel density estimations using different numbers of superpixels

<i>Features.R</i> - Plotting segmentation size estimates across full dataset (>42k individuals)

<i>Size_glm.R</i> -Supplement: Generalized linear-mixed model (GLMM) of size estimates derived from segmentation across full dataset (>42k individuals)

<i>PCA_Treatment.R</i> -Supplement: Ordinating morphological features derived from segmentation size estimates across full dataset (>42k individuals)

<b>Reference:</b>

MacNeil, Liam; Joly, Léa J; Ito, Maysa; Steinmann, Anna; Mehler, Knut; Scotti, Marco (2023): Rapid segmentation and sizing of an abundant mudsnail: Applying superpixels to scale accurate detection of growth response to ocean warming. <i>Methods in Ecology and Evolution</i>.

See the [PANGAEA database](https://doi.pangaea.de/10.1594/PANGAEA.957929) for all raw input images, processed images, and output segments.

MacNeil, Liam; Joly, Léa J; Ito, Maysa; Steinmann, Anna; Drakula, Maja; Wiegand, Hannes; Morsbach, Samuel; Karnatz, Josefine; Scotti, Marco (2024): Hydrobiid mudsnail image dataset: Raw stereomicroscope, processed, and segmented images of abundant brackish snails from a mesocosm experiment with multiple temperature treatments. PANGAEA, https://doi.org/10.1594/PANGAEA.957929

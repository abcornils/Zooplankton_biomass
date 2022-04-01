# Zooplankton_biomass

This repository holds the scripts used for the publication "Testing the usefulness of optical data for zooplankton long-term monitoring: Taxonomic composition, abundance, biomass and size spectra from ZooScan image analysis" by Cornils et al. (2022). The five R scripts transform the exported data of EcoTaxa dataset https://ecotaxa.obs-vlfr.fr/prj/2771 to abundance, biovolume and biomass estimates in the following order:

The script "PS78_ecotaxa_export_transformation.R" transforms the exported data from project https://ecotaxa.obs-vlfr.fr/prj/2771. It adjust station data, adds manual counts for images with multiple objects (ps78_multiples_ident.txt) and calculates abundance and biovolume.

To calculate biomass we used different approaches. For the description of the methodology and formulas please refer to the manuscript. 

1. PS78_transect_biomass_CF16.R
Using established conversion factors to transform biovolume (wet mass) to dry mass. 

2. PS78_transect_biomass_LW_relationship.R
Using Length-Mass relationships to estimate biomass. 
For better results we established relationships between ESD and length with the script "PS78_transect_PL_ESD_relationship.R" and provide the file "PS78_length_all.tsv" for that. Afterwards we used established Length-Mass relationships for Arctic zooplankton taxa.

3. The script "PS78_transect_Calanus_biomass.R" estimates biomass of Calanus spp. using different approaches and compares them to direct measurements of dry mass (PANGAEA doi).

If you use this code or the data please cite the manuscript:
Astrid Cornils, Karolin Thomisch, Joanna Hase, Nicole Hildebrandt, Holger Auel, Barbara Niehoff (2022) Testing the usefulness of optical data for zooplankton long-term monitoring: Taxonomic composition, abundance, biomass and size spectra from ZooScan image analysis. Limnology and Oceanography: Methods, in review

# Isotopes and Modern Coexistence Theory (MCT)
Using stable isotopes for empirical applications of modern coexistence theory (MCT)

This code is associated with the unpublished manuscript:

`Buschke, F.T. & Codron, D. Empirical applications of Modern Coexistence Theory using dietary niche information from stable isotopes`

It is up-to-date as of 11 December 2020. For queries contact Falko Buschke `falko.buschke@gmail.com`

## File structure

This repository is made up of three files:

* `Conceptual_diagram.R`
This contains the R-code needed to replicate the conceptual diagrams in Figure 1 and Figure S1

* `MCT_coexistence_biplots.R`
This include the R-code needed to replicate Figures 2 and 3 in the main manuscript, which show the equalising and stabilising components of pair-wise coexistence of species. Relies on data from the data file `Isotope_data.txt`

* `Isotope_data.txt`
This is the raw data, which includes data for the isoptopic dietary niches of five mammamls from Kruger national Park, South Africa.

The `Isotope_data.txt` dataset includes multiple columns gnerated from stable isotope analyses, but only the following columns are relevant for this study:

  * `community`, which includes the month in which the community was sampled.
  * `parCol`, which is a dummy variable for a pair of species. This is used to plot different symbols in Figure 2.
  * `BM_i` and `BM_j`, which is the body mass of species *i* and *j* respectively.
  * `Rmax_i` and `Rmax_j`, which is the maximum grwoth rate of species *i* and *j* respectively.
  * `SEAi_mean` and `SEAj_mean`, which is the total dietary niche of species *i* and *j* respectively (I and J).
  * `Olap` is the niche overlap between the pair of species (I *interesect* J)
  


# BENTHIS_2020: BENTHIS WP2 Workflow revisited.

* Input data are EFLALO and TACSAT data formats (see VMStools R library on github) and need to be put in the EflaloAndTacsat folder. Two very small example are provided.

* Workflow itself is split in two parts:
** BenthisWorkflow2020.r is used to obtain a gridded swept area from interpolating bottom contacting gears tracks,
** couplingInterpolatedVmsToLandings2020.r is of use to obtain spatial origin of catches (landings) by dispatching landings back to interpolated VMS positions

* quickmap2020.r provides a routine for mapping the gridded data per fishery and species

* 2 example shell scripts to run on a High Performance Computing (HPC) service are also provided as
such a workflow can be both time and memory consuming

* Background map in use was made with Natural Earth. Free vector and raster map data @ naturalearthdata.com

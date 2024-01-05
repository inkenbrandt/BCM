# BCM
Unofficial repository for the USGS Basin Characterization Model by Flint and Flint

All of the attached and below were downloaded from https://www.sciencebase.gov/catalog/item/5ff8e4f9d34e52c3b3d9d53a 

This work is by:
Flint, L.E., Flint, A.L., and Stern, M.A., 2021, The Basin Characterization Model - A Regional Water Balance Software Package, US Geological Survey Techniques and Methods 6-H1, 85 p. [https://pubs.usgs.gov/publication/tm6H1](https://doi.org/10.3133/tm6H1)

This report was prepared in cooperation with California Department of Water Resources   

In support of this document, the Basin Characterization Model v8 (BCMv8) is archived according to California Water Science Center guidelines.

# Package Description
BCM_montlyv8.exe is the executable for the model. The program is written in FORTRAN and all input and output 
files are ASCII flat files, with exactly the same number of rows and columns for model operation. 
The projection is NAD83 California Teale Albers, meters. 

The grid cell size is 270-m, model domain is hydrologic California (all watersheds draining into California). 
Because of the large size and number of the files, only water years (WYs) 2018-2019 have been archived. All input climate variables and water balance output variables
for WYs 2018-2019 are archived. 

# Running the model
Download input files from https://www.sciencebase.gov/catalog/item/5ff8e4f9d34e52c3b3d9d53a 

To run the Basin Characterization Model:

1) Unzip files "climate_WY2018-WY2019.7z",  "InputFiles.zip", and "BCM-Monthlyv8.7z".
2) Check to see that there are 24 files for each of ppt, tmn, tmx, and pet, and that each has data, to ensure that all files copied and unzipped correctly.
	All input and output files are in ascii format. Input files must be in the same directory as the BCM executable and control file (should already be there).
3) Open the control file in a text program.
	Change the folder location path pointing to the climate variable files on line 28 in the control file to ensure it is pointing to the correct drive and folder location.
	Note: climate files can also be located in the same directory as the input files and executable if the switch on line 30 in the control file is set to 0 (off).
4) Open a DOS window (command prompt) and navigate to directory with the input files.
5) Type BCMv8_Monthlyv8 and enter. 

Note: When the model is run with a grid of basins and a table file the output time series files (*.out) will include all input and output variables averaged (temperature) 
or added (all others) for each basin.

All output maps, along with the time series output in the monthly and yearly *.out files will be printed to the InputFiles directory.

The 2-year run takes approximately 58 minutes to run from an external solid state drive (USB3) on a desktop computer with the following specifications:
Windows 10 Enterprise
Processor: Intel(R) Xeon(R) CPU E5-2687W v3 @ 3.10 GHz (2 processors)
Installed memory (RAM): 40 GB
System type: 64-bit Operating System, x64-based processor

# Archive structure

There are 3 directories in the archive.

(1) InputFiles- includes all input data and climate files necessary to run the BCM for water years 2018-2019.
	Climate_WY2018-WY2019: Air temperature data (tmn and tmx) is in degrees C, precipitation (ppt) and potential evapotranspiration (pet) data are
	in millimeters per month.
	File naming convention: e.g. xxx2018dec.asc, xxx (variable name), 2018 (year), dec (December)
	Variables: ppt (monthly precipitation), tmx (maximum monthly air temperature), tmn (minimum monthly air temperature), pet (potential evapotranspiration).

        InputFiles.7z: All input maps are the California model domain, 270-meters resolution, and NAD83 CA Teale Albers meters projection.
		ca_270m_v8.asc                Digital elevation model (DEM) 270-meters grid size                                                                                                                                                                                                                                                                                                                                  
		ca_thck4_v8.asc               SSURGO estimated soil thickness (meters), 4 meter maximum depth.                                                                                                                                                                                                                                                                                                                            
		ca_mp6000_v8.asc              Water content at wilting point calculated using Saxton and Rawls equation and soil texture (cm water/cm soil).                                                                                                                                                                                                                                                                                                        
		ca_mp0010_v8.asc              Water content at field capacity calculated using Saxton and Rawls equation and soil texture (cm water/cm soil).                                                                                                                                                                                                                                                                                                    
		ca_por0_v8.asc                Total soil porosity (v/v) estimated using Saxton and Rawls equation.                                                                                                                                                                                                                                                                                                                                    
		ca_kss0_v8.asc                Soil saturated hydraulic conductivity.                                                                                                                                                                                                                                                                                                                                                    
		ca_geolid_v8.asc              California geology map identifier corresponding to lookup table in BCM control file.                                                                                                                                                                                                                                                                                                  
		ca_whr_fullveg62.asc          California vegetation types including urban development from FRAP map corresponding to lookup table in BCM control file.                                                                                                                                                                                                                                                        
		ca_ksr0_v8.asc                Estimated bedrock saturated hydraulic conductivity (mm/day). File "ctl_rockks.asc" is created with the table data matched to the "ca_geolid_v8.asc" file.                                                                                                                                                                                                                                  
		ca_snowaccum_v8.asc           Snow accumulation temperature parameter map.                                                                                                                                                                                                                                                                                                                                                    
		ca_mfmax_v8.asc               Maximum melt factor parameter map.                                                                                                                                                                                                                                                                                                                                                              
		ca_mfmin_v8.asc               Minimum melt factor parameter map.                                                                                                                                                                                                                                                                                                                                                             
		ca_aridity_v8.asc             Aridity index used to dry out soils below wilting point                                                                                                                                                                                                                                                                                                                          
		ca_mask_v8.asc                Optional mask for blocking out areas in printed maps and output summarizing.  
                            
(2) Output- includes output files for water years 2018-2019 in millimeters per month, zipped by variable.
	File naming convention: e.g. xxx2018dec.asc, xxx (variable name), 2018 (year), dec (December)
	Variables: aet (actual evapotranspiration), climatic water deficit (cwd), snow water equivalent (pck), 
                   rch (recharge), run (runoff), and soil storage (str).


(3) Source- includes the BCMv8 source code (.f90), executable (.exe), and control file (.ctl).

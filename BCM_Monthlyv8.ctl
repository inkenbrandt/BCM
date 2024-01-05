BCM_Monthlyv8.ctl: California domain model using the Teale Albers NAD83 projection                                                                                                                                                                                                                                                                                                                                          
!----- File Name Length (30)-><--- OPERATIONS: I/O and startup information --- keep description comment at space 31 ----------------------------------------------------------------                                                                                                                                                                                                        
ca_mo_v8_huc8.out             !monthly output file                                                                                                                                                                                                                                                                                                                                                          
ca_yr_v8_huc8.out             !yearly output file                                                                                                                                                                                                                                                                                                                                                           
ca_huc8_basins.asc            !grid file for map areas and summarized output time series                                                                                                                                                                                                                                                                                                                     
ca_huc8_basins.tbl            !table for map areas                                                                                                                                                                                                                                                                                                                                                           
2018                          !Beginning water year for model run   #### LIST WATER YEARS TO PRINT MAPS line 229 ####                                                                                                                                                                                                                                                                                        
2019                          !Ending water year for model run                                                                                                                                                                                                                                                                                                                                               
0                off=0,on=1   !ANTECEDENT conditions switch, if this is a new run with no prior data use 0, if you have soil storage and snowpack for prior year use 1                                                                                                                                                                                                                                          
0.5              If ANTECEDENT off INITIALIZE soil-water content, multiply difference between wilting point and field capacity for initial soil-water content, 0.0 is wilting point                                                                                                                                                                                                                                       
!----- File Name Length (30)-><--- REQUIRED LAYER files: dem, soils, geology, and vegtypes (all files exactly match the dem) -----------------------------------------------                                                                                                                                                                                                                                    
ca_270m_v8.asc                !DEM California Teale Albers 270 meter ca domain                                                                                                                                                                                                                                                                                                                                  
ca_thck4_v8.asc               !SSURGO estimated soil depth (m) with added soil depth                                                                                                                                                                                                                                                                                                                            
ca_mp6000_v8.asc              !WILTING POINT Saxton and Rawls estimated wilting point (cm water/cm soil)                                                                                                                                                                                                                                                                                                        
ca_mp0010_v8.asc              !FIELD CAPACITY Saxton and Rawls estimated field capacity (cm water/cm soil)                                                                                                                                                                                                                                                                                                      
ca_por0_v8.asc                !POROSITY Saxton and Rawls estimated porosity                                                                                                                                                                                                                                                                                                                                     
ca_kss0_v8.asc                !SOIL Hydraulic Conductivity                                                                                                                                                                                                                                                                                                                                                       
ca_geolid_v8.asc              !GEOLOGY ID California geology map identifier corresponding to lookup table below                                                                                                                                                                                                                                                                                                 
ca_fullveg62.asc              !VEGETATION TYPE California vegetation types including urban development from FRAP map corresponding to LOOKUP table 1 below                                                                                                                                                                                                                                                        
!----- File Name Length (30)-><--- OPTIONAL LAYER files: snow parameters, aridity for soil dryout, mask -------------------------------------------------------------------                                                                                                                                                                                                                                     
ca_ksr0_v8.asc                !Estimated bedrock saturated hydraulic conductivity (mm/day) File "ctl_rockks.asc" is created with the table data matched to the geology id file                                                                                                                                                                                                                                  
ca_snowaccum_v8.asc           !Snow accumulation temperature                                                                                                                                                                                                                                                                                                                                                    
ca_mfmax_v8.asc               !Maximum melt factor                                                                                                                                                                                                                                                                                                                                                              
ca_mfmin_v8.asc               !Minimum melt factor                                                                                                                                                                                                                                                                                                                                                              
ca_aridity_v8.asc             !Aridity index used to dry out soils below wilting point                                                                                                                                                                                                                                                                                                                          
ca_mask_v8.asc                !Mask for blocking out areas in printed maps and output summarizing                                                                                                                                                                                                                                                                                                               
!----- File Name Length (45)----------------><--- OPTIONAL input location switch below: Directory location of climate files for pet, ppt, tmn and tmx
I:\Programs\BCMv8\Climate_WY2018-WY2019\     !Enter climate file location including the ending back slash, for example D:\ClimateDirectory\
!----- BELOW use an on-off SWITCH to:        See comment at end of each SWITCH for replicating published v65 output
1                    off=0,on=1 ; Read pet, ppt, tmn, tmx files from another directory, otherwise the files are read from the same directory where the BCM is running (off for v65)
0                    off=0,on=1 ; GEOL Bedrock conductivity map is used, if 0 then use LOOKUP TABLE 1 for bedrock conductivity, if 1 then provide map (see ctl_rockks.asc, line 21)
1  0.65 -10.00 0.05  off=0,on=1 ; SOIL DRYDOWN scaler, for exponential equation y=a*exp(b*aridity)+c to shape the curve to slow dry down. Uses the aridity map. (off for v65)
1                    off=0,on=1 ; PT, Modified Priestley-Taylor equation, when on then AET is reduced by soil dryness from Flint and Childs (1986) (off for v65)
1  0.300             off=0,on=1 ; RCH/RUN scaler, to enhance recharge and runoff (0.5 means 50% increase or decrease) moves rch to run or run to rch (off for v65)
0  53  0.1  0.5      off=0,on=1 ; URBAN, urban vegtype ID, urban soil depth (m), urban bedrock K (mm/day), 999 keeps original bedrock K, 999 keeps original soil depth (off for v65)
0                    off=0,on=1 ; SNOW to use a single value for snow accumulation and melt, otherwise use the maps supplied (on for v65)
3.5                                 Snow accumulation temperature (degrees C)
1.8                                 Maximum melt factor
0.4                                 Minimum melt factor
7.0 0.1 0.21                        Default(maf,tipm,nmf) monthly adjustment factor for the sine function from Snow-17
1                    off=0,on=1 ; SOLAR, if on adds solar radiation loading to enhance snowmelt (on for v65)
0                    off=0,on=1 ; RAIN fraction, if on when it snows reduces PET to % of PPT that is rain (on for v65; ONLY USED TO REPLICATE BCMv65)
0  0.00              off=0,on=1 ; SUBLIMATION, if off uses an equation relating PET and sublimation. If on, uses literature constant (1.4-8.5 mm/mon) (off for v65)
0                    off=0,on=1 ; MASK, use a mask to block out areas in printed maps and summarized outfile time series (off for v65)
0                    off=0,on=1 ; FLOOD, set to one to read in the 12 monthly maps where managed recharge is to be located (off for v65)
0                    off=0,swe=1,str=2,lai=3 ; INGEST, use data for snow water equivalent(SWE),soil moisture status (str),leaf area index (lai)
2                                   Number of substitute pairs to ingest (off for v65)
<------File to be replaced goes here-----------------------><-------Replacement file goes here----------->
pck2019mar.asc                                              aso2019mar.asc
pck2019apr.asc                                              aso2019apr.asc
!----- LOOKUP TABLE 1: geology and bedrock permeability ----------------------------------------------------------------------------------------------------------------------
54                     Number of rock types in bedrock geology map
!rock ID  ks (mm/day)  Geologic Type  
!------   -----------  -------------
1           500.00     alluvium - ash 
2           200.00     alluvium - channels 
3           500.00     alluvium - aeolian sand 
4            40.00     alluvium - glacial till
5            10.00     alluvium - desert fill 
6             0.27     alluvium - lake sediments 
7           500.00     alluvium - landslides 
8            55.00     alluvium - marshes 
9             2.74     alluvium - mud and salt flats 
10           20.00     alluvium - older upland soils 
11            0.82     alluvium - playas 
12          200.00     alluvium - valley fill 
13          100.00     Carbonates - dolomite 
15           90.00     Carbonates - limestone
16            0.27     Carbonates - travertine 
17            1.00     Chert 
18           20.00     Conglomerate 
19            0.50     Gabbro 
20           15.00     Granite
21            0.60     Granite - granodiorite 
22           40.00     Granite - mixed 
23           14.00     Granite - quartz monzonite 
24           27.40     Igneous - diabase 
25            0.27     Igneous - dikes and plugs 
26            2.00     Metamorphics - gneiss/schist 
27            2.00     Metamorphics - phyllite 
28           38.00     Metamorphics - serpentinite 
29            2.00     Metasediments 
30            1.50     Metavolcanics 
31            0.05     Quartzite 
32            6.00     Sandstone 
42            0.10     Sandstone - claystone 
43            1.00     Sandstone - fine 
44            3.00     Sandstone - shale 
45            0.90     Sandstone - siltstone 
46            2.00     Sedimentary - coastal belt 
47            2.00     Sedimentary 
48            0.27     Volcanics - andesites 
49            1.00     Volcanics - andesites (flows and breccias) 
50            6.00     Volcanics - ash-flow tuffs 
51           32.00     Volcanics - basalts
52            0.27     Volcanics - breccias 
53            6.00     Volcanics - lava flows
54            0.00     No Geology
55            8.00     Volcanics - lava flows (Quaternary) 
56           20.00     Volcanics - pyroclastics 
57            0.50     Volcanics - rhyolites 
58            0.00     Water 
59            1.20     Sandstone - claystone mélange 
60            0.30     Sandstone - shale Eocene 
61            0.10     Sandstone - shale lower 
62            0.20     Sandstone - shale upper 
63           15.00     Volcanics - lava flows Tertiary 
64          200.00     Sandstone - Santa Margarita 
!----- LOOKUP TABLE 2: vegetation types and actual evapotranspiration parameters ---------------------------------------------------------------------------------------------------------------------------------------------
62                   Number of vegetation types in the vegetation map 
!----- vegetation density and growth parameters --- ---------------- Monthly Kfactors (PET/AET) order by water year month) ----------------------- --- Vegetation Type -----------------------------------------------------  
VegID InitLAI UpLimit  DnLimit  UpRate  DnRate  RootDepth   oct      nov      dec      jan      feb      mar      apr      may      jun      jul      aug      sep     WHR
!---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------                                                                                                                  
1     1.000    2.000    0.750    1.500    0.850    0.250    0.214    0.139    0.048    0.077    0.212    0.262    0.426    0.365    0.340    0.213    0.197    0.227    Alpine-Dwarf Shrub
3     1.000    3.400    0.650    1.800    0.500    2.000    0.219    0.176    0.308    0.246    0.658    0.858    0.860    0.681    0.601    0.576    0.063    0.358    Annual Grassland
4     1.000    2.500    0.500    1.500    0.950    0.000    0.066    0.086    0.125    0.142    0.193    0.445    0.568    0.626    0.516    0.391    0.290    0.164    Alkali Desert Scrub
5     1.000    3.000    0.750    1.750    0.750    0.500    0.079    0.218    0.119    0.045    0.024    0.184    0.454    0.448    0.361    0.407    0.332    0.127    Aspen
6     1.000    1.700    0.900    1.100    0.950    0.250    0.045    0.078    0.138    0.170    0.115    0.163    0.175    0.215    0.213    0.240    0.198    0.113    Barren
7     1.000    2.200    0.850    1.250    0.850    0.000    0.022    0.137    0.302    0.303    0.207    0.342    0.184    0.191    0.222    0.170    0.098    0.036    Bitterbrush
8     1.000    1.500    0.900    1.400    0.750    1.250    0.121    0.131    0.123    0.170    0.149    0.337    0.448    0.621    0.558    0.496    0.413    0.276    Blue Oak-Foothill Pine
9     1.000    2.500    0.750    1.500    0.500    1.300    0.078    0.120    0.173    0.188    0.206    0.533    0.650    0.734    0.582    0.467    0.342    0.189    Blue Oak Woodland
10    1.000    1.500    0.900    1.000    0.950    1.250    0.130    0.227    0.311    0.382    0.352    0.491    0.515    0.488    0.421    0.327    0.270    0.158    Coastal Oak Woodland
11    1.000    1.050    0.950    1.025    0.975    1.500    0.415    0.348    0.233    0.417    0.510    0.676    0.722    0.897    0.949    0.875    0.819    0.627    Closed-Cone Pine-Cypress
12    1.000    2.300    0.950    1.700    0.750    2.100    0.170    0.180    0.250    0.320    0.410    0.760    0.790    0.880    0.910    0.810    0.700    0.520    Chamise-Redshank Chaparral
13    1.000    1.700    0.900    1.400    0.800    1.500    0.059    0.062    0.146    0.177    0.211    0.353    0.346    0.307    0.262    0.230    0.171    0.097    Coastal Scrub
14    1.000    1.400    0.950    1.200    0.975    2.000    0.496    0.457    0.339    0.556    0.521    0.693    0.647    0.797    0.820    0.874    0.864    0.717    Douglas Fir
15    1.000    1.000    1.000    1.000    1.000    0.500    0.352    0.350    0.384    0.292    0.330    0.584    0.574    0.820    1.062    1.208    1.090    0.776    Desert Riparian
17    1.000    2.500    0.950    2.000    0.100    1.000    0.440    0.340    0.371    0.244    0.274    0.486    0.504    0.714    0.840    0.840    0.714    0.645    Desert Scrub
18    1.000    2.800    0.750    1.025    0.975    0.000    0.001    0.005    0.012    0.023    0.026    0.056    0.021    0.005    0.002    0.002    0.004    0.006    Desert Succulent Shrub
19    1.000    1.500    0.950    2.100    0.300    1.500    0.043    0.042    0.197    0.224    0.129    0.231    0.184    0.188    0.217    0.215    0.189    0.119    Desert Wash
20    1.000    1.400    0.850    1.200    0.950    0.500    0.067    0.182    0.188    0.342    0.272    0.449    0.330    0.390    0.371    0.394    0.321    0.167    Eastside Pine
21    1.000    1.000    1.000    1.000    1.000    0.000    0.451    0.235    0.139    0.241    0.402    0.369    0.550    0.589    0.752    0.753    0.744    0.651    Estuarine
22    1.000    1.000    1.000    1.000    1.000    0.000    0.333    0.321    0.241    0.189    0.327    0.391    0.367    0.497    0.562    0.516    0.466    0.419    Fresh Emergent Wetland
24    1.000    1.650    0.950    1.250    0.900    1.500    0.112    0.086    0.131    0.272    0.221    0.449    0.395    0.435    0.428    0.450    0.409    0.288    Jeffrey Pine
25    1.000    3.200    0.950    1.500    0.975    1.800    0.625    0.566    0.166    0.404    0.338    0.658    0.574    0.604    0.629    0.625    0.625    0.625    Joshua Tree
26    1.000    3.000    0.900    1.250    0.850    1.500    0.014    0.131    0.133    0.230    0.216    0.283    0.185    0.277    0.215    0.162    0.090    0.032    Juniper
27    1.000    1.250    0.900    1.250    0.975    1.000    0.280    0.236    0.204    0.392    0.353    0.547    0.544    0.640    0.620    0.696    0.668    0.533    Klamath Mixed Conifer
28    1.000    1.000    1.000    1.000    1.000    0.000    0.654    0.628    0.551    0.778    0.808    0.926    0.858    0.951    0.954    0.922    0.910    0.852    Lacustrine
29    1.000    1.500    1.000    1.000    0.850    0.750    0.083    0.144    0.126    0.191    0.151    0.428    0.569    0.613    0.438    0.419    0.403    0.239    Lodgepole Pine
30    1.000    2.500    0.850    1.800    0.850    0.400    0.040    0.052    0.028    0.099    0.092    0.191    0.303    0.136    0.106    0.111    0.109    0.086    Low Sage
32    1.000    1.300    0.900    1.100    0.750    0.750    0.126    0.107    0.172    0.222    0.257    0.437    0.472    0.576    0.551    0.482    0.379    0.245    Mixed Chaparral
34    1.000    1.400    0.800    1.500    0.750    0.750    0.096    0.136    0.141    0.224    0.185    0.363    0.381    0.506    0.484    0.504    0.436    0.276    Montane Chaparral
35    1.000    1.400    0.950    1.500    0.975    1.250    0.398    0.472    0.379    0.533    0.448    0.589    0.610    0.755    0.793    0.832    0.803    0.609    Montane Hardwood-Conifer
36    1.000    1.500    0.900    1.000    0.750    1.000    0.270    0.224    0.158    0.227    0.282    0.471    0.534    0.741    0.758    0.718    0.652    0.471    Montane Hardwood
37    1.000    1.000    1.000    1.000    1.000    0.500    0.188    0.162    0.225    0.311    0.297    0.404    0.408    0.578    0.641    0.663    0.585    0.396    Montane Riparian
39    1.000    2.500    0.750    1.500    0.750    0.750    0.034    0.105    0.127    0.227    0.242    0.377    0.298    0.377    0.314    0.277    0.193    0.099    Perennial Grassland
40    1.000    1.750    0.950    1.200    0.975    1.200    0.034    0.044    0.118    0.173    0.152    0.248    0.148    0.218    0.225    0.212    0.157    0.077    Pinyon-Juniper
42    1.000    1.400    0.950    1.250    0.900    1.400    0.214    0.171    0.116    0.232    0.273    0.477    0.437    0.609    0.631    0.646    0.607    0.472    Ponderosa Pine
43    1.000    1.000    1.000    1.000    1.000    0.000    0.444    0.305    0.250    0.243    0.426    0.630    0.658    0.764    0.856    0.873    0.856    0.718    Riverine
44    1.000    1.300    0.980    1.200    0.950    1.400    0.485    0.474    0.363    0.438    0.475    0.565    0.561    0.668    0.731    0.777    0.807    0.657    Redwood
45    1.000    1.500    0.850    1.350    0.800    0.750    0.101    0.133    0.121    0.212    0.193    0.482    0.591    0.689    0.520    0.513    0.478    0.324    Red Fir
48    1.000    1.500    0.850    1.350    0.900    0.750    0.089    0.122    0.098    0.163    0.125    0.347    0.462    0.529    0.410    0.394    0.378    0.215    Subalpine Conifer
49    1.000    1.000    1.000    1.000    1.000    0.000    0.023    0.093    0.386    0.653    0.272    0.084    0.056    0.117    0.114    0.100    0.065    0.032    Saline Emergent Wetland
50    1.000    2.500    0.900    1.150    0.850    1.500    0.017    0.138    0.298    0.034    0.266    0.838    0.900    0.890    0.881    0.487    0.686    0.619    Sagebrush
51    1.000    1.100    0.800    1.000    0.900    0.500    0.206    0.167    0.147    0.282    0.308    0.554    0.491    0.580    0.569    0.594    0.572    0.455    Sierran Mixed Conifer
53    1.000    1.500    0.950    1.200    1.000    0.500    0.121    0.100    0.239    0.216    0.176    0.247    0.305    0.355    0.411    0.402    0.339    0.199    Urban
55    1.000    1.750    0.950    1.500    0.750    0.750    0.063    0.058    0.087    0.093    0.108    0.218    0.267    0.363    0.317    0.275    0.212    0.132    Valley Oak Woodland
56    1.000    1.000    1.000    1.000    1.000    0.500    0.302    0.284    0.329    0.339    0.353    0.470    0.524    0.692    0.744    0.725    0.684    0.567    Valley Foothill Riparian
57    1.000    1.000    1.000    1.000    1.000    0.000    0.610    0.513    0.391    0.598    0.660    0.788    0.692    0.827    0.896    0.963    0.957    0.910    Water
58    1.000    1.100    0.800    1.000    0.975    0.750    0.212    0.202    0.174    0.391    0.367    0.633    0.600    0.657    0.568    0.622    0.607    0.474    White Fir
59    1.000    1.000    1.000    1.000    1.000    0.500    0.069    0.230    0.410    0.236    0.191    0.494    0.375    0.540    0.638    0.687    0.507    0.236    Wet Meadow
60    1.000    1.000    1.000    1.000    1.000    0.000    0.063    0.110    0.163    0.201    0.223    0.269    0.249    0.317    0.328    0.299    0.243    0.157    Cropland
66    1.000    1.000    1.000    1.000    1.000    0.000    0.067    0.075    0.127    0.117    0.194    0.351    0.324    0.311    0.410    0.509    0.491    0.316    Dryland Grain Crops
67    1.000    1.000    1.000    1.000    1.000    0.000    0.202    0.177    0.162    0.108    0.161    0.284    0.344    0.483    0.582    0.603    0.568    0.440    Deciduous Orchard
68    1.000    1.000    1.000    1.000    1.000    0.000    0.164    0.150    0.162    0.104    0.142    0.243    0.245    0.362    0.443    0.468    0.446    0.368    Evergreen Orchard
69    1.000    1.000    1.000    1.000    1.000    0.000    0.168    0.181    0.201    0.197    0.229    0.275    0.264    0.392    0.549    0.641    0.585    0.409    Irrigated Grain Crops
70    1.000    1.000    1.000    1.000    1.000    0.000    0.052    0.062    0.109    0.097    0.120    0.209    0.188    0.237    0.376    0.444    0.388    0.245    Irrigated Row and Field Crops
71    1.000    1.000    1.000    1.000    1.000    0.000    0.187    0.174    0.203    0.231    0.290    0.405    0.429    0.495    0.599    0.630    0.599    0.442    Irrigated Hayfield
72    1.000    1.250    0.900    1.250    0.900    0.500    0.052    0.108    0.205    0.252    0.201    0.269    0.304    0.309    0.282    0.218    0.180    0.099    Pasture
75    1.000    1.000    1.000    1.000    1.000    0.000    0.194    0.210    0.186    0.151    0.162    0.243    0.268    0.368    0.465    0.481    0.452    0.346    Vineyard
77    1.000    2.000    0.850    1.350    0.750    0.500    0.040    0.092    0.083    0.134    0.172    0.255    0.279    0.332    0.260    0.210    0.170    0.092    Eucalyptus
78    1.000    1.000    1.000    1.000    1.000    0.000    0.223    0.229    0.238    0.181    0.296    0.252    0.201    0.568    0.880    0.908    0.908    0.784    Rice
79    1.000    1.000    1.000    1.000    1.000    0.000    0.700    0.578    0.234    0.306    0.426    0.580    0.500    0.517    0.532    0.422    0.463    0.558    Marsh
80    1.000    2.000    0.800    1.500    0.900    0.250    0.056    0.114    0.098    0.094    0.153    0.335    0.373    0.370    0.281    0.235    0.169    0.084    Rock Cliff Scree
81    1.000    1.500    1.000    1.250    0.900    0.000    0.080    0.182    0.289    0.421    0.148    0.161    0.127    0.271    0.219    0.140    0.128    0.124    Unknown
!----- LIST of average monthly precipitation for actual evapotranspiration adjustments (1981-2010)--------------------------------------------------------------------------------
ca_pptaveoct.asc
ca_pptavenov.asc
ca_pptavedec.asc
ca_pptavejan.asc
ca_pptavefeb.asc
ca_pptavemar.asc
ca_pptaveapr.asc
ca_pptavemay.asc
ca_pptavejun.asc
ca_pptavejul.asc
ca_pptaveaug.asc
ca_pptavesep.asc
ca_pptaveann.asc
!----- LIST of optional average monthly solar radiation files for melting snow -------------------------------------------------------------------------------------------------------------
ca_radaveoct.asc
ca_radavenov.asc
ca_radavedec.asc
ca_radavejan.asc
ca_radavefeb.asc
ca_radavemar.asc
ca_radaveapr.asc
ca_radavemay.asc
ca_radavejun.asc
ca_radavejul.asc
ca_radaveaug.asc
ca_radavesep.asc
!----- LIST of optional average monthly flood maps for managed aquifer recharge -------------------------------------------------------------------------------------------------------------
ca_fldaveoct.asc
ca_fldavenov.asc
ca_fldavedec.asc
ca_fldavejan.asc
ca_fldavefeb.asc
ca_fldavemar.asc
ca_fldaveapr.asc
ca_fldavemay.asc
ca_fldavejun.asc
ca_fldavejul.asc
ca_fldaveaug.asc
ca_fldavesep.asc
!----- WATER YEARS TO PRINT OUT MAPS -------------------------------------------------------------------------------------------------------------------------------------------
125                !number of water years to print out                                                                                                                                                                                                                                                                                                                                                           
1896
1897
1898
1899
1900
1901
1902
1903
1904
1905
1906
1907
1908
1909
1910
1911
1912
1913
1914
1915
1916
1917
1918
1919
1920
1921
1922
1923
1924
1925
1926
1927
1928
1929
1930
1931
1932
1933
1934
1935
1936
1937
1938
1939
1940
1941
1942
1943
1944
1945
1946
1947
1948
1949
1950
1951
1952
1953
1954
1955
1956
1957
1958
1959
1960
1961
1962
1963
1964
1965
1966
1967
1968
1969
1970
1971
1972
1973
1974
1975
1976
1977
1978
1979
1980
1981
1982
1983
1984
1985
1986
1987
1988
1989
1990
1991
1992
1993
1994
1995
1996
1997
1998
1999
2000
2001
2002
2003
2004
2005
2006
2007
2008
2009
2010
2011
2012
2013
2014
2015
2016
2017
2018
2019
2020
!--- END of Control File -------------------------------------------------------------------------------------------------------------------------------------------------------
!Add notes below-- Additional information located below to assist in cutting and pasting print out years
'----- WATER YEAR LIST ----------------------------------------------------------------------------------------------------------------------------------------------------------
historical years below
124
1896
1897
1898
1899
1900
1901
1902
1903
1904
1905
1906
1907
1908
1909
1910
1911
1912
1913
1914
1915
1916
1917
1918
1919
1920
1921
1922
1923
1924
1925
1926
1927
1928
1929
1930
1931
1932
1933
1934
1935
1936
1937
1938
1939
1940
1941
1942
1943
1944
1945
1946
1947
1948
1949
1950
1951
1952
1953
1954
1955
1956
1957
1958
1959
1960
1961
1962
1963
1964
1965
1966
1967
1968
1969
1970
1971
1972
1973
1974
1975
1976
1977
1978
1979
1980
1981
1982
1983
1984
1985
1986
1987
1988
1989
1990
1991
1992
1993
1994
1995
1996
1997
1998
1999
2000
2001
2002
2003
2004
2005
2006
2007
2008
2009
2010
2011
2012
2013
2014
2015
2016
2017
2018
2019
!
Future years below
100
2001
2002
2003
2004
2005
2006
2007
2008
2009
2010
2011
2012
2013
2014
2015
2016
2017
2018
2019
2020
2021
2022
2023
2024
2025
2026
2027
2028
2029
2030
2031
2032
2033
2034
2035
2036
2037
2038
2039
2040
2041
2042
2043
2044
2045
2046
2047
2048
2049
2050
2051
2052
2053
2054
2055
2056
2057
2058
2059
2060
2061
2062
2063
2064
2065
2066
2067
2068
2069
2070
2071
2072
2073
2074
2075
2076
2077
2078
2079
2080
2081
2082
2083
2084
2085
2086
2087
2088
2089
2090
2091
2092
2093
2094
2095
2096
2097
2098
2099
2100

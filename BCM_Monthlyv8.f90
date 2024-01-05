     program BCM_Monthlyv8
!    version v5.3 added month adjustment factor (maf, for the sine function) 
!    version v5.4 added soil porosity and field capacity changes for the soil organic matter study supported by the Marin Carbon Project
!    version v6.0 adds control file to the annual output file
!    version v6.1 turns petmon(k) to zero for recharge calc when its colder than the snow acculmulation temperature and sets it back after the recharge calcs 
!    version v6.2 takes solartmp out the snow accumulations equation
!    version v6.3 adds snow file from JPL
!    version v6.4 changes the control file to have more upfront file selection
!    version v6.5 changes snowpack ati and deld so that it only effects the model when snowpack exists (error in accumulating ati and deld in the air and applying it to the pack the first time it shows up
!    version v6.5 also added the reduction of stor below wp.  Cut and pasted from v6.3 which had it, however 6.4 didn't
!    version v7.0 adds a additional soil layer based on geologic type to expand the depth water can penetrate and still be removed by ET
!    version v7.0 fixed the problem of not writing output to table if tavemax=0
!    version v7.1 adds several layers for enhanced analysis vegtype, lai, monthly kfact, roots (extra soil depth), allows for AET when it's cold (a fix from converstion with Adrian Harpold)
!    version v7.2 adds several layers for snowaccum, mfmax, mfmin based on Michelle's cokriging against snow course data (101 stations) in California, urban soil and urban bedrock k is added and a gaining and loosing stream for wetter or drier months
!    version v7.3 adds average radiation files for snowmelt
!    version v7.4 remove the limits on cwd if kfactor or lai is so low that it can et with small amounts of water.  THis keeps soil drydown more honest.
!    version v7.5 this allow a single snow accumulation temperature and max and min melt factor for calibration or small areas outside of California    
!    version v7.6 allows for the injestion of snow pack data from JPL or ASO coverages.  It uses BCM data for any cells not covered by JPL or ASO. istor added to set initial conditions
!    version v7.7 reorganized control file 5/31/2018
!    version for new BCM_Monthly
!    version v8.0 add climate directory for pet, ppt, tmn, and tmx so you can opt to not overwrite previous output with different climate runs (6/13/2019, Alan Flint and Michelle Stern)
     double precision xll,yll,cellsize,nodata1
     integer nodata2,nodata3i,nodataswe,recover(10000,10000)
     integer cols,rows,ngeol,nveg,iveg,ndy(1895:2101,12),veg(10000),numingest,storyear,begyear,storcount,yearstorun,notsequenced
     character*200 header,header2
     character*400 headerout(800)
     character*14 dum1
     character*60 digestfile(0:150),ingestfile(0:150)
     character*45 ClimateDirFile
     character*30 pack,packfile(0:150)
     character*75 petfile(150),tmaxfile(150),pptfile(150),tminfile(150)
     character*30 snowfile(150),tavgfile(150),meltfile(150),lai_pptfile(12),averad_file(12),flood_file(12),smdfile(150),smrfile(150)
     character*20 sublfile(150),storfile(0:150),exc1file(0:150),aet1file(0:150),cwd1file(0:150),demfile,rch3file(150),run2file(150),geolfile,atifile(0:150),deffile(0:150),laifile(0:150),wtbfile(0:150),evpfile(0:150)
     character*30 soildfile,fcfile,porfile,wpfile,soilksfile,rockksfile,vegfile,aapptfile
     character*30 outfile,outfile2,snowaccumfile,mfmaxfile,mfminfile,aridityfile,maskfile  
     double precision sublpar1,meltpar1,maxrch
     double precision pptmon(10000),pptavemon(10000),laiscaler(10000),petmon(10000),vegpetmon(10000),tmaxmon(10000),snowmon(10000),meltmon(10000),sublmon(10000),exc1mon(10000),cwd1mon(10000)
     double precision subdata(0:16000),packmon(0:16000),priorpackmon(10000),stormon(0:16000),priorstormon(10000),aet1mon(10000),tminmon(10000),rch3mon(10000),run2mon(10000),atimon(0:16000),defmon(0:16000),laimon(0:16000)
     double precision smdmon(0:16000),smrmon(0:16000),aveppt(10000),alan(10000)    
     integer geolid(10000),urbanveg,iyear,imon
     double precision lastlai(6000),kfactor(12,500),watbal(10000),evapmon(10000),aridity(10000),mask(10000)
     double precision soild(10000),wp(10000),fc(10000),por(10000),soilks(10000),drydown(10000),rchrunscaler(0:16000),avgrchrunscaler(0:16000),rchrunscaleryr(0:16000),urbansoild,urbanks
     double precision fcmm(10000),pormm(10000),wpmm(10000),elev(10000),geolks(10000),vegsoil(400),initveglai(400),distlai(400),initlai(10000),uplailimit(400),dnlailimit(400),uplairate(400),dnlairate(400)
     double precision pptyr(0:10000),petyr(0:10000),tminyr(0:10000),snowyr(0:10000),meltyr(0:10000),sublyr(0:10000),exc1yr(0:10000),packyr(0:10000),rch3yr(0:10000)
     double precision cwd1yr(0:10000),run2yr(0:10000),aet1yr(0:10000),evapyr(0:10000),watbalyr(0:10000),storyr(0:10000),tmaxyr(0:10000),tavgyr(0:10000),avgppt(0:10000),avgpet(0:10000),avgsnow(0:10000)
     double precision avgmelt(0:10000),avgpack(0:10000),avgexc1(0:10000),avgaet1(0:10000),avgrch3(0:10000),avgcwd1(0:10000),avgrun2(0:10000),avgsubl(0:10000),avgevap(0:10000),avgwatbal(0:10000),avglaiscaler(0:10000)
     double precision avgstor(0:10000),avgtmax(0:10000),avgtmin(0:10000),avgtavg(0:10000),ratio(0:10000),rch3acft(0:10000),run2acft(0:10000),avgsmd(0:5000),avgsmr(0:5000),smdyr(0:5000),smryr(0:5000)
!    Snow melt parameters for Snow-17 model
     double precision mf,mp,mfmax(10000),mfmin(10000),maf,tipm,nmf,nmfac,pi,tempmf,solartmp,solarrad,rchrunlimit,snowaccumt,maxmf,minmf
     double precision d(0:12),deld(12),ati(0:12),janrad(10000),febrad(10000),marrad(10000),aprrad(10000),mayrad(10000),junrad(10000),julrad(10000),augrad(10000),seprad(10000),octrad(10000),novrad(10000),decrad(10000)
     double precision flood(5000),floodwater(5000,5000),gradient
     double precision recharge,runoff,istor,TmaxC,TminC,TavgC,Trange,snowfrac,rainfrac,maxiwc,rchstor,runstor,snowaccum(10000),alphaprime,thetarel
     character*3 mon
     character*30 ascfile
     character*60 ascfile2
     character*30 infile
     integer lpyr(1895:2101),ybeg,yend,a,aprior,i,iprior,j,k,ii,jj,ik,ij,jk,alphaprimeflag,drydownflag,urbanflag,maskflag,snowflag,ingestflag,sub,rockksflag,floodflag,ClimateDirFlag
     integer rockid(500),flag,temp1,temp2,temp3,temp4,temp5,temp6,rect,startflag
     double precision rockks(0:500),rocksoil(0:500),inpnodat(9)
!    declarations for rectangular area statistics 
     integer n,nareas,rrowb(0:10000),rcolb(0:10000),rrowe(0:10000),rcole(0:10000),totcells(0:10000),nposexc1(0:10000),nposaet1(0:10000),nposrch3(0:10000),nposcwd1(0:10000)
     double precision totarea
!    declarations for basin area statistics
     character*30 areafile,areatable
     integer iarea(0:10000),area(10000),outstore(1894:2101),coutstore,barea
!    declarations to change input grids by a additive scaler or multiplier
     integer porscaler,rchrunflag,wppercent,wpscaler,soildpercent,geolksscaler
     double precision scalsoild,scalpor,scalfc,scalwp,persoild,perpor,perfc,perwp,scalrks
!     ---  set costants ---
     pi=3.14159265358979
     do i=1895,2101
      outstore(i)=0
     enddo
5    format(A)
6    format(2A60)
     open(unit=88,file='pcknames.txt')
     open(unit=98,file='strnames.txt')
     open(unit=99,file='atinames.txt')
     open(unit=100,file='defnames.txt')
     open(unit=123,file='lainames.txt')
     open(unit=7,file='BCM_Monthlyv8.ctl')
     read(7,5) header2
     Write(*,*)'Running BCM_Monthlyv8 (11/6/2018) with the following control file header:'
     Write(*,*)''
     Write(*,*)header
     read(7,5) header2
     !<--- OPERATIONS:  I/O and startup information------------------------------------------------------------------------------------------------
     read(7,5) outfile
     read(7,5) outfile2
     read(7,5) areafile
     read(7,5) areatable
     read(7,*) ybeg
     read(7,*) yend
     read(7,*) startflag
     read(7,*) istor
     read(7,*) header
     !<--- REQUIRED LAYER files: dem, soils, geology, and vegtypes (all files exactly match the dem) -----------------------------------------------
     read(7,5) demfile
     read(7,5) soildfile
     read(7,5) wpfile
     read(7,5) fcfile
     read(7,5) porfile
     read(7,5) soilksfile
     read(7,5) geolfile
     read(7,5) vegfile
     read(7,*) header
     read(7,5) rockksfile
     read(7,5) snowaccumfile
     read(7,5) mfmaxfile
     read(7,5) mfminfile
     read(7,5) aridityfile     
     read(7,5) maskfile
     read(7,5) header2
     read(7,5) ClimateDirFile
     read(7,5) header2
     !----- BELOW use an on-off SWITCH
     read(7,*) ClimateDirflag
     read(7,*) rockksflag
     read(7,*) drydownflag,arida,aridb,aridc
     read(7,*) alphaprimeflag
     read(7,*) rchrunflag,rchrunlimit
     read(7,*) urbanflag,urbanveg,urbansoild,urbanks     
     read(7,*) snowflag
     read(7,*) snowaccumT
     read(7,*) maxmf
     read(7,*) minmf
     read(7,*) maf,tipm,nmf
     read(7,*) solarflag
     read(7,*) rainfracflag
     read(7,*) sublflag,sublpar1
     read(7,*) maskflag
     read(7,*) floodflag
     read(7,*) ingestflag
     read(7,*) numingest
     read(7,5) header2
    do i=1,numingest
     read(7,6)digestfile(i),ingestfile(i)
    enddo
     read(7,5) header2
     !----- LOOKUP TABLE: geology and bedrock permeability ----------------------------------------------------------------------------------------------------------------------     
     read(7,*) ngeol
     read(7,5) header2
     read(7,5) header2
    do i = 1,ngeol
     read(7,*) rockid(i),rockks(rockid(i))
    enddo
     read(7,5) header2
     !----- LOOKUP TABLE: vegetation types and actual evapotranspiration parameters ----------------------------------------------------------------------------------   
     read(7,*) nveg
     read(7,5) header2
     read(7,5) header2
     read(7,5) header2
     !----- vegetation density and growth parameters --- ---------------- Monthly Kfactors (PET/AET) order by water year month) ----------------------- --- Vegetation Type ------------  
     do i=1, nveg
      read(7,*) iveg,initveglai(iveg),uplailimit(iveg),dnlailimit(iveg),uplairate(iveg),dnlairate(iveg),vegsoil(iveg),(kfactor(k,iveg), k=1,12)
      uplairate(iveg)=1+uplairate(iveg)
      dnlairate(iveg)=1-dnlairate(iveg)
     enddo
     read(7,5) header2
     !----- LIST of average monthly precipitation for actual evapotranspiration adjustments (1981-2010)--------------------------------------------------------------------------------
     do i=1,12
      read(7,5)lai_pptfile(i)
     enddo
      read(7,5)aapptfile
      read(7,5) header2
     !----- LIST of optional average monthly solar radiation files for melting snow -------------------------------------------------------------------------------------------------------------
      do i=1,12
      read(7,5)averad_file(i)
      enddo
      read(7,5) header2
      
      do i=1,12
      read(7,5)flood_file(i)
      enddo
      
      read(7,5) header2
     !----- WATER YEARS TO PRINT OUT MAPS -------------------------------------------------------------------------------------------------------------------------------------------
      read(7,*) coutstore
     do i=1,coutstore
      read(7,*) a
      outstore(a)=1
     enddo
      if(startflag.eq.1) then
      Write(*,*)'This is a restart so the following two new output files will be created.'
      write(*,*) outfile,outfile2
      write(*,*)''
     else
      Write(*,*)'No prior data used for startup, generating initial conditions'
      Write(*,*)'for snow pack and soil moisture storage.'
      Write(*,*)''
     endif
     open(unit=21,file=outfile)
     open(unit=22,file=outfile2)
     write(*,*)'The model is running from water year',ybeg,' to ',yend
     write(*,*)''
     
     yearstorun=yend-ybeg+1
     Write(*,*)'Total years to run are', yearstorun
     storyear=0
     notsequenced=0
     do i=ybeg,yend
      if(outstore(i).eq.1) then
      storyear=storyear+1
      endif
     enddo
     do i=ybeg,yend-1
       if((outstore(i).eq.1).and.(outstore(i+1).ne.1))then
       notsequenced=1
       endif
     enddo
     write(*,*)'Number of water year maps to store are',storyear
     write(*,*)'Number of water year maps listed to store are',coutstore
     write(*,*)''
     if(coutstore.gt.storyear) then
      Write(*,*)'!!! Caution, there are more years to store than years in the model run!!!'
      Write(*,*)'!!! Check consistency for model year run and years to store!!!'
      Write(*,*)''
     else
    if(coutstore.ne.yearstorun) then
        if(notsequenced.eq.1) then
          Write(*,*)'!!! Caution, there are missing water year maps to store from the model run!!!'
          Write(*,*)''
          Write(*,*)'Not all years are stored and there is a break in the sequence'
        else
          Write(*,*)'!!! Caution, there are missing water year maps to store from the model run!!!'
          Write(*,*)''
          Write(*,*)'Not all years are stored but the years being stored are in sequence'
         endif
     else
      Write(*,*)'All years are sequential'
     endif
     endif

     if(coutstore.EQ.0) goto 95
!     open area table if reading basins
95    open(unit=18,file=areatable)
      open(unit=81,file='text.txt')
!     read in table for defining map areas
      read(18,5) header2
      read(18,5) header
      n=1
105   format(i8)
100   read(18,105,end=150) iarea(n)
      n=n+1
      goto 100
150   continue
      n=n-1
      write(*,*)''
      write(*,*)'Number of basins in the Table File = ',n
900  format(a6,a3,a4)
901  format(a3,2i2,a3,a4)
902  format(a5,i2,a3,a4)
903  format(a3,2i2,a3,a4)
904  format(a4,a3,2i2,a4)
911  format(a3,i2,i1,i1,a3,a4)
913  format(a4,i2,i1,i1,a3,a4)
914  format(a4,a3,i2,i1,i1,a4)
!    write header to output files
     write(21,2525)'Year','Month','Basin','ppt_mm','pet_mm','tmx_C','tmn_C','tav_C','snw_mm','mlt_mm','sbl_mm','pck_mm','exc_mm','aet_mm','cwd_mm','str_mm','rch_mm','run_mm','rch_acft','run_acft','Basin_area_m^2','smd_mm','smr_mm','evap_mm','watbal_mm','rchrunscaler'
2525 format(2x,a4,2x,a5,1x,a5,2(4x,a6),13(3x,a7),4x,a8,4x,a8,7x,a14,2a8,a8,2x,a9,2x,a12)
     write(22,2524)'Year','Basin','ppt_mm','pet_mm','tmx_C','tmn_C','tav_C','snw_mm','mlt_mm','sbl_mm','exc_mm','aet_mm','cwd_mm','rch_mm','run_mm','rch_acft','run_acft','Basin_area_m^2','evap_mm','watbal_mm','rchrunscaler'
2524 format(2x,a4,1x,a5,2(4x,a6),11(3x,a7),4x,a8,4x,a8,4x,a14,4x,a9,2x,a9,a14)
2526 format(a12,6x,a13)
      temp6=n       !number of basins
     do i=1,temp6
       j=i
       j=iarea(i)
       totcells(j)=0
     enddo
!    set the number of days in each month
     do a=1895,2101
      lpyr(a)=0
      ndy(a,1) = 31
      ndy(a,2) = 30
      ndy(a,3) = 31
      ndy(a,4) = 31
      ndy(a,5) = 28
      ndy(a,6) = 31
      ndy(a,7) = 30
      ndy(a,8) = 31
      ndy(a,9) = 30
      ndy(a,10) = 31
      ndy(a,11) = 31
      ndy(a,12) = 30
     enddo
     do i=1896,2101,4
      lpyr(i)=1
     enddo
      lpyr(1900)=0
      lpyr(2100)=0
     do i=1,12
      do a=1895,2101
       temp3=ndy(a,i)
        if((i.EQ.2).AND.(lpyr(a).EQ.1)) temp3=29
        ndy(a,i)=temp3
       enddo
     enddo
!    open input ascii grids for basin characteristics
     open(unit=20,file=areafile)
     open(unit=30,file=demfile)
     open(unit=31,file=soildfile)
     open(unit=32,file=wpfile)
     open(unit=33,file=fcfile)
     open(unit=34,file=porfile)
     open(unit=38,file=soilksfile)
     open(unit=35,file=aapptfile)
     open(unit=36,file=geolfile)
     if(rockksflag.eq.1) then
     open(unit=37,file=rockksfile)
     endif
     open(unit=118,file=vegfile)
     if(snowflag.eq.0) then
     open(unit=140,file=snowaccumfile)
     open(unit=141,file=mfmaxfile)
     open(unit=142,file=mfminfile)
     endif
     open(unit=143,file=aridityfile)
     if(maskflag.eq.1) then
     open(unit=144,file=maskfile)
     endif
     
 !   read in ascii grid header for area ID raster grid to get nodata3i
     read(20,*) dum1,cols
     read(20,*) dum1,rows
     read(20,*) dum1,xll
     read(20,*) dum1,yll
     read(20,*) dum1,cellsize
     read(20,*) dum1,nodata3i
!    read in standard ARCINFO ASCII matrix
!    use DEM to set grid dimensions
     read(30,*) dum1,cols
     read(30,*) dum1,rows
     read(30,*) dum1,xll
     read(30,*) dum1,yll
     read(30,*) dum1,cellsize
     read(30,*) dum1,nodata1
     write(*,21) rows*cols
21   format(/1x,'total number of grid cells = ',i10)
     read(36,*) dum1
     read(36,*) dum1
     read(36,*) dum1
     read(36,*) dum1
     read(36,*) dum1
     read(36,*) dum1,nodata2
     rewind(20)
     rewind(30)
     rewind(36)
     if(rockksflag.eq.1) then
     rewind(37)
     endif
!    start stepping thru months  This is the Beginning of the BCM starting at the first year specificed in the control file=======
     do 700 a=ybeg,yend,1
      if(a.lt.1900)then
       temp3=a-1800
       temp1=18
      elseif((a.ge.1900).and.(a.lt.2000)) then
       temp3=a-1900
       temp1=19
      elseif((a.ge.2000).and.(a.lt.2100)) then
       temp3=a-2000
       temp1=20
      elseif(a.ge.2100) then
       temp3=a-2100
       temp1=21
      endif
       temp2=0
      do ii=1,temp6
       j=ii
       j=iarea(ii)
       pptyr(j) = 0.
       petyr(j) = 0.
       tmaxyr(j) = 0.
       tminyr(j) = 0.
       tavgyr(j) = 0.
       snowyr(j) = 0.
       meltyr(j) = 0.
       sublyr(j) = 0.
       packyr(j) = 0.
       exc1yr(j) = 0.
       aet1yr(j) = 0.
       cwd1yr(j) = 0.
       storyr(j) = 0.
       rch3yr(j) = 0.
       run2yr(j) = 0.
       evapyr(j) = 0.
       smryr(j)  = 0.
       smdyr(j)  = 0.
       
      enddo     
!      this is water year, month 1 = oct, month 12 = sep
      do k=1,12    !do k=1,12 loop
       if(k.EQ.1) mon='oct'
       if(k.EQ.2) mon='nov'
       if(k.EQ.3) mon='dec'
       if(k.EQ.4) mon='jan'
       if(k.EQ.5) mon='feb'
       if(k.EQ.6) mon='mar'
       if(k.EQ.7) mon='apr'
       if(k.EQ.8) mon='may'
       if(k.EQ.9) mon='jun'
       if(k.EQ.10) mon='jul'
       if(k.EQ.11) mon='aug'
       if(k.EQ.12) mon='sep'
       if(k.le.3) then 
        temp3=temp3-1
       endif
       if(temp3.eq.-1) then
        temp1=temp1-1
        temp2=9
        temp3=9
       endif
       if(temp3.le.9) then
        write(81,911) 'ppt',temp1,temp2,temp3,mon,'.asc'
       endif
       if(temp3.gt.9) then
        write(81,901) 'ppt',temp1,temp3,mon,'.asc'
       endif
       if(temp3.le.9)then
        write(81,911) 'pet',temp1,temp2,temp3,mon,'.asc'
       endif
       if(temp3.ge.10)then
        write(81,901) 'pet',temp1,temp3,mon,'.asc'
       endif
       if(temp3.ge.10) then
        write(81,901) 'tmx',temp1,temp3,mon,'.asc'
        write(81,901) 'tmn',temp1,temp3,mon,'.asc'
        write(81,901) 'snw',temp1,temp3,mon,'.asc'
        write(81,901) 'mlt',temp1,temp3,mon,'.asc'
        write(81,901) 'sbl',temp1,temp3,mon,'.asc'
        write(81,901) 'pck',temp1,temp3,mon,'.asc'
        write(81,901) 'str',temp1,temp3,mon,'.asc'
        write(81,901) 'exc',temp1,temp3,mon,'.asc'
        write(81,901) 'aet',temp1,temp3,mon,'.asc'
        write(81,901) 'rch',temp1,temp3,mon,'.asc'
        write(81,901) 'cwd',temp1,temp3,mon,'.asc'
        write(81,901) 'run',temp1,temp3,mon,'.asc'
        write(81,901) 'ati',temp1,temp3,mon,'.asc'
        write(81,901) 'def',temp1,temp3,mon,'.asc'
        write(81,901) 'lai',temp1,temp3,mon,'.asc'
        write(81,901) 'wtb',temp1,temp3,mon,'.asc'
        write(81,901) 'evp',temp1,temp3,mon,'.asc'
        write(81,901) 'smd',temp1,temp3,mon,'.asc'
        write(81,901) 'smr',temp1,temp3,mon,'.asc'
        
               else
        write(81,911) 'tmx',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'tmn',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'snw',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'mlt',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'sbl',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'pck',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'str',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'exc',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'aet',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'rch',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'cwd',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'run',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'ati',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'def',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'lai',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'wtb',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'evp',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'smd',temp1,temp2,temp3,mon,'.asc'
        write(81,911) 'smr',temp1,temp2,temp3,mon,'.asc'
               endif
       if(k.le.3) then
        temp3=temp3+1
       endif
       if((temp3.eq.10).and.(temp2.eq.9)) then
        temp1=temp1+1
        temp2=0
        temp3=0
       endif
       do jj=1,21                         !Do jj=1,14 loop
        backspace(81)
       enddo                              !Enddo jj=1,14 loop 
       ascfile2=ADJUSTR(ClimateDirFile)
       ClimateDirFile=ascfile2
       do j=1,21                          !Do J=1,14 loop
        read(81,*) infile
        ascfile=ADJUSTL(infile)
       if(ClimateDirFlag.eq.1) then
        if(j.EQ.1) pptfile(k)=ADJUSTL(ClimateDirFile//ascfile)
        if(j.EQ.2) petfile(k)=ADJUSTL(ClimateDirFile//ascfile)
        if(j.EQ.3) tmaxfile(k)=ADJUSTL(ClimateDirFile//ascfile)
        if(j.EQ.4) tminfile(k)=ADJUSTL(ClimateDirFile//ascfile)
       else
        if(j.EQ.1) pptfile(k)=ascfile
        if(j.EQ.2) petfile(k)=ascfile
        if(j.EQ.3) tmaxfile(k)=ascfile
        if(j.EQ.4) tminfile(k)=ascfile 
       endif   
        if(j.EQ.5) snowfile(k)=ascfile
        if(j.EQ.6) meltfile(k)=ascfile
        if(j.EQ.7) sublfile(k)=ascfile
        if(j.EQ.8) packfile(k)=ascfile
        if(j.EQ.9) storfile(k)=ascfile
        if(j.EQ.10) exc1file(k)=ascfile
        if(j.EQ.11) aet1file(k)=ascfile
        if(j.EQ.12) rch3file(k)=ascfile
        if(j.EQ.13) cwd1file(k)=ascfile
        if(j.EQ.14) run2file(k)=ascfile
        if(j.EQ.15) atifile(k)=ascfile
        if(j.EQ.16) deffile(k)=ascfile
        if(j.EQ.17) laifile(k)=ascfile
        if(j.EQ.18) wtbfile(k)=ascfile
        if(j.EQ.19) evpfile(k)=ascfile
        if(j.EQ.20) smdfile(k)=ascfile
        if(j.EQ.21) smrfile(k)=ascfile
       enddo                              !Enddo J=1,16 loop 
      enddo                               !Enddo k=1,12 loop
     if(solarflag.eq.1) then
      open(unit=101,file=averad_file(1))
      open(unit=102,file=averad_file(2))
      open(unit=103,file=averad_file(3))
      open(unit=104,file=averad_file(4))
      open(unit=105,file=averad_file(5))
      open(unit=106,file=averad_file(6))
      open(unit=107,file=averad_file(7))
      open(unit=108,file=averad_file(8))
      open(unit=109,file=averad_file(9))
      open(unit=110,file=averad_file(10))
      open(unit=111,file=averad_file(11))
      open(unit=112,file=averad_file(12))
      
      do i=1,12
       read(100+i,*) dum1,cols
       read(100+i,*) dum1,rows
       read(100+i,*) dum1,xll
       read(100+i,*) dum1,yll

       read(100+i,*) dum1,cellsize
       read(100+i,*) dum1,nodata_value
      enddo
     endif               
       if(a.eq.ybeg) then
       Write(*,*) ''
       Write(*,*) 'Climate File Name Format:'
       Write(*,*) ''
       Write(*,*) petfile(1)
       write(*,*) pptfile(1)
       Write(*,*) tminfile(1)
       write(*,*) tmaxfile(1)
       Write(*,*) ''
       endif
       write(*,*) '       Year        Month    Consecutive Month'

     do 1000 i = 1, 12
!      compute counter year 1, month 1=1; year 2, month 1 = 13 ..
       if(a.EQ.ybeg) then
        ij=i
       else
        ij=a-ybeg+1
        ij=(ij-1)*12 + i
       endif
       if(i.lt.4) then
           imon=i+9
           iyear=a-1
       else
           imon=i-3
           iyear=a
       endif
       
       write(*,*) iyear,imon,ij
!      open input ascii grids for monthly parameters
       if(ingestflag.ne.0) then
        do sub=1,numingest
         open(unit=sub+124,file=ingestfile(sub))
         read(sub+124,*) dum1,cols
         read(sub+124,*) dum1,rows
         read(sub+124,*) dum1,xll
         read(sub+124,*) dum1,yll
         read(sub+124,*) dum1,cellsize
         read(sub+124,*) dum1,nodatasub
        enddo
      endif
       open(unit=8,file=pptfile(i))
       open(unit=121,file=lai_pptfile(i)) !average ppt for each month 1981-2010
       if(floodflag.eq.1) then
       open(unit=201,file=flood_file(i))
       endif
       open(unit=9,file=petfile(i))
       open(unit=10,file=tmaxfile(i))
       open(unit=11,file=tminfile(i))
!      open output ascii grids
       open(unit=54,file=packfile(i))
       open(unit=58,file=storfile(i))
       open(unit=62,file=atifile(i))
       open(unit=63,file=deffile(i))
       open(unit=64,file=laifile(i))
       open(unit=65,file=wtbfile(i))
       open(unit=66,file=evpfile(i))
       open(unit=67,file=smdfile(i))
       open(unit=68,file=smrfile(i))       
       
!      only open files if output is to be stored (this saves lots of storage space)
       if(outstore(a).EQ.1.AND.coutstore.NE.0)then
        open(unit=51,file=snowfile(i))
        open(unit=52,file=meltfile(i))
        open(unit=53,file=sublfile(i))
        open(unit=55,file=exc1file(i))
        open(unit=56,file=aet1file(i))
        open(unit=57,file=cwd1file(i))
        open(unit=59,file=rch3file(i))
        open(unit=60,file=run2file(i))
       endif
       if(geolksscaler.EQ.0)then
        open(unit=61,file='ctl_ksr0.asc')
        write(61,155) cols
        write(61,156) rows
        write(61,157) xll
        write(61,158) yll
        write(61,159) cellsize
        write(61,160) nodata1
       endif
    
!      get snowpack and soil moisture from prior month with new startup
       if((ij.eq.1).and.(startflag.eq.1)) then                         !if #1
!       for october, must look at snowpack and soil moisture prior year at new startup
         temp5=temp3-1
         if (temp5.eq.-1) then                  !if #2
          temp1=19
          temp2=9
          temp5=9
         endif                                  !endif #2
         if(a.eq.1900) then
          temp1=18
         endif
         if(a.eq.2100) then
          temp1=20
         endif
         mon='sep'    
    
         if (temp5.ge.10) then                  !if #3
          write(88,901) 'pck',temp1,temp5,mon,'.asc'
         else
          write(88,911) 'pck',temp1,temp2,temp5,mon,'.asc'
         endif                                  !endif #3
         backspace(88)
          read(88,*) infile
          ascfile=ADJUSTL(infile)
          packfile(i-1)=ascfile
          write(*,*) 'opened prior packfile at startup as ',ascfile
          open(unit=71,file=packfile(i-1))
        
         if(temp5.ge.10) then                   !if #4
          write(98,901) 'str',temp1,temp5,mon,'.asc'
         else
          write(98,911) 'str',temp1,temp2,temp5,mon,'.asc'
         endif                                 !endif #4
         backspace(98)
         read(98,*) infile
         ascfile=ADJUSTL(infile)
         storfile(i-1)=ascfile
         write(*,*) 'opened prior storefile at startup as ',ascfile
         open(unit=72,file=storfile(i-1))
        

         if(temp5.ge.10) then                   !if #5
          write(99,901) 'ati',temp1,temp5,mon,'.asc'
         else
          write(99,911) 'ati',temp1,temp2,temp5,mon,'.asc'
         endif                                 !endif #5
         backspace(99)
         read(99,*) infile
         ascfile=ADJUSTL(infile)
         atifile(i-1)=ascfile
         write(*,*) 'opened prior atifile at startup as ',ascfile
         open(unit=73,file=atifile(i-1))
        

         if(temp5.ge.10) then                   !if #6
          write(100,901) 'def',temp1,temp5,mon,'.asc'
         else
          write(100,911) 'def',temp1,temp2,temp5,mon,'.asc'
         endif                                 !endif #6
         backspace(100)
         read(100,*) infile
         ascfile=ADJUSTL(infile)
         deffile(i-1)=ascfile
         write(*,*) 'opened prior deffile at startup as ',ascfile
         open(unit=74,file=deffile(i-1))
         
         if(temp5.ge.10) then                   !if #7
          write(123,901) 'lai',temp1,temp5,mon,'.asc'
         else
          write(123,911) 'lai',temp1,temp2,temp5,mon,'.asc'
         endif                                 !endif #7
         backspace(123)
         read(123,*) infile
         ascfile=ADJUSTL(infile)
         laifile(i-1)=ascfile
         write(*,*) 'opened prior laifile at startup as ',ascfile
         open(unit=75,file=laifile(i-1))
        do j=1,6
         read(71,5) dum1
         read(72,5) dum1
         read(73,5) dum1
         read(74,5) dum1
         read(75,5) dum1
        enddo            
       
       endif                                   !endif #1  
!       end of get snow pack with prior data for start up
!       get snowpack and soil moisture from previous month
       if(ij.gt.1) then                         !if #1
        if(i.NE.1) then                         !if #2
         open(unit=71,file=packfile(i-1))
         open(unit=72,file=storfile(i-1))
         open(unit=73,file=atifile(i-1))
         open(unit=74,file=deffile(i-1))
         open(unit=75,file=laifile(i-1))
        else
  !       for october, must look at snowpack and soil moisture
  !       from previous year
         temp5=temp3-1
         if (temp5.eq.-1) then                  !if #3
          temp1=19
          temp2=9
          temp5=9
         endif                                  !endif #3
         if(a.eq.1900) then
          temp1=18
         endif
         if(a.eq.2100) then
          temp1=20
         endif
         mon='sep'    
         if (temp5.ge.10) then                  !if #4
          write(88,901) 'pck',temp1,temp5,mon,'.asc'
         else
          write(88,911) 'pck',temp1,temp2,temp5,mon,'.asc'
         endif                                  !endif #4
         backspace(88)
          read(88,*) infile
          ascfile=ADJUSTL(infile)
          packfile(i-1)=ascfile
          write(*,*) 'opened prior month packfile as ', ascfile
          open(unit=71,file=packfile(i-1))
         if(temp5.ge.10) then                   !if #5
          write(98,901) 'str',temp1,temp5,mon,'.asc'
         else
          write(98,911) 'str',temp1,temp2,temp5,mon,'.asc'
         endif                                 !endif #5
         backspace(98)
         read(98,*) infile
         ascfile=ADJUSTL(infile)
         storfile(i-1)=ascfile
         write(*,*) 'opened prior month storfile as ',ascfile
         open(unit=72,file=storfile(i-1))
         if(temp5.ge.10) then                   !if #5
          write(99,901) 'ati',temp1,temp5,mon,'.asc'
         else
          write(99,911) 'ati',temp1,temp2,temp5,mon,'.asc'
         endif                                 !endif #5
         backspace(99)
         read(99,*) infile
         ascfile=ADJUSTL(infile)
         atifile(i-1)=ascfile
         write(*,*) 'opened prior month atifile as ',ascfile
         open(unit=73,file=atifile(i-1))
         if(temp5.ge.10) then                   !if #5
          write(100,901) 'def',temp1,temp5,mon,'.asc'
         else
          write(100,911) 'def',temp1,temp2,temp5,mon,'.asc'
         endif                                 !endif #5
         backspace(100)
         read(100,*) infile
         ascfile=ADJUSTL(infile)
         deffile(i-1)=ascfile
         write(*,*) 'opened prior month deffile as ',ascfile
         open(unit=74,file=deffile(i-1))

         if(temp5.ge.10) then                   !if #6
          write(123,901) 'lai',temp1,temp5,mon,'.asc'
         else
          write(123,911) 'lai',temp1,temp2,temp5,mon,'.asc'
         endif                                 !endif #6
         backspace(123)
         read(123,*) infile
         ascfile=ADJUSTL(infile)
         laifile(i-1)=ascfile
         write(*,*) 'opened prior month laifile as ',ascfile
         open(unit=75,file=laifile(i-1))
        endif                                  !endif #2
       endif                                   !endif #1  

!      write-out standard ARCINFO ASCII matrix
       write(54,155) cols
       write(54,156) rows
       write(54,157) xll
       write(54,158) yll
       write(54,159) cellsize
       write(54,160) nodata1
       write(58,155) cols
       write(58,156) rows
       write(58,157) xll
       write(58,158) yll
       write(58,159) cellsize
       write(58,160) nodata1
       write(62,155) cols
       write(62,156) rows
       write(62,157) xll
       write(62,158) yll
       write(62,159) cellsize
       write(62,160) nodata1
       write(63,155) cols
       write(63,156) rows
       write(63,157) xll
       write(63,158) yll
       write(63,159) cellsize
       write(63,160) nodata1
       write(64,155) cols
       write(64,156) rows
       write(64,157) xll
       write(64,158) yll
       write(64,159) cellsize
       write(64,160) nodata1
       write(65,155) cols
       write(65,156) rows
       write(65,157) xll
       write(65,158) yll
       write(65,159) cellsize
       write(65,160) nodata1
       write(66,155) cols
       write(66,156) rows
       write(66,157) xll
       write(66,158) yll
       write(66,159) cellsize
       write(66,160) nodata1
       write(67,155) cols
       write(67,156) rows
       write(67,157) xll
       write(67,158) yll
       write(67,159) cellsize
       write(67,160) nodata1
       write(68,155) cols
       write(68,156) rows
       write(68,157) xll
       write(68,158) yll
       write(68,159) cellsize
       write(68,160) nodata1

!      only open files if output is to be stored (this saves lots of storage space)
       if(outstore(a).EQ.1.AND.coutstore.NE.0)then
!       write to unit 51,52,53
        do k=2,4
         write(49+k,155) cols
         write(49+k,156) rows
         write(49+k,157) xll
         write(49+k,158) yll
         write(49+k,159) cellsize
         write(49+k,160) nodata1
        enddo
!       write to unit 55,56,57
        do k=6,8
         write(49+k,155) cols
         write(49+k,156) rows
         write(49+k,157) xll
         write(49+k,158) yll
         write(49+k,159) cellsize
         write(49+k,160) nodata1
        enddo
!       write to unit 59,60
        do k=10,11
         write(49+k,155) cols
         write(49+k,156) rows
         write(49+k,157) xll
         write(49+k,158) yll
         write(49+k,159) cellsize
         write(49+k,160) nodata1
        enddo
              endif
155    format('ncols       ',2x,i6)
156    format('nrows       ',2x,i6)
157    format('xllcorner   ',1x,f18.6)
158    format('yllcorner   ',1x,f18.6)
159    format('cellsize    ',1x,f14.6)
160    format('NODATA_value',2x,f12.1)
!      initialize 
       do ii=1,temp6
        jj=ii
        jj=iarea(ii)
        avgppt(jj)  = 0.
        avgpet(jj)  = 0.
        avgsnow(jj) = 0.
        avgmelt(jj) = 0.     
        avgsubl(jj) = 0.
        avgpack(jj) = 0.
        avgexc1(jj) = 0.
        avgaet1(jj) = 0.
        avgrch3(jj) = 0.
        avgcwd1(jj) = 0.
        avgrun2(jj) = 0.
        avgevap(jj) = 0.
        avgstor(jj) = 0.
        avgtmax(jj) = 0.
        avgtmin(jj) = 0.
        avgtavg(jj) = 0.
        avgsmd(jj)  = 0.
        avgsmr(jj)  = 0.
       enddo
!      read in first 5 lines ascii grid headers for each input grid
       do j = 1,5
        read(8,5) dum1
        read(9,5) dum1
        read(10,5) dum1
        read(11,5) dum1
        read(20,5) dum1
        read(30,5) dum1
        read(31,5) dum1
        read(32,5) dum1
        read(33,5) dum1
        read(34,5) dum1
        read(38,5) dum1
        read(35,5)dum1
        read(36,5) dum1
        if(rockksflag.eq.1) then
        read(37,5) dum1
        endif
        read(118,5) dum1
        read(121,5) dum1
        if(floodflag.eq.1) then
        read(201,5) dum1
        endif
        if(snowflag.eq.0.) then
         read(140,5) dum1
         read(141,5) dum1
         read(142,5) dum1
        endif
        read(143,5) dum1
        if(maskflag.eq.1) then
         read(144,5) dum1
        endif
        if(ij.gt.1) then
         read(71,5) dum1
         read(72,5) dum1
         read(73,5) dum1
         read(74,5) dum1
         read(75,5) dum1
        endif                              
       enddo
       read(8,*) dum1,inpnodat(1)
       read(9,*) dum1,inpnodat(2)
       read(10,*) dum1,inpnodat(3)
       read(11,*) dum1,inpnodat(4)
       read(20,5) dum1
       read(30,5) dum1
       read(31,*) dum1,inpnodat(5)
       read(32,*) dum1,inpnodat(6)
       read(33,*) dum1,inpnodat(7)
       read(34,*) dum1,inpnodat(8)
       read(38,*) dum1,inpnodat(8)
       read(35,*) dum1,inpnodat(9)
       read(36,5) dum1
       if(rockksflag.eq.1) then
       read(37,5) dum1
       endif
       read(118,5) dum1
       read(121,5) dum1
       if(floodflag.eq.1) then
       read(201,5) dum1
       endif
        if(snowflag.eq.0.) then
       read(140,5) dum1
       read(141,5) dum1
       read(142,5) dum1
       endif
       read(143,5) dum1
       if(maskflag.eq.1) then
       read(144,5) dum1
       endif
       if(ij.gt.1) then
        read(71,5) dum1
        read(72,5) dum1
         read(73,5) dum1
         read(74,5) dum1
         read(75,5) dum1
       endif                         
!      read in input ascii grids, one row at a time
       do 1100 j = 1,rows  !First Row of the BCM
!       read in the climate data
        read(8,*) (pptmon(k), k = 1,cols)
        read(121,*)(pptavemon(k),k=1,cols)
        if(floodflag.eq.1) then
        read(201,*)(flood(k),k=1,cols)
        endif
        read(9,*) (petmon(k), k = 1,cols)
        do k=1,cols
            if(petmon(k).ne.inpnodat(2)) then
             if(petmon(k).lt.0) then
              petmon(k)=0.
             endif
        endif
        enddo
        read(10,*) (tmaxmon(k), k = 1,cols)
        read(11,*) (tminmon(k), k = 1,cols)
        if(solarflag.eq.1) then
        if(i.eq.1) read(101,*)(octrad(k),k=1,cols)
        if(i.eq.2) read(102,*)(novrad(k),k=1,cols)
        if(i.eq.3) read(103,*)(decrad(k),k=1,cols)
        if(i.eq.4) read(104,*)(janrad(k),k=1,cols)
        if(i.eq.5) read(105,*)(febrad(k),k=1,cols)
        if(i.eq.6) read(106,*)(marrad(k),k=1,cols)
        if(i.eq.7) read(107,*)(aprrad(k),k=1,cols)
        if(i.eq.8) read(108,*)(mayrad(k),k=1,cols)
        if(i.eq.9) read(109,*)(junrad(k),k=1,cols)
        if(i.eq.10) read(110,*)(julrad(k),k=1,cols)
        if(i.eq.11) read(111,*)(augrad(k),k=1,cols)
        if(i.eq.12) read(112,*)(seprad(k),k=1,cols)
        endif
        
!       read in basin characteristics
        read(20,*) (area(k), k = 1,cols)
        read(30,*) (elev(k), k = 1,cols)
        read(31,*) (soild(k), k = 1,cols)
        read(32,*) (wp(k), k = 1,cols)
        read(33,*) (fc(k), k = 1,cols)
        read(34,*) (por(k), k = 1,cols)
        read(38,*) (soilks(k), k = 1,cols)
        read(35,*) (aveppt(k),k=1,cols)
        do k=1,cols
        if((wp(k).eq.nodata1).or.(fc(k).eq.nodata1).or.(por(k).eq.nodata1).or.(soild(k).eq.nodata1).or.(elev(k).eq.nodata1)) then
         wp(k)=nodata1
         fc(k)=nodata1
         por(k)=nodata1
         soild(k)=nodata1
         elev(k)=nodata1
        else
            !por(k)=por(k)/100
            if(wp(k).lt.0) then
                wp(k)=0
                endif
            endif
        enddo
  !       for sensitivity analysis of soil depth
        if(urbanflag.GE.1)then  !1
         do k=1,cols
          if(soild(k).ne.nodata1)then !2
           if(soild(k).gt.urbansoild) then !4
            if(urbansoil.ne.999) then
             soild(k)=urbansoild
            endif
           endif !4
           if (soild(k).lt.0) then !6
            soild(k)=0
           endif !6
          endif !2
         enddo 
        endif !1
    if(soildpercent.EQ.1)then
         do k=1,cols
          if(soild(k).ne.nodata1)then
           soild(k)=soild(k)*persoild + soild(k)
           if (soild(k).lt.0) then
            soild(k)=0
           endif
          endif
         enddo
        endif
!    for sensitivity analysis of porosity, field capacity, and wilting point
     do k=1,cols
      if(soild(k).ne.nodata1) then
       if(fc(k).gt.por(k)) then
         write(*,*)j,k,por(k),fc(k),'row,column,por, fc'
         pause
        endif
        if(wp(k).gt.fc(k)) then
         write(*,*)'wp.gt.fc, hit enter to swap and move on'
         pause
        endif
      endif
     enddo     
      read(36,*) (geolid(k), k = 1,cols)
      if(rockksflag.eq.1) then
      read(37,*) (geolks(k), k = 1,cols)
      endif
      read(118,*) (veg(k), k = 1,cols)
     !set up snow accumlation and melt parameters
     if(snowflag.eq.0.) then
      read(140,*) (snowaccum(k), k = 1,cols)
      read(141,*) (mfmax(k), k = 1,cols)
      read(142,*) (mfmin(k), k = 1,cols)
     else
      do k=1,cols
       snowaccum(k)=snowaccumT
       mfmax(k)=maxmf
       mfmin(k)=minmf
      enddo
     endif
       
      read(143,*) (aridity(k), k = 1,cols)
      if(maskflag.eq.1) then
      read(144,*) (mask(k), k=1,cols)
      endif
       do k=1,cols
  !   If rockksflag is 1 then use the ctl file table of conductivities                
       if(rockksflag.EQ.0.)then        
        if(geolid(k).eq.nodata1) then
         geolks(k)=nodata1
        else
         geolks(k)=rockks(geolid(k))         
        endif
       endif
        if((veg(k).eq.urbanveg).and.(soilscaler.eq.1)) then
         geolks(k) = urbanks
        endif
        if(soild(k).ne.nodata1)then 
         if(veg(k).ne.nodata1) then
          soild(k)=vegsoil(veg(k)) + soild(k)  !add extra soil depth to compensate for extra depth of root extraction
         endif
        endif
       enddo
       if(rockksflag.eq.0) then
       write(61,1205) (geolks(k),k=1,cols)
      endif
       do k=1, cols
  !       fix soils that have depth but no por, wc, or fc
         if((soild(k).gt.0.).and.((wp(k).eq.0.).or.(fc(k).eq.0).or.(por(k).eq.0))) then
          soild(k)=0
         endif     
         if(soild(k).eq.0.) then !fixed to make up some no soil zones 11/1/2006
          wp(k)=0
          fc(k)=0
          por(k)=0
         endif
       enddo
!    format 5/11/2018 started here       
!    set packmon to 0 mm, atimon to 0 mm, defmon to 0 mm, and evapmon to 0 mm for 1st month of a new run
!    set soil moisture to (istor*(fc(k)-wp(k))+wp(k))*soild(k)*1000. for 1st month of sequence for a new run
       if((ij.eq.1).and.(startflag.eq.0)) then
       do k = 1,cols
        if(soild(k).ne.nodata1) then
         packmon(k) = 0.
         atimon(k)  = 0.
         defmon(k)  = 0.
         evapmon(k) = 0.
         stormon(k) = (istor*(fc(k)-wp(k))+wp(k))*soild(k)*1000.
         maxiwc = por(k)*soild(k)*1000.       
         if(stormon(k).gt.maxiwc) then
             stormon(k) = maxiwc
             endif
          if(stormon(k).lt.0) then
           write(*,*)'stormon.lt.0.'
           pause
          endif
         if(INT(wp(k)).eq.INT(inpnodat(6)).and.soild(k).ne.0.OR.INT(por(k)).eq.INT(inpnodat(8)).and.soild(k).ne.0)then
          stormon(k)=nodata1
          endif
         if(veg(k).eq.nodata1) then
           laimon(k)=nodata1
          else
           laimon(k)=initveglai(veg(k)) !this determines the starting point for each cell so it can change later
         endif
        endif
       enddo
      else
!if the startflag is set to 1 then the data will be read from the files for the previous month, i.e. the packmon file will be from September of the previous water year, rather than created above.
       read(71,*) (packmon(k), k = 1,cols)
       read(72,*) (stormon(k), k = 1,cols)
       read(73,*) (atimon(k), k = 1,cols)
       read(74,*) (defmon(k), k = 1,cols)
       read(75,*) (laimon(k), k = 1,cols)
      endif
!    format 5/11/2018 ended here
      
!------- The BCM start calculations for output here --------------------------------------------------------------------------------------------------------------
!       Loop through grid columns for each row
!       Initialize the output grids
        do 1200 k = 1,cols  !First Column of the BCM Run
 !       if((k.eq.3).and.(j.eq.1166)) then
  !       pause
   !     endif
 
          barea=0
         do ii=1,temp6
          if(area(k).NE.iarea(ii))then
           barea=barea +0
          else
            barea=barea +1
          endif
         enddo
         if(barea.EQ.0)then
          pptmon(k) = nodata1
          pptavemon(k) = nodata1
          snowmon(k)  = nodata1
          packmon(k)  = nodata1
          meltmon(k)  = nodata1
          sublmon(k)  = nodata1
          exc1mon(k)  = nodata1
          aet1mon(k)  = nodata1
          rch3mon(k)  = nodata1
          cwd1mon(k)  = nodata1
          run2mon(k)  = nodata1
          stormon(k)  = nodata1
          atimon(k)   = nodata1
          defmon(k)   = nodata1
          laimon(k)   = nodata1
          evapmon(k)  = nodata1
          smdmon(k)   = nodata1
          smrmon(k)   = nodata1
          goto 1200
          endif
         if(area(k).EQ.nodata3i)then
          pptmon(k) = nodata1
          pptavemon(k) = nodata1
          snowmon(k)  = nodata1
          packmon(k)  = nodata1
          meltmon(k)  = nodata1
          sublmon(k)  = nodata1
          exc1mon(k)  = nodata1
          aet1mon(k)  = nodata1
          rch3mon(k)  = nodata1
          cwd1mon(k)  = nodata1
          run2mon(k)  = nodata1
          stormon(k)  = nodata1
          atimon(k)   = nodata1
          defmon(k)   = nodata1
          laimon(k)   = nodata1
          evapmon(k)  = nodata1
          smdmon(k)   = nodata1
          smrmon(k)   = nodata1
          goto 1200
         endif
         if((mask(k).eq.nodata1).and.(maskflag.eq.1.)) then
         pptavemon(k) = nodata1
          snowmon(k)  = nodata1
          packmon(k)  = nodata1
          meltmon(k)  = nodata1
          sublmon(k)  = nodata1
          exc1mon(k)  = nodata1
          aet1mon(k)  = nodata1
          rch3mon(k)  = nodata1
          cwd1mon(k)  = nodata1
          run2mon(k)  = nodata1
          stormon(k)  = nodata1
          atimon(k)   = nodata1
          defmon(k)   = nodata1
          laimon(k)   = nodata1
          evapmon(k)  = nodata1
          smdmon(k)   = nodata1
          smrmon(k)   = nodata1
          goto 1200
          endif
         
         snowmon(k) = 0.
         meltmon(k) = 0.
         sublmon(k) = 0.
         exc1mon(k) = 0.
         aet1mon(k) = 0.
         rch3mon(k) = 0.
         cwd1mon(k) = 0.
         run2mon(k) = 0.
         evapmon(k) = 0.
         smdmon(k)  = 0.
         smrmon(k)  = 0.
         
          if((INT(elev(k)).eq.INT(nodata1)).OR.(INT(geolks(k)).eq.INT(nodata1)).OR.(INT(pptmon(k)).eq.INT(inpnodat(1))).OR.(INT(veg(k)).eq.INT(inpnodat(1))).OR.(INT(pptavemon(k)).eq.INT(inpnodat(1))).OR.(INT(petmon(k)).eq.INT(inpnodat(2))).OR.(INT(tmaxmon(k)).eq.INT(inpnodat(3))).OR.(INT(tminmon(k)).eq.INT(inpnodat(4))).OR.(INT(soild(k)).eq.INT(inpnodat(5))).OR.(INT(wp(k)).eq.INT(inpnodat(6)).and.soild(k).ne.0).OR.(INT(fc(k)).eq.INT(inpnodat(7)).and.soild(k).ne.0).OR.(INT(por(k)).eq.INT(inpnodat(8)).and.soild(k).ne.0))then
           pptmon(k) = nodata1
           pptavemon(k) = nodata1
           snowmon(k)  = nodata1
           packmon(k)  = nodata1
           meltmon(k)  = nodata1
           sublmon(k)  = nodata1
           exc1mon(k)  = nodata1
           aet1mon(k)  = nodata1
           rch3mon(k)  = nodata1
           cwd1mon(k)  = nodata1
           run2mon(k)  = nodata1
           stormon(k)  = nodata1
           atimon(k)   = nodata1
           defmon(k)   = nodata1
           laimon(k)   = nodata1
           evapmon(k)  = nodata1
           smdmon(k)   = nodata1
           smrmon(k)   = nodata1
          else
             if(stormon(k).lt.0) then
              write(*,*)2,'stormon(k).lt.0)'
              pause
             endif
              
              !           count the number of grid cells in a basin (first year,first month)
            if(a.EQ.ybeg.AND.i.EQ.1)then
 !            jk=rect
             jk=area(k)
             totcells(jk)=totcells(jk) + 1
            endif
!           Start grid calculations
!           Define variables for maximum and minimun temperature

            TmaxC=tmaxmon(k)!+3 simulated future climate
            TminC=tminmon(k)!+3 simulated future climate
!           Use air temperature to set snow model parameters
            Trange = TmaxC - TminC
            TavgC = (TmaxC + TminC)/2.
!            Partition snow/rain using min/max air temps
!            Calibrated Utah snow using MODIS snow cover (allows snow to accumulate when air is colder than 1.5 C)
!            Snow2 version of the BCM (increase snow to 4.5 and negative melt factor is 0.35
             if(((TminC-snowaccum(k)).le.0.).and.((TmaxC-snowaccum(k)).le.0.)) then
              snowfrac = 1.0
              rainfrac = 0.0
             else if(((TminC-snowaccum(k)).le.0.).and.((TmaxC-snowaccum(k)).gt.0.)) then
              snowfrac = -(TminC-snowaccum(k))/Trange
              rainfrac = (TmaxC-snowaccum(k))/Trange
             else
              snowfrac = 0.0
              rainfrac = 1.0                               
             endif
   !        NWS Snowmodel-17 melt model
   !        vegetation type (veg(k) and month (i) determine the pet for the specific month
             !This is inserted to allow this version of the BCM to replicate v6.5
            if(rainfracflag.eq.1) then
                pethold=petmon(k)
                petmon(k) =  petmon(k)*rainfrac  !in version v6.5 but taken out for later versions
            endif
            snowmon(k) = pptmon(k)*snowfrac     !snow taken out of precipitation later in program
            laiscaler(k)=pptmon(k)/pptavemon(k)
            rchrunscaler(k)=pptmon(k)/pptavemon(k)
             if (rchrunscaler(k).gt.(1+rchrunlimit)) then
              rchrunscaler(k)=1+rchrunlimit
             endif
             if (rchrunscaler(k).lt.(1-rchrunlimit)) then
              rchrunscaler(k)=1-rchrunlimit
             endif
             if (rchrunscaler(k).lt.(1-rchrunlimit))then
                 write(*,*) 'rchrunscaler'
             pause
             endif
            priorpackmon(k)=packmon(k)
            packmon(k) = packmon(k) + snowmon(k)
            sublpar1=0.1309*petmon(k)+3.9669      !y = 0.1309x + 3.9669  R2 = 0.5882 From Reba Site Sublimation (Daily Sublimation 2009_05_06.xls) Idaho
            thetarel=(stormon(k)-wp(k)*soild(k)*1000)/(por(k)*soild(k)*1000.-wp(k)*soild(k)*1000) !cap theta rel 10 for the Beekman soil series in the alphaprime equation
        !   thetarel=stormon(k)/(fc(k)*soild(k)*1000.)                                            !orignal theta rel (theta/thetasat) at 2.83 for the Beekman soil series instead of the 10 in the alphaprime equation
            if(alphaprimeflag.eq.1) then    
            if(thetarel.lt.0.0) thetarel=0.0
                alphaprime=(1-exp(-10*thetarel))
                !alphaprime=(1-exp(-2.83*thetarel))
           if((por(k).eq.0.).or.(geolid(k).eq.58).or.(stormon(k).eq.0.)) then
              alphaprime=1
             endif
            else
                alphaprime=1.0
            endif

     !start limit up and down lai
     if(laimon(k).eq.0) then
       laimon(k)=.01
     endif
     lastlai(k)=laimon(k)
     if((initveglai(veg(k)).lt.dnlailimit(veg(k))).and.(recover(j,k).ne.1))  then !there was a disturbance
        if(laimon(k).le.1.) then  
         laimon(k)=lastlai(k)+(1.0-(aveppt(k)-pptmon(k))/aveppt(k))*(uplairate(veg(k))-1)
         if(laimon(k).ge.1.) then
           laimon(k)=1.
           recover(j,k)=1
          endif
        endif
     else
         laimon(k)=lastlai(k)*laiscaler(k)    
          if(laiscaler(k).gt.uplairate(veg(k))) then
           laimon(k)=lastlai(k)*uplairate(veg(k))
          endif
          if(laiscaler(k).lt.dnlairate(veg(k))) then
           laimon(k)=lastlai(k)*dnlairate(veg(k))
          endif
          if(laimon(k).gt.uplailimit(veg(k))) then
           laimon(k)=uplailimit(veg(k))
          endif
          if(laimon(k).lt.dnlailimit(veg(k))) then
           laimon(k)=dnlailimit(veg(k))
          endif
      endif  !end of disturbance lai
! end limit up and down lai

2 Format(3f12.5)
 
            vegpetmon(k)=alphaprime*petmon(k)*kfactor(i,veg(k))*laimon(k) !moved down here to get sublimation correct
           if(vegpetmon(k).gt.petmon(k)) then
            vegpetmon(k)=petmon(k)
           endif
          if(packmon(k).GE.sublpar1)then
             sublmon(k) = sublpar1
            else
             sublmon(k)= packmon(k)
            endif
            packmon(k) = packmon(k) - sublmon(k)
!           NWS Snowmodel-17 snow melt calculations
            if(solarflag.eq.1) then
              if(i.EQ.1) solarrad=octrad(k)/31
              if(i.EQ.2) solarrad=novrad(k)/30
              if(i.EQ.3) solarrad=decrad(k)/31
              if(i.EQ.4) solarrad=janrad(k)/31
              if(i.EQ.5) solarrad=febrad(k)/28
              if(i.EQ.6) solarrad=marrad(k)/31
              if(i.EQ.7) solarrad=aprrad(k)/30
              if(i.EQ.8) solarrad=mayrad(k)/31
              if(i.EQ.9) solarrad=junrad(k)/30
              if(i.EQ.10) solarrad=julrad(k)/31
              if(i.EQ.11) solarrad=augrad(k)/31
              if(i.EQ.12) solarrad=seprad(k)/30
             if(solarrad.lt.7.5) solartmp=0
             if((solarrad.ge.7.5).and.(solarrad.lt.10.)) solartmp=0.25
             if((solarrad.ge.10.5).and.(solarrad.lt.12.5)) solartmp=0.50
             if((solarrad.ge.12.5).and.(solarrad.lt.15.0)) solartmp=0.75
             if((solarrad.ge.15.0).and.(solarrad.lt.17.5)) solartmp=1.00
             if((solarrad.ge.17.5).and.(solarrad.lt.20.0)) solartmp=1.25
             if((solarrad.ge.20.0).and.(solarrad.lt.22.5)) solartmp=1.50
             if((solarrad.ge.22.5).and.(solarrad.lt.25.0)) solartmp=1.75
             if((solarrad.ge.25.0).and.(solarrad.lt.27.5)) solartmp=2.00
             if((solarrad.ge.27.5).and.(solarrad.lt.30.0)) solartmp=2.25
             if(solarrad.ge.30.0) solartmp=2.50
            else
             solartmp=0
            endif
          
!           mfmax set by control file
!           mfmin set by control file
!           month adjustment factor maf=5.677 set by control file
!           tipm=0.10   !Anticedent temperature index
!           nmf=0.21    !maximum negative melt factor
            if(packmon(k).gt.0) then !calculate ati and deld only if snow pack exists otherwise it accumulates in the air and is applied to the pack once it occurs error fixed 11/23/2014
             mf=0.5*(mfmax(k)+mfmin(k))+0.5*(mfmax(k)-mfmin(k))*sin((i-maf)*2*pi/12) !5.677
             mp=mf*((solartmp+tavgc)-0)  ! base temperature where snow can melt this was 2.5 but mp wasn't used, mf was
             nmfac=(mf/mfmax(k))*nmf
             !ati(0,k)=0
             ati(i)=atimon(k)+tipm*((tavgc+solartmp)-atimon(k)) ! Antecedent temperature index
             if(ati(i).gt.0.) then 
              ati(i)=0
             endif
            atimon(k)=ati(i) !getready for next month
            deld(i)=nmfac*(ati(i)-(tavgc+solartmp)) !Snow cover heat deficit change
            d(i)=defmon(k)-delD(i)
             if(d(i).le.0.) then
              meltmon(k)=0.
              else
              meltmon(k) = mp*ndy(a,i) 
              d(i)=0
             endif
             defmon(k)=d(i)  !get ready for next month
             if(meltmon(k).LT.0.) meltmon(k)=0.
              packmon(k) = packmon(k) - meltmon(k)
             if(packmon(k).lt.0.) then
              meltmon(k) = meltmon(k) + packmon(k)
              packmon(k) = 0.
             endif
            endif
!           to stop glaciers from getting too big
            if(packmon(k).gt.5000) then
             sublmon(k)=sublmon(k)+packmon(k)-5000
             packmon(k)=5000
            endif
!           calculate recharge and runoff potential
!           using soil storage terms and antecedent soil moisture
 !      if((k.eq.883).and.(j.eq.1568)) then
 !      write(*,*)'here',stormon(k)
 !       pause
 !       endif
       
            
            wpmm(k) = wp(k)*soild(k)*1000.
            fcmm(k) = fc(k)*soild(k)*1000.
            pormm(k) = por(k)*soild(k)*1000.
            rchstor = fcmm(k) - wpmm(k)
            runstor = pormm(k) - wpmm(k)
            priorstormon(k)=stormon(k)
            stormon(k) = stormon(k) + pptmon(k)+ meltmon(k) - snowmon(k) !- petmon(k) possible error and taken out on 3/27/2014
            if(stormon(k).lt.0) then
                write(*,*)'stormon.lt.0'
                pause
            endif
            
            if(stormon(k).gt.pormm(k)) then
             runoff = stormon(k) - pormm(k)
             stormon(k) = pormm(k)
            else
             runoff = 0.
            endif
          
 !if the pixel is flooded fill the soil to porosity, floodwater 
        if(floodflag.eq.1) then
            if (flood(k).eq.1.) then 
             floodwater(j,k)=(pormm(k)-stormon(k))
             stormon(k)=pormm(k)
            else
             floodwater(j,k)=0
            endif
         endif
            
            if(drydownflag.eq.0) then
                drydown(k)=0
                else
              drydown(k)=arida*exp(aridb*aridity(k))+aridc
              endif
   		   if(stormon(k).ge.fcmm(k)) then    !beginning of big if
	        if((stormon(k)-wpmm(k)).ge.vegpetmon(k)) then !if we have enough water for pet then aet equal pet
		      aet1mon(k)= vegpetmon(k)
			  cwd1mon(k)= petmon(k)-aet1mon(k)
		      stormon(k)=stormon(k)-aet1mon(k) !stor is getter than or equal to wp
              if(stormon(k).gt.fcmm(k)) then !if stor is greater than fc then recharge
                recharge=stormon(k)-fcmm(k)
			    stormon(k)=fcmm(k)
              else
                recharge=0
              endif
            else !we don't have enought for aet so we run stor down to wp
			  recharge=0
              aet1mon(k)=stormon(k)-wpmm(k)
			  cwd1mon(k)=petmon(k)-aet1mon(k)
              stormon(k)=wpmm(k) - drydown(k)*cwd1mon(k)   !We get down to wp but there is still a demand and we take out of the soil.  This line was missing in v43 add in v44 also added  ![-cwd1mon(k) lets the soil dry beyond field capapcity]
              evapmon(k)=drydown(k)*cwd1mon(k)
              if(stormon(k).lt.0.) then
               evapmon(k)=evapmon(k)+stormon(k)
               stormon(k)=0
              endif 

            endif
	      else !recharge will be zero but we can aet1mon and cwd1mon and change stormon
			if(stormon(k).ge.wpmm(k)) then
			 if((stormon(k)-wpmm(k)).ge.vegpetmon(k)) then !if we have enough water for pet the aet equal pet
			  aet1mon(k)= vegpetmon(k)
			  cwd1mon(k)=petmon(k)-aet1mon(k)
			  stormon(k)=stormon(k)-aet1mon(k)  
             else
			  aet1mon(k)=stormon(k)-wpmm(k)
			  cwd1mon(k)=petmon(k)-aet1mon(k)
			  stormon(k)=wpmm(k) - drydown(k)*cwd1mon(k)  ![-cwd1mon(k) lets the soil dry beyond wilting point] add in v44
              evapmon(k)=drydown(k)*cwd1mon(k)
              if(stormon(k).lt.0.) then
               evapmon(k)=evapmon(k)+stormon(k)
               stormon(k)=0
              endif 
             endif
            else
             aet1mon(k)=0 !aet1mon(k)-(wpmm(k)-stormon(k)) only come here with stormon is less than wilting point so AET is zero but evaporation can still occur
             cwd1mon(k)=petmon(k)-aet1mon(k)
             stormon(k)=stormon(k) - drydown(k)*cwd1mon(k)  ![-cwd1mon(k) lets the soil dry beyond wilting point] add in v44
             evapmon(k)=drydown(k)*cwd1mon(k)
              if(stormon(k).lt.0.) then
               evapmon(k)=evapmon(k)+stormon(k)
               stormon(k)=0
              endif 
            endif
           recharge=0   !if stormon is less than fc the recharge must be zero
          endif  !end of big if
          
 !          if((soild(k).gt.0.4).and.(stormon(k).lt.wp(k)*(soild(k)-0.4)*1000)) then  taking out when testing -0.1*cwd
 !          stormon(k)=wp(k)*(soild(k)-0.4)*1000
 !          endif
          if(stormon(k).lt.0) then
           stormon(k)=0
          endif
          if(stormon(k).gt.fcmm(k)) then
           write(*,*)3
           pause
          endif
    if(soild(k).eq.0) then 
	 runoff=pptmon(k)+meltmon(k)-snowmon(k)
	 aet1mon(k)=0
	 cwd1mon(k)=0
	 recharge=0
    endif 
            smdmon(k)=fcmm(k)-stormon(k)  !this is soil moisture deficit need to generate recharge or runoff
            smrmon(k)=pormm(k)-stormon(k) !this is soil moisture deficit need to generate overland flow
            if(smdmon(k).lt.0) then
                write(*,*) 'smdmon'
                pause
            endif
!if((j.eq.777).and.(k.eq.710)) then
 !write(*,*)'check pethold'
! pause
!endif
            
            
            if((TavgC.le.snowaccumT).and.(rainfracflag.eq.1)) then  !put petmon(k) back to what it was before
            petmon(k)=pethold
            endif         
  
            if(aet1mon(k).lt.-0.1) then
            write(*,*) aet1mon(k),'aet is negative'
            pause
            endif
!           calculate recharge and runoff indicators
!           Basic calculations (no antecedent conditions)
!           Excess water or = precip - PET
!           recharge # 2 = precip - PET - (fieldcap storage)
!           if recharge > bedrock ksat, recharge = bedrock ksat
!           runoff #1 = precip -PET - (porosity storage)
!           + excess recharge if ksat exceeded
            maxrch = float(ndy(a,i))*geolks(k)
            exc1mon(k) = pptmon(k)-petmon(k)
            if(exc1mon(k).lt.0.) exc1mon(k) = 0.

!           Snow model and soil water storage model calculation
!           (includes antecedent soil water and snow pack from previous month)
!           (includes bedrock ksat as limiting factor)
! alan 
       if(rchrunflag.ne.0) then
        if((recharge.gt.0).and.(rchrunscaler(k).gt.1.)) then
         runoff = runoff + (rchrunscaler(k)-1)*recharge
         recharge = recharge - (rchrunscaler(k)-1)*recharge
         if (recharge.lt.0.) then
          runoff=runoff+recharge
          recharge=0
         endif
        endif
        if((runoff.gt.0).and.(rchrunscaler(k).lt.1.)) then
         recharge = recharge + (1-rchrunscaler(k))*runoff
         runoff = runoff - (1-rchrunscaler(k))*runoff    
          if (runoff.lt.0.) then
           recharge=recharge+runoff
           runoff=0
          endif
        endif
       else
           rchrunscaler(k)=1
       endif
 
       !flooding a pixel allows recharge to equal lowest permeability for the pixel    
      if(floodflag.eq.1) then
       if (flood(k).eq.1.) then !This calulates the floodwater on a monthly basis
         if(soilks(k).gt.geolks(k)) then
          floodwater(j,k)=geolks(k)*ndy(a,i)-recharge+floodwater(j,k) !this is used to calculate a water balance
          recharge=geolks(k)*ndy(a,i)
         else
          gradient=7e-14*soild(k)**6-8e-11*soild(k)**5-8e-6*soild(k)**3+.001*soild(k)**2-0.0668*soild(k)+3.151
          floodwater(j,k)=soilks(k)*ndy(a,i)*gradient-recharge+floodwater(j,k)  
          recharge=soilks(k)*ndy(a,i)*gradient
        endif
        endif
      endif
!if the recharge is greater than the soil conductivity limit recharge to the soil conductivity and add the excess to runoff.  At this point rock conductivity has already limited recharge, now it is time to limit the soil
      !  if (recharge.gt.soilks(k)) then
      !   runoff=runoff+(recharge-soilks(k))
      !   recharge=soilks(k)
      !  endif
      
             rch3mon(k) = recharge
             run2mon(k) = runoff   
 
25   Format(I8,5f11.2)
             
             if(rch3mon(k).gt.maxrch) then 
              rch3mon(k) = maxrch
              run2mon(k) = runoff + (recharge-maxrch)
             endif
             
             if (maxrch.eq.0.) then
             run2mon(k)=0
             rch3mon(k)=0
            endif
            if(rch3mon(k).lt.0.) rch3mon(k) = 0.
            if(run2mon(k).lt.0.) run2mon(k) = 0.
!           if the geology permeability is set to zero make all the output zero
            if(geolks(k).eq.0) then
             if(floodflag.eq.1) then
              flood(k)=0
              floodwater(j,k)=0
             endif
             meltmon(k)=0
             sublmon(k)=0
             packmon(k)=0
             exc1mon(k)=0
             aet1mon(k)=0
             rch3mon(k)=0
             cwd1mon(k)=0
             run2mon(k)=0
             priorstormon(k)=0
             stormon(k)=0
             atimon(k)=0
             defmon(k)=0
             laimon(k)=0
             evapmon(k)=0
             smdmon(k)=0
             smrmon(k)=0
             if(floodflag.eq.1) then
              watbal(k)=aet1mon(k)-rch3mon(k)-run2mon(k)-sublmon(k)+(priorstormon(k)-stormon(k))+(priorpackmon(k)-packmon(k))-evapmon(k)+floodwater(j,k) !pptmon(k) taken out so pptmon(k) not set to zero
             else
              watbal(k)=aet1mon(k)-rch3mon(k)-run2mon(k)-sublmon(k)+(priorstormon(k)-stormon(k))+(priorpackmon(k)-packmon(k))-evapmon(k) !pptmon(k) taken out so pptmon(k) not set to zero
             endif
            else
             if(floodflag.eq.1) then    
              watbal(k)=pptmon(k)-aet1mon(k)-rch3mon(k)-run2mon(k)-sublmon(k)+(priorstormon(k)-stormon(k))+(priorpackmon(k)-packmon(k))-evapmon(k)+floodwater(j,k)
             else
              watbal(k)=pptmon(k)-aet1mon(k)-rch3mon(k)-run2mon(k)-sublmon(k)+(priorstormon(k)-stormon(k))+(priorpackmon(k)-packmon(k))-evapmon(k)
             endif
            endif

            if((watbal(k).gt.5.).or.(watbal(k).lt.-5.)) then
           write(*,*)'WatBal',j,k, watbal(k)
           pause
            endif
            !           count the number of cells with recharge greater than "value"
!           value=0 set in control file (assume this is larger than nodata)
!            jk=rect
            jk=area(k)
!!           sum terms for calculating statistics
!if((k.eq.710.).and.(j.eq.777)) then
!write(*,*)'check pet'
! pause
!endif
           
            avgppt(jk)   = avgppt(jk)   + pptmon(k)
            avgpet(jk)   = avgpet(jk)   + petmon(k)
            avgtmax(jk)  = avgtmax(jk)  + TmaxC
            avgtmin(jk)  = avgtmin(jk)  + TminC
            avgtavg(jk)  = avgtavg(jk)  + TavgC
            avgsnow(jk)  = avgsnow(jk)  + snowmon(k) 
            avgmelt(jk)  = avgmelt(jk)  + meltmon(k)
            avgsubl(jk)  = avgsubl(jk)  + sublmon(k)
            avgpack(jk)  = avgpack(jk)  + packmon(k)
            avgexc1(jk)  = avgexc1(jk)  + exc1mon(k)
            avgaet1(jk)  = avgaet1(jk)  + aet1mon(k)
            avgrch3(jk)  = avgrch3(jk)  + rch3mon(k)
            avgcwd1(jk)  = avgcwd1(jk)  + cwd1mon(k)
            avgrun2(jk)  = avgrun2(jk)  + run2mon(k)
            avgstor(jk)  = avgstor(jk)  + stormon(k)
            avgevap(jk)  = avgevap(jk)  + evapmon(k)
            avgwatbal(jk) = avgwatbal(jk)+ watbal(k)
            avgsmd(jk)   = avgsmd(jk)   + smdmon(k)
            avgsmr(jk)   = avgsmr(jk)   + smrmon(k)
     avgrchrunscaler(jk) = avgrchrunscaler(jk)+rchrunscaler(k)
                        
           endif
1200    continue     
!      write to the output ascii grids
!      substitute ASO snowpack for calculated snowpack
       if (ingestflag.eq.1) then
        do sub=1,numingest
         if(digestfile(sub).eq.packfile(i)) then
          read(sub+124,*) (subdata(k),k=1,cols)
           do k=1,cols
            if ((subdata(k).ne.nodatasub)) then
             packmon(k)=subdata(k)
             if(packmon(k).lt.0.) then
              packmon(k)=0.
             endif
             if(geolid(k).eq.58) then
              packmon(k)=0
             endif
            endif
           enddo
         endif
        enddo    
       endif !ingestflag packmon
       if (ingestflag.eq.2) then
        do sub=1,numingest
         if(digestfile(sub).eq.storfile(i)) then
          read(sub+124,*) (subdata(k),k=1,cols)
          do k=1,cols
           if ((subdata(k).ne.nodatasub)) then
            stormon(k)=subdata(k)
            if(stormon(k).gt.fc(k)*soild(k)*1000) then
             stormon(k)=fc(k)*soild(k)*1000
            endif
            if(stormon(k).lt.0.) then
             stormon(k)=0.
            endif
           endif
          enddo
         endif
        enddo    
       endif !ingestflag stormon
       if (ingestflag.eq.3) then
        do sub=1,numingest
         if(digestfile(sub).eq.laifile(i)) then
          read(sub+124,*) (subdata(k),k=1,cols)
           do k=1,cols
            if ((subdata(k).ne.nodatasub)) then
             laimon(k)=subdata(k)
 !            dnlailimit(veg(k))=subdata(k)
             if(laimon(k).lt.0.) then
              laimon(k)=0.
             endif
             if(geolid(k).eq.58) then
              laimon(k)=0
             endif
            endif
           enddo
         endif
        enddo    
       endif !ingestflag laimon
       write(54,1205) (packmon(k),k=1,cols)
       write(58,1205) (stormon(k),k=1,cols)
       write(62,1205) (atimon(k),k=1,cols)
       write(63,1205) (defmon(k),k=1,cols)
       write(64,1206) (laimon(k),k=1,cols)
       
!      only open files if output is to be stored (this saves lots of storage space)
       if(outstore(a).EQ.1.AND.coutstore.NE.0)then
        write(51,1205) (snowmon(k),k=1,cols)
        write(52,1205) (meltmon(k),k=1,cols)
        write(53,1205) (sublmon(k),k=1,cols)
        write(55,1205) (exc1mon(k),k=1,cols)
        write(56,1205) (aet1mon(k),k=1,cols)
        write(57,1205) (cwd1mon(k),k=1,cols)
        write(59,1205) (rch3mon(k),k=1,cols)
        write(60,1205) (run2mon(k),k=1,cols)
        write(65,1205) (watbal(k),k=1,cols)
        write(66,1205) (evapmon(k),k=1,cols)
        write(67,1205) (smdmon(k),k=1,cols)
        write(68,1205) (smrmon(k),k=1,cols)
       endif
1205   format(20000f12.2)
1206   format(20000f12.4)
       
1100   continue
!     calculate grid statistics - temp6 is the number of areas
      do ii=1,temp6
       jj=ii
       jj=iarea(ii)
!      calculate recharge/runoff ratio and recharge and runoff in acre-ft  
!      depth(mm)*1/1000*cellsize^2*(#active cells)
       if(avgtmax(jj).NE.0.)then
        avgppt(jj)  = avgppt(jj)/float(totcells(jj))
        avgpet(jj)  = avgpet(jj)/float(totcells(jj))
        avgtmax(jj) = avgtmax(jj)/float(totcells(jj))
        avgtmin(jj) = avgtmin(jj)/float(totcells(jj))
        avgtavg(jj) = avgtavg(jj)/float(totcells(jj))
        avgsnow(jj) = avgsnow(jj)/float(totcells(jj))
        avgmelt(jj) = avgmelt(jj)/float(totcells(jj))
        avgsubl(jj) = avgsubl(jj)/float(totcells(jj))
        avgpack(jj) = avgpack(jj)/float(totcells(jj))
        avgexc1(jj) = avgexc1(jj)/float(totcells(jj))
        avgaet1(jj) = avgaet1(jj)/float(totcells(jj))
        avgcwd1(jj) = avgcwd1(jj)/float(totcells(jj))
        avgstor(jj) = avgstor(jj)/float(totcells(jj))
        avgrch3(jj) = avgrch3(jj)/float(totcells(jj))
        avgrun2(jj) = avgrun2(jj)/float(totcells(jj))
        avgevap(jj) = avgevap(jj)/float(totcells(jj))
        avgsmd(jj)  = avgsmd(jj)/float(totcells(jj))
        avgsmr(jj)  = avgsmr(jj)/float(totcells(jj))
        avgwatbal(jj) = avgwatbal(jj)/float(totcells(jj))
        avgrchrunscaler(jj)=avgrchrunscaler(jj)/float(totcells(jj))
!       convert from number of cells to area in km^2
        run2acft(jj)=(avgrun2(jj)/1000.)*cellsize*cellsize*float(totcells(jj))/1233.482D0
        rch3acft(jj)=(avgrch3(jj)/1000.)*cellsize*cellsize*float(totcells(jj))/1233.482D0
        totarea=DBLE(totcells(jj))*cellsize*cellsize
        if(i.lt.4) then 
            aprior=a-1
            iprior=i+9
        else
            aprior=a
            iprior=i-3
        endif
        write(21,1005) aprior,iprior,jj,avgppt(jj),avgpet(jj),avgtmax(jj),avgtmin(jj),avgtavg(jj),avgsnow(jj),avgmelt(jj),avgsubl(jj),avgpack(jj),avgexc1(jj),avgaet1(jj),avgcwd1(jj),avgstor(jj),avgrch3(jj),avgrun2(jj),rch3acft(jj),run2acft(jj),totarea,avgsmd(jj),avgsmr(jj),avgevap(jj),avgwatbal(jj),avgrchrunscaler(jj)
1005    format(i6,i5,i7,1x,15(1x,f9.1),2f12.1,6x,f15.0,3f8.1,1x,f10.1,f10.1)
1015    format(i6,i5,i5,6(1x,f8.2))
         pptyr(jj) = pptyr(jj)  + avgppt(jj)
         petyr(jj) = petyr(jj)  + avgpet(jj)
        tmaxyr(jj) = tmaxyr(jj) + avgtmax(jj)
        tminyr(jj) = tminyr(jj) + avgtmin(jj)
        tavgyr(jj) = tavgyr(jj) + avgtavg(jj)
        snowyr(jj) = snowyr(jj) + avgsnow(jj)
        meltyr(jj) = meltyr(jj) + avgmelt(jj)
        sublyr(jj) = sublyr(jj) + avgsubl(jj)
        packyr(jj) = packyr(jj) + avgpack(jj)
        exc1yr(jj) = exc1yr(jj) + avgexc1(jj)
        aet1yr(jj) = aet1yr(jj) + avgaet1(jj)
        rch3yr(jj) = rch3yr(jj) + avgrch3(jj)
        cwd1yr(jj) = cwd1yr(jj) + avgcwd1(jj)
        run2yr(jj) = run2yr(jj) + avgrun2(jj)
        evapyr(jj) = evapyr(jj) + avgevap(jj)
      watbalyr(jj) = watbalyr(jj) + avgwatbal(jj)        
rchrunscaleryr(jj) = rchrunscaleryr(jj)+avgrchrunscaler(jj)
        endif
      enddo
      close(8)
      close(121)
      if(floodflag.eq.1) then
      close(201)
      endif
      close(9)
      close(10)
      close(11)
      if(ij.gt.1) close(71)          
      if(ij.gt.1) close(72)          
      close(54)
      close(58)
!     only open/close files if output is to be stored (this saves lots of storage space)
      if(outstore(a).EQ.1.AND.coutstore.NE.0)then
       close(51)
       close(52)
       close(59)
       close(60)
      endif
      close(61)
      rewind(20)
      rewind(30)
      rewind(31)
      rewind(32)
      rewind(33)
      rewind(34)
      rewind(35)
      rewind(38)  !the new soil ksat file
! 35 was old soil ksat file
      rewind(36)
      if(rockksflag.eq.1) then
      rewind(37)
      endif
      rewind(118)
   !   rewind(119)
   !   rewind(120)
      rewind(121)
      if(floodflag.eq.1) then
      rewind(201)
      endif
     !rewind(122)
      rewind(123)
    !  rewind(124)
      rewind(125)
      rewind(126)
      rewind(127)
      rewind(128)
      rewind(129)
      rewind(130)
      rewind(131)
      rewind(132)
     if(snowflag.eq.0.) then
      rewind(140)
      rewind(141)
      rewind(142)
     endif
      rewind(143)
     if(maskflag.eq.1) then
      rewind(144)
     endif
      
1000  continue
      close(101)
      close(102)
      close(103)
      close(104)
      close(105)
      close(106)
      close(107)
      close(108)
      close(109)
      close(110)
      close(111)
      close(112)
  
!    compute yearly totals
      do ii=1,temp6
       jk=ii
       jk=iarea(ii)
       if(tmaxyr(jk).NE.0.)then
        tmaxyr(jk) = tmaxyr(jk)/12.
        tminyr(jk) = tminyr(jk)/12.
        tavgyr(jk) = tavgyr(jk)/12.
        storyr(jk) = storyr(jk)/12.
        rchrunscaleryr(jk)=rchrunscaleryr(jk)/12.
!       compute run2 and rch3 in acre-ft
        run2acft(jk)=(run2yr(jk)/1000.)*cellsize*cellsize*float(totcells(jk))/1233.482D0
        rch3acft(jk)=(rch3yr(jk)/1000.)*cellsize*cellsize*float(totcells(jk))/1233.482D0
        totarea=DBLE(totcells(jk))*cellsize*cellsize
        write(22,1006) a,jk,pptyr(jk),petyr(jk),tmaxyr(jk),tminyr(jk),tavgyr(jk),snowyr(jk),meltyr(jk),sublyr(jk),exc1yr(jk),aet1yr(jk),cwd1yr(jk),rch3yr(jk),run2yr(jk),rch3acft(jk),run2acft(jk),totarea,evapyr(jk),watbalyr(jk),rchrunscaleryr(jk)
       endif
      enddo
1006  format(i6,i6,13(1x,f9.1),2f12.1,5x,f13.0,4x,f9.1,2x,f9.1,2x,f9.1)
1016  format(i6,i5,6(1x,f8.2))
700  enddo
     close(7)
     open(unit=7,file='BCM_Monthlyv8.ctl')
1220 format(a400)
     write(21,1220) ' '
     write(22,1220) ' '
     i=1
1400 read(7,1220,end=1500) headerout(i)
     write(21,1220) headerout(i)
     write(22,1220) headerout(i)
     i=i+1
	 goto 1400
1500 continue
     close(7)
     close(21)
     close(22)
     stop
     end
!=============================================================
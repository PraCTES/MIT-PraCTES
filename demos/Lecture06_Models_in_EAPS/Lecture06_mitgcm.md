


# The MIT general circulation model ([MITgcm](https://mitgcm.readthedocs.io/en/latest/)) and Ocean State Estimate solutions ([ECCO](https://eccov4.readthedocs.io/en/latest/))

[MITgcm](https://github.com/MITgcm) is a general circulation model that can simulate the Ocean, Amosphere, sea-ice, ocean biogeochemistry, and marine ecosystems. It can be configured for a wide range of scales -- from a planet to a swimming pool or something like that. It is widely used for ocean modeling and ocean state estimation as done in [ECCO](https://github.com/ECCO-GROUP). 

Here we use this [global ocean configuration](https://eccov4.readthedocs.io/en/latest/), which is [permanently archived](http://dx.doi.org/10.7910/DVN/ODM2IQ ) and optimized to best fit most observations -- incl. [Argo T/S profiles](http://doi.org/10.5194/os-11-839-2015) and [Sea Level data](https://doi.org/10.1016/j.pocean.2015.06.002). For more please see [Forget et al 2015](https://www.geosci-model-dev.net/8/3071/2015/)

![](https://mitgcm.readthedocs.io/en/latest/_images/eddy_on_cubic_globe.svg)         |  ![](https://github.com/gaelforget/GlobalOceanNotebooks/blob/master/OceanTransports/MOC.png)
:------------------------------:|:---------------------------------:
![](https://mitgcm.readthedocs.io/en/latest/_images/scales.png)  |  ![](https://mitgcm.readthedocs.io/en/latest/_images/adv_gyre_maps.png)

MITgcm is coded in Fortran and therefore is manually compiled before one can run the model. `MITgcm` has extensive documentation that, while imperfect, is widely regarded as quite useful.  

Variables are on the C-grid such that U,V velocity components are shifted, and some cases. Let's have a look at some of its inputs & outputs. Namelists are used to specify run time parameters like the duration of the model run.

There is a difference between `unit test` and `curated setup`. [ECCOv4](https://eccov4.readthedocs.io/en/latest/) firmly belongs in the `curated setup` category. In this session we also discuss a simplified atmosphere case which belongs in `unit test` category -- the [Held and Suarez 94](https://doi.org/10.1175/1520-0477(1994)075<1825:APFTIO>2.0.CO;2) dynamical core benchmark.

### Exercise Material

- Exercise 1: Let's compile and run the ECCO model configuration; follow [these instructions](](https://eccov4.readthedocs.io/en/latest/)) with modifications listed below -- since we want to run on 8 cores we want to .
- Exercise 2: set `useSingleCpuIO=.false.` in `input/data` and rerun model; this will generate more files but smaller one. To go further, you can generate netcdf output instead [see docs about pkg/mnc](https://mitgcm.readthedocs.io/en/latest/outp_pkgs/outp_pkgs.html)
- Exercise 3: using output from the atmospheric or ocean model, compute proper global mean in matlab, julia, or python


_Note: various language analysis frameworks exist incl. [gcmfaces](https://gcmfaces.readthedocs.io/en/latest/), [xgcm](https://xgcm.readthedocs.io/en/latest/), and [MeshArrays](https://juliaclimate.github.io/MeshArrays.jl/stable/)_

### ECCO model configuration tweaks

- 1. link these instead of downloading them

```
cd MITgcm/mysetups/ECCOv4
ln -s /data/* .
```

- 2. before the `make -j 4` command, edit compile-time options:

```
--- a/code/SIZE.h
+++ b/code/SIZE.h
-     &           nSx =   1,
+     &           nSx =  12,
      &           nSy =   1,
-     &           nPx =  96,
+     &           nPx =   8,
      &           nPy =   1,
```

- 3. before the `mpiexec` command, edit run-time options:

```
--- a/input/data.pkg
+++ b/input/data.pkg
@@ -8,8 +8,8 @@
  useSALT_PlUME      = .TRUE.,
  useDiagnostics     = .TRUE.,
  useAUTODIFF        = .FALSE.,
- useECCO            = .TRUE.,
+ useECCO            = .FALSE.,
  useCTRL            = .TRUE.,
- useProfiles        = .TRUE.,
+ useProfiles        = .FALSE.,
  useSMOOTH          = .TRUE.,
  &
```

```
--- a/input/data
+++ b/input/data
-#20y:
- nTimeSteps=175295,
+#10 days:
+ nTimeSteps=240,
```

- 4. run model on 8 procs (not 96 as in docs)

`mpiexec -np 8 ./mitgcmuv`


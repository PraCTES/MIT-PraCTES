


# Numerical models -- The MIT general circulation model ([MITgcm](https://mitgcm.readthedocs.io/en/latest/)) and Ocean State Estimation ([ECCO](https://ecco.jpl.nasa.gov/))

`MITgcm` is a general circulation model that can simulate the Ocean, Amosphere, sea-ice, ocean biogeochemistry, and marine ecosystems.

<img src="https://mitgcm.readthedocs.io/en/latest/_images/u_cube_latlon_comb.svg" width="50%">

It is coded in Fortran and therefore is manually compiled before using the model. `MITgcm` has extensive documentation that, while imperfect, is widely regarded as quite useful.  

Variables are on the C-grid such that U,V velocity components are shifted, and some cases. Let's have a look at some of its inputs & outputs. Namelists are used to specify run time parameters like the duration of the model run.

There is a difference between unit test case and curated setup. [ECCOv4](https://eccov4.readthedocs.io/en/latest/) firmly belongs in the latter category. 

- Now lets; compile, analyze, and modify the model ... papers & links
- analysis frameworks (gcmfaces, xgcm, MeshArrays, ...)
- Exercise: proper global mean in julia; redo with xgcm etc
- Exercise: set `useSingleCpuIO` to false in namelist and rerun model 

[https://github.com/JuliaClimate/MeshArrays.jl/blob/master/docs/images/sphere_all.png]()

```
cd MITgcm/mysetups/ECCOv4
ln -s /data/* .
```

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


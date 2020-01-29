- ocn, atm, ice, bgc, etc
- fortran, compiled, docs
- grids (global=cs/llc), C-grid, scaling factors
- inputs / outputs, codes / namelists
- unit tests vs curated setup
- ECCO ... papers & links ... compile / run analyze ... modify
- analysis frameworks (gcmfaces, xgcm, MeshArrays, ...)
- proper global mean in julia (exercise: redo with xgcm etc)
- change useSingleCpuIO in MITgcm (exercise: redo global mean)

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


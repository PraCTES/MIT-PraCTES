
### Notes from running SAM at the workshop (G.F. & T.A.)

The model is inside `~/SAM6.11.3/` and **key files include**:

- `SRC/domain.f90` sets the number of grid points and cores
- `SRC/setgrid.f90` computes the vertical grid
- `CaseName` sets the name of the configuration (e.g. `DYNAMO` or `RCE`)
- `Build` is the build script that compiles the model executable
- `SAM_ADV_MPDATA_SGS_TKE_RAD_CAM_MICRO_SAM1MOM` is the executable
- `RCE/prm` is the main file settings the model run-time parameters
- `RCE/grd` ...
- `RCE/lsf` ... 
- `RCE/sfc` ... 
- `RCE/snd` ... 
- `UTIL/*` ...

**Model output** generated during the model simulation will go to `~/SAM_RUNS/`

- `setenv SAM_SCR ~/SAM_RUNS` will set the model run directory
- `~/SAM6.11.3/UTIL/stat2nc RCE_128x128x64.stat` will generate netcdf file
- analysis tools are available in [the SAMtools repo](https://github.com/thabbott/python/tree/master/SAMtools)

**Reference** Khairoutdinov and Randall, 2003: Cloud Resolving Modeling of the ARM Summer 1997 IOP: Model Formulation, Results, Uncertainties, and Sensitivities. J. Atmos. Sci., [D.O.I.](https://doi.org/10.1175/1520-0469(2003)060<0607:CRMOTA>2.0.CO;2)
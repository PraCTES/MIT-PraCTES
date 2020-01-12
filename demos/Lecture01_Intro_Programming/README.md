## Instructions for demos

The *Introduction to Programming* tutorial is available in the three high-level, interpretive programming languages commonly used in earth science research: [Python](https://www.python.org/), [Julia](https://docs.julialang.org/en/v1/), and [Matlab](https://www.mathworks.com/products/matlab.html).

The Python and Julia notebooks can be run in two ways: via a ["binder"](https://mybinder.org/v2/gh/PraCTES/MIT-PraCTES/master?urlpath=lab) (an online, interactive environment that can be run directly in your browser) or by installing the language's core software on your personal machine, as well as any additional software packages (see how to do this using packages managers below).


## Step-by-step instructions to setting up your programming environment

### Python

1. Follow the [instructions to install Anaconda](https://docs.conda.io/projects/conda/en/latest/user-guide/install/), a Python package manager which handles the complex inter-dependences between various Python packages behind the scenes.

2. Follow the [instructions to launch Anaconda](https://docs.conda.io/projects/conda/en/latest/user-guide/getting-started.html#starting-conda) (conda for short).

3. Navigate to the `/MIT-PraCTES/` root directory and type the command `conda env update -f binder/environment.yml` to create (and, if it already exists, update) a conda environment based on the `environment.yml` file, which lists the Python packages we want to install and should look something like:
```yml
name: PraCTES
channels:
  - conda-forge
dependencies:
  - jupyterlab >= 1.0
  - python
  - numpy
  - netcdf4
  - pandas
  - matplotlib
  - cartopy
  - dask
  - xarray
  - nodejs
```

4. Activate the newly created conda environment with `conda activate PraCTES`.

5. Open a [jupyter-lab](https://jupyterlab.readthedocs.io/en/stable/) instance with the command `jupyter-lab`.

6. Use the filesystem browser in the top left to navigate to `/MIT-PraCTES/demos/Lecture01_Intro_Programming/` and open the Python tutorial notebook `Lecture01_intro_programming.ipynb`.

### Julia

1. [Download](https://julialang.org/downloads/) and install Julia (> v.1.0.0)

2. Launch julia

3. Use the julia ["shell mode"](https://docs.julialang.org/en/v1/stdlib/REPL/#man-shell-mode-1) to navigate to the `MIT-PraCTES` root directory

4. Activate the julia ["Pkg REPL"](https://docs.julialang.org/en/v1/stdlib/Pkg/index.html) (a package manager mode) by typing the `]` key.

5. Within the julia Pkg REPL, type `activate binder` to activate the julia environment described by the `Project.toml` and `Manifest.toml` files. The `Project.toml` contains the high-level instructions for the packages (depdendencies or deps for short) to be installed:
```toml
[deps]
IJulia = "7073ff75-c697-5162-941a-fcdaad2a7d2a"
NetCDF = "30363a11-5582-574a-97bb-aa9a979735b9"
PyPlot = "d330b81b-6aea-500a-939a-2ce795aea3ee"
```
while the `Manifest.toml` file contains a comprehensive list of every single julia package the above packages depend upon.

6. Press the `[Backspace]` or `[Delete]` buttons to return to the julia REPL

7. In the julia REPL type:

```julia
using IJulia
jupyterlab()
```
To activate [jupyterlab](https://jupyterlab.readthedocs.io/en/stable/) and launch a jupyterlab instance.

8. Use the filesystem browser in the top left to navigate to `/MIT-PraCTES/demos/Lecture01_Intro_Programming/` and open the julia tutorial notebook `Lecture01_intro_programming_jl.ipynb`.

### Matlab

1. [Download](https://www.mathworks.com/downloads/) and install Matlab (you may be able to get a free license through your academic institution or company - if not, a personal license can be very pricey and we recommend the free and open-source alternatives above).

2. Launch Matlab

3. Navigate to `/MIT-PraCTES/demos/Lecture01_Intro_Programming/` and open the Matlab script `Lecture01_intro_programming.m`.

## Instructions for demos

The *Introduction to Programming* tutorial is available in the three high-level, interpretive programming languages commonly used in earth science research: [Python](https://www.python.org/), [Julia](https://docs.julialang.org/en/v1/), and [Matlab](https://www.mathworks.com/products/matlab.html).

The Python and Julia notebooks can be run in two ways: via a ["binder"](https://mybinder.org/v2/gh/PraCTES/MIT-PraCTES/master?urlpath=lab) (an online, interactive environment that can be run directly in your browser) or by installing the language's core software on your personal machine, as well as any additional software packages (see how to do this using packages managers below).


## Step-by-step instructions to setting up your programming environment

### Python

1. Follow the [instructions to install Anaconda](https://docs.conda.io/projects/conda/en/latest/user-guide/install/), a Python package manager which handles the complex inter-dependences between various Python packages behind the scenes.

2. Follow the [instructions to start Anaconda](https://docs.conda.io/projects/conda/en/latest/user-guide/getting-started.html#starting-conda) (conda for short).

3. Navigate to the `/MIT-PraCTES/` root directory and type the command `conda env update -f binder/environment.yml` to create (and, if it already exists, create) a conda environment based on the `environment.yml` file, the contents of which describe the Python packages we want to install in the environment and should look something like:
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

6. User the filesystem browser in the top left to navigate to `/MIT-PraCTES/demos/Lecture01_Intro_Programming/` and open the Python tutorial `Lecture01_intro_programming.ipynb`.

### Julia

### Matlab



# Organizing version-controlled, reproducible, and efficient scientific projects

This session focuses on tools and methods (_section 1._) that make it easy to organize, share, reproduce, and maintain your scientific computations and collaborate with others on code (_s. 2._). Representative examples are provided to illustrate these tools at work. Self-guided exercises are found at the end of this file. 

**Table of contents**

- 1. Set Up Your Project & Version Control
- 2. Maintain And Reproduce Your Results
- 3. Exercises & Additional Ressources

**Ahead of this session, it is suggested** that you create a [github.com](http://github.com/) account if you do not have one yet. The [github guides](https://guides.github.com) provide very useful examples. **Exercise #0** in fact is just their `Hello World` guide -- a great way to get ready for this session. If you want to get further ahead, please consider: 

- the listed webpages and documentations.
- the exercises using our [cloud instance](https://mybinder.org/v2/gh/PraCTES/MIT-PraCTES/master?urlpath=lab).

![](https://github.blog/wp-content/uploads/2019/01/Community-Updated-2X.png?resize=1024,611)         |  ![](https://image.slidesharecdn.com/ci-131213183852-phpapp01/95/github-travis-ci-coveralls-maven-ci2-19-638.jpg)
:------------------------------:|:---------------------------------:
![](https://camo.githubusercontent.com/6d7260eaa159cfb838b45bf168ff77adecf8b198/68747470733a2f2f6a756c69616c616e672e6f72672f696d616765732f6c6f676f5f68697265732e706e67)  |  ![](https://insights.tuhh.de/wp-content/uploads/2019/02/jupyter-workflow-973x1024.png)

## 1. Set Up Your Project & Version Control

- github ~ git + social networking + apps / cloud services
	- Get started with [GitHub](https://github.com/) & [command-line git](https://education.github.com/git-cheat-sheet-education.pdf) in **exercise 0**
	- Get more familiar with command-line tools (**exercise 2**)
	- Organization examples: [MITgcm](https://github.com/mitgcm), [JuliaDynamics](https://github.com/juliadynamics), [JuliaClimate](https://github.com/JuliaClimate)
	- Repo examples: [MITgcm repo](https://github.com/mitgcm/mitgcm), [JuliaDynamics repo](https://github.com/juliadynamics/agents.jl), [JuliaClimate repo](https://github.com/meta)
	- App examples: [GitHub Help](https://help.github.com/en/github/authenticating-to-github/authorizing-oauth-apps), [zenodo](https://zenodo.org), [Travis CI](https://travis-ci.org)
	
_Notes:_ Linked repos provide examples of how _pull requests_, _git commits_, _issue trackers_, _stars_, etc provide user-friendly ways to collaborate on computational projects, document code changes and related discussions, etc. and maintain them through time.

- documentation 
	- [Mastering Markdown](https://guides.github.com/features/mastering-markdown/), [MacDown](https://macdown.uranusjr.com/) app for macOS.
	- [Documenter.jl](https://juliadocs.github.io/Documenter.jl/stable/) & [docstrings](https://docs.julialang.org/en/v1/manual/documentation/index.html) in Julia.
	- [reStructuredText](https://www.sphinx-doc.org/en/master/usage/restructuredtext/index.html) and [readthedocs](https://readthedocs.org) as an alternative.
	- pull requests, issues, star, watch as documentation (see repo examples).

- version control
	- [git-handbook](https://guides.github.com/introduction/git-handbook/) tutorial from GitHub
	- [the github GUI](https://github.com/PraCTES/MIT-PraCTES) (e.g. history, blame, commits, releases; see repo examples)
	- [package registries](https://github.com/JuliaRegistries/General) & [package managers](https://julialang.github.io/Pkg.jl/v1/) (in python: conda, pip, etc).
	- dependencies & compatibility constraints (e.g. `*.toml`)
	- scripts (git) vs notebooks (jupytext) vs binary (dataverse)

- unit testing 
	- reference result, test formula, computer
	- **exercise 2** provides examples
	- in **exercise 4** you try it yourself
	- automation: [Travis CI](https://docs.travis-ci.com)

- packages
	- see previous lecture for `'python` examples. Here is a [Julia example](https://github.com/gaelforget/IndividualDisplacements.jl)
	- creating (**exercise 4**), registering, versioning, archiving, and distributing
	- [Pkg.jl](https://julialang.github.io/Pkg.jl/v1/), [PkgTemplates.jl](https://invenia.github.io/PkgTemplates.jl/stable/)
	- environmments / dependencies inside `.julia/` and packages 
	- releases, semantic versioning, DOI / zenodo
	- Julia registrator and tag bots / apps

## 2. Maintain And Reproduce Your Results

- archiving and versioning
	- [Making Your Code Citable](https://guides.github.com/activities/citable-code/)
	- [Dataverse](https://dataverse.harvard.edu/dataverse/ECCOv4r2) or [zenodo](http://doi.org/10.5281/zenodo.3461529) for data sets
	- doi, zenodo, dataverse vs ftp, html, cloud hosting
- maintainance & user support
	- [Mastering Issues](https://guides.github.com/features/issues/) help
	- Help with pull requests, Issues
	- Regular, continued unit testing
	- Keep up with dependency updates
	- debuggers [like this one](https://github.com/JuliaDebug/Debugger.jl)
	- documentation ...
- collaborators, modularity, coding practices, reuse vs recode, etc
	- [Mastering Forks](https://guides.github.com/activities/forking/)
	- **exercise 3**
- Real-Life Examples [MITgcm](http://mitgcm.readthedocs.io/en/latest/?badge=latest) & [ECCO](https://eccov4.readthedocs.io/en/latest/?badge=latest) of stuff you should be able to re-run anytime

## 3. Exercises & Additional Ressources

All of the exercises can be done using the cloud-based environment included in the workshop material. Just hit the `binder` badge in the [workshop landing page](https://github.com/PraCTES/MIT-PraCTES) to get it started.

This method is freely provided by [mybinder.org]() and it uses the [jupyterlab](https://jupyterlab.readthedocs.io/en/stable/) interface in your web browser (_tip: hrome seems to work well for this_). You can also use your own computer if that's more convenient though.

### Exercise #0 -- GitHub Hello World

Follow the directions in the [Hello World](https://guides.github.com/activities/hello-world/) guide using your web-browser and your own laptop -- i.e., **not** the `binder` / `jupyterlab` cloud instance used for the rest of the exercises. 

The _Hello World_ guide will lead you to set up a [GitHub.com](https://github.com/) account and start using [git](https://en.wikipedia.org/wiki/Git) for [Version control](https://en.wikipedia.org/wiki/Version_control). This commonly used framework is a focus of this session. Then, in the _Hello World_ guide, you will learn to use `git` to track repositories (_repos_). You can think of those as folders and dowload them to any computer (_git clone_ them). `GitHub.com` is something like `git` + social networking + apps / cloud services.

### Exercise #1 -- Command Line Git

In the Jupyter lab side bar, or the launcher pane, click to launch a terminal window where you will be able to type:


```
cd ~
git clone https://github.com/PraCTES/MIT-PraCTES
cd MIT-PraCTES
ls -la
```

In the Jupyter lab side bar (not the open terminal pane), double click on `demos/README.md` to open it the build in text editor. Add a blank line, save, and close the file. Then go back to the open terminal pane, and create a new folder that we will then turn into a git repository. 

```
cd ~
mkdir test20200117
cd test20200117
```

`git init` is next -- it will set up `git` to track files inside the `test20200117/` folder. Try `ls -la` before and after to see the hidden `.git/` folder appear. The sequence of commands reprted below further creates an empty file (`touch readme`), makes git track this file (`git add`), and then saves the file (`git commit`). 

The final command will ask for a username and email once -- this is the key information that git uses for its book-keeping and attribute authorship to contributions inside the `.git/` folder.

```
git init
git status
touch readme
git status
git add readme
git status
git commit -m "initial commit"
```

The `git status` commands were inserted several time for demonstrative purposes. `git log` ... 

Next is `git remote` then `git push` ... 

### Exercise #2 -- Command Line & Unit Tests

Download, compile, and run MITgcm on one of the unit tests:

```
$ cd ~
$ git clone https://github.com/MITgcm/MITgcm
$ cd MITgcm/verification/
$ ./testreport -t advect_cs
```

Download a `Julia` package (e.g. `MeshArrays.jl`) from github and start julia.

```
$ git clone https://github.com/gaelforget/MeshArrays.jl
$
$ julia
               _
   _       _ _(_)_     |  Documentation: https://docs.julialang.org
  (_)     | (_) (_)    |
   _ _   _| |_  __ _   |  Type "?" for help, "]?" for Pkg help.
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  Version 1.3.1 (2019-12-30)
 _/ |\__'_|_|_|\__'_|  |  Official https://julialang.org/ release
|__/                   |
julia> 
```

This is the basic julia mode where you can execute operations (e.g. try `julia> 1+1` & return). One can access the package manager mode by typing `]`. Then `pkg> add MeshArrays.jl` & return will add the chosen package to the `Julia` environment. 

In package manager mode (`]`), one can then run the unit tests for this package using `test MeshArrays`. If all unit tests were successful, the final display should look something like this:

```
   Testing MeshArrays
 Resolving package versions...
    Status `/tmp/jl_8pEyMo/Manifest.toml`
  [81a5f4ea] CatViews v1.0.0
  [cb8c808f] MeshArrays v0.2.4
...
Test Summary:     | Pass  Total
MeshArrays tests: |   15     15
   Testing MeshArrays tests passed 
```

To leave the package mode and go back to the basic `julia>` prompt hit the `esc` keystroke. You can then start using the `Mesharrays.jl` package that we just been added. For example:

```
julia> using MeshArrays
julia> (Rini,Rend,DXCsm,DYCsm)=MeshArrays.demo2()
julia> show(Rend)
```

To access the documentation of a function in Julia one can just type `?` followed by e.g. the name of the package / module followed by the function name (e.g. `?MeshArrays.demo2`). This will print out the `docstring` which is inside the package code, just next to the `demo2` function definition (see [the julia docs](https://docs.julialang.org/en/v1/manual/documentation/index.html) for detail).

```
help?> MeshArrays.demo2
  demo2()

  Demonstrate higher level functions using smooth. Call sequence:

  (Rini,Rend,DXCsm,DYCsm)=MeshArrays.demo2();
```

Now let's look inside the Julia package we have downloaded. We see a `.git` as before but also a documentation folder, `docs/`, and the `src/` folder where the code is. The `*.toml` files document the package dependencies and the `.travis.yml` file is used to automate the unit testing via the [https://travis-ci.org/]() cloud service. 

```
$ ls -la MeshArrays.jl/
total 52
...
drwxr-xr-x 4 jovyan root   4096 Jan 17 14:50 docs
drwxr-xr-x 8 jovyan root   4096 Jan 17 14:50 .git
-rw-r--r-- 1 jovyan root     82 Jan 17 14:50 .gitignore
-rw-r--r-- 1 jovyan root   1163 Jan 17 14:50 LICENSE.md
-rw-r--r-- 1 jovyan root   1429 Jan 17 14:50 Manifest.toml
-rw-r--r-- 1 jovyan root    464 Jan 17 14:50 Project.toml
-rw-r--r-- 1 jovyan root   2457 Jan 17 14:50 README.md
-rw-r--r-- 1 jovyan root     10 Jan 17 14:50 REQUIRE
drwxr-xr-x 2 jovyan root   4096 Jan 17 14:50 src
drwxr-xr-x 2 jovyan root   4096 Jan 17 14:50 test
-rw-r--r-- 1 jovyan root   1430 Jan 17 14:50 .travis.yml
```

If you go to the [repo on github.com](https://github.com/gaelforget/MeshArrays.jl) then the landing page should show a `README.md` (scroll down maybe) with a `build` badge. Clicking on this badge will get you to the results of automated unit tests generated using the [https://travis-ci.org/]() cloud service. The docs badge will in turn lead you to the [hosted documentation](https://gaelforget.github.io/MeshArrays.jl/stable/) based on what's in `docs/`.

To take this exercise a git further, let's now install a registered package by typing `]` then `add IndividualDisplacements.jl`. Notice that `julia`'s package manager takes care of the `git clone` part for registered packages. Wondering where the package got downloaded? Press `esc` then type `pathof(IndividualDisplacements)` to find out.

Finally:

- To display the list of packages currently installed : type `]` then `st`.
- To remove a packages from `julia` : `]` then e.g. `git rm IndividualDisplacements.jl `. 

### Exercise #3 -- Practical Collaboration Setup

The approach described in the [MITgcm manual's](https://mitgcm.readthedocs.io/en/latest/contributing/contributing.html) is a typical, practical collaborative  model. The basic configuration is depicted below and your exercise is to set this up for the repo from the class. 

To this end, you will first fork (github), clone (terminal), and branch (terminal) [as explained here](https://mitgcm.readthedocs.io/en/latest/contributing/contributing.html). 

Then, in the terminal window, execute the following command:

`git remote add upstream https://github.com/PraCTES/MIT-PraCTES`

This will allow you to 

- 1. modify codes and **commit** this changes to your **local branch** (on your laptop, e.g. all offline)
- 2. **push** your branch back to your repo on `Github.com` and possibly see your changes **merged in the upstream repo** repo via a **pull request**.

But imagine a scenario where multiple people are doing this and code changes happen in the main repo while you were in the middle of doing your changes to the code? No panic -- that is why the above schematic shows a connection between upstream and your local clone. 

The `pull upstream` command effectively brings the latest updates from the main repo to your local copy /clone clone. This will then allow you to **merge** these updates from upstream into your local branches, and then push them to your online repo (see [this doc](https://mitgcm.readthedocs.io/en/latest/contributing/contributing.html)). We will do this in **exercise #4b** using a practice repo.

<img src="https://mitgcm.readthedocs.io/en/latest/_images/git_setup.svg" width="50%">

### Exercise #4a -- Create a package

Create a julia package, add tests, add docs, and push to your github account following [this guide](https://julialang.github.io/Pkg.jl/v1/creating-packages/)

### Exercise #4b -- Collaborate on pkg

- 1. Pair up and setup the structure depicted in **example 3** using one-another's repo from **example 4a** as the `main repo`. 
- 2. Use `git branch`, `git commit`, `git push`, `PRs`, `git merge`, etc back and forth between your two github accounts using your own laptops for command-line git.

### Exercise #5

Overload `Julia`'s `heatmap` function from the `Plots.jl` package to set color scale to pre-specified ranges when this information is build into a custom array type. We did something similar in `session #2` using `Python`. 

Here we want to highlight some of the key aspects in which `Julia` differs from `Python`: type system, _functions are objects_, type annotations, multiple dispatch, broadcasting, unicode, ...

Let's first define a custom array data type that contains a range...

### Exercise #6

Install and try [jupytext](https://jupytext.readthedocs.io/en/latest/install.html) in the Jupyter lab environments or on your laptop. Using the paired _light scripts_ appraoch where `.jl` files are kept in sync with your notebooks (as in [this example](https://github.com/gaelforget/MeshArrayNotebooks)) can make it easier to trace back changes made to jupyter notebooks using `git` and `github`.

### Exercise #7 

Mix and Match packages to create something new. E.g. `Juls.jl`, `MeshArrays.jl`, `IndividualDisplacements.jl`, compute transports (e.g. [Forget & Ferreira 2019](https://doi.org/10.1038/s41561-019-0333-7)).

### Additional Resources

- Check out these other tools: aws, docker, slack, overleaf, nextjournal, jupytext, [youtube](https://www.youtube.com/watch?v=RDxAy_zSUvg&feature=youtu.be), ...
- [https://opensource.guide/how-to-contribute/]()
- [https://www.youtube.com/user/JuliaLanguage]()
- [mybinder.org]() by itself
- [https://julialang.org/learning/]()
- Besides github: [bitbucket](https://www.atlassian.com/git/tutorials), [gitlab](https://www.tutorialspoint.com/gitlab/index.htm), etc.
- Besides jupyter: e.g. [atom and juno](https://junolab.org)

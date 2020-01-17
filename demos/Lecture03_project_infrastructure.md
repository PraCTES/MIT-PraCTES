

# Organizing version-controlled, reproducible, and efficient scientific projects

Here we will run through tools and methods (_section 1._) that will make it much easier to later share, reproduce, and maintain your scientific computations (_s. 2._). A couple representative examples are provided to illustrate these tools at work (_s. 3._). Self-guided exercises are found at the end of this file. 

**Ahead of this session, it is suggested** that you create a [github.com](http://github.com/) account if you do not have one yet. The [github guides](https://guides.github.com) provide very useful examples. **Exercise #0** in fact is just `Hello World guide` using git on your laptop -- a great way to get ready for this session. If you want to get further ahead, please consider: 

- the webpages and documentations listed below.
- exercises 2 and 3 using the repo `binder` link.

_**Note: this file is work in progress -- further modifications & cleanup are expected by tomorrow.**_

![](https://github.blog/wp-content/uploads/2019/01/Community-Updated-2X.png?resize=1024,611)         |  ![](https://image.slidesharecdn.com/ci-131213183852-phpapp01/95/github-travis-ci-coveralls-maven-ci2-19-638.jpg)
:------------------------------:|:---------------------------------:
![](https://camo.githubusercontent.com/6d7260eaa159cfb838b45bf168ff77adecf8b198/68747470733a2f2f6a756c69616c616e672e6f72672f696d616765732f6c6f676f5f68697265732e706e67)  |  ![](https://insights.tuhh.de/wp-content/uploads/2019/02/jupyter-workflow-973x1024.png)

## 1. Set up your project 

- github, git & cloud services
	- Get started with [GitHub](https://github.com/) & command-line `git` in **exercise 0**
	- Get more familiar with command-line `git` & `julia` (**exercise 2**)
	- Organization examples: [MITgcm](https://github.com/mitgcm), [JuliaDynamics](https://github.com/juliadynamics)
	- Apps examples: [GitHub Help](https://help.github.com/en/github/authenticating-to-github/authorizing-oauth-apps), [zenodo](https://zenodo.org), [Travis CI](https://travis-ci.org)
- documentation 
	- [Mastering Markdown](https://guides.github.com/features/mastering-markdown/), [MacDown](https://macdown.uranusjr.com/) app for macOS.
	- [Documenter.jl](https://juliadocs.github.io/Documenter.jl/stable/) & [docstrings](https://docs.julialang.org/en/v1/manual/documentation/index.html) in Julia.
	- [reStructuredText](https://www.sphinx-doc.org/en/master/usage/restructuredtext/index.html) and [readthedocs](https://readthedocs.org) as an alternative.
	- GitHub PRs & issues
- version control (git, blame, commit, releases, ... pkg registry/manager)
	- [git-handbook](https://guides.github.com/introduction/git-handbook/)
	- [MITgcm repo](https://github.com/mitgcm/mitgcm), [JuliaDynamics repo](https://github.com/juliadynamics/agents.jl)
	- via the github GUI (blame, commits, releases)
	- package registries & package managers
	- dependencies & compatibility constraints
- unit testing (ref result, formulate test, computer, ... automation, travis)
	- **exercise 2** gives two examples
	- [Travis CI](https://docs.travis-ci.com)
- archiving (doi, zenodo, dataverse ... vs ftp, html, cloud hosting)
	- [Making Your Code Citable](https://guides.github.com/activities/citable-code/)
	- [Dataverse](https://dataverse.harvard.edu/dataverse/ECCOv4r2) or [zenodo](http://doi.org/10.5281/zenodo.3461529) for data sets
- registering and distributing a package
	- [Pkg.jl](https://julialang.github.io/Pkg.jl/v1/), `PkgTemplates.jl` and `registries` in Julia
	- environmments / dependencies / `.julia/`
	- [mybinder.org]() by itself
	- ...

## 2. Maintain and reproduce your results

- maintainance & user support
	- [Mastering Issues](https://guides.github.com/features/issues/)
	- Pull requests, dependency updates, documentation
- collaborators, modularity, coding practices, reuse vs recode, etc
	- [Mastering Forks](https://guides.github.com/activities/forking/)
	- **exercise 3**

## 3. Real-Life Examples

- Julia & diagnostic examples
	- docs, tests, apps, ... 
	- `MeshArrays.jl`, `IndividualDisplacements.jl`, Forget & Ferreira 2019
- [MITgcm](http://mitgcm.readthedocs.io/en/latest/?badge=latest) & [ECCO](https://eccov4.readthedocs.io/en/latest/?badge=latest) examples
	- org, repo, doc, blame, issues, PRs, tests, upstream, ...
	- ECCO, archives, doc, rerun, test, notebooks, ...

## 4. Additional resources & Exercises

- Check out these other tools: aws, docker, slack, overleaf, nextjournal, jupytext, [youtube](https://www.youtube.com/watch?v=RDxAy_zSUvg&feature=youtu.be), ...
- [https://opensource.guide/how-to-contribute/]()
- [https://www.youtube.com/user/JuliaLanguage]()

### exercise #0

Follow the directions in [Hello World](https://guides.github.com/activities/hello-world/) using your web-browser and your own laptop -- i.e., **not** the `binder` / `jupyterlab` cloud instance used for the rest of the exercises

### exercise #1

In the Jupyter lab side bar, or the launcher pane, click to launch a terminal window where you will be able to type:


```
cd ~
git clone https://github.com/PraCTES/MIT-PraCTES
cd MIT-PraCTES
ls -la
```

In the Jupyter lab side bar (not the open terminal pane), double click on `demos/README.md` to open it the build in text editor. Add a blank line, save, and close the file. 

Then go back to the open terminal pane, and type:

```
git status
git diff
git diff
git remote -v
```

Next we will try `git commit` ...


### exercise #2

Download, compile, and run MITgcm on one of the unit tests:

```
cd ~
git clone https://github.com/MITgcm/MITgcm
cd MITgcm/verification/
./testreport -t advect_cs
```

Download a `Julia` package (e.g. `MeshArrays.jl`) from github and start julia.

```
$ git clone https://github.com/gaelforget/MeshArrays.jl
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

Then in the terminal window you can access the package manager using the `]` stroke. Type `] add MeshArrays.jl` followed by `] test MeshArrays`.

To come back to the normal `julia>` prompt use the `esc` stroke. You can then start using the `Mesharrays.jl` package. For example:

```
julia> using MeshArrays
julia> (Rini,Rend,DXCsm,DYCsm)=MeshArrays.demo2()
julia> show(Rend)
```

To finish this exercise, install a registered package directly via `Julia` package manager. 

Where did it get installed though? See:

```
julia> using IndividualDisplacements
julia> pathof(IndividualDisplacements)
```

You can also remove the package from the Julia environment 


### exercise #3

The approach described in the [MITgcm manual's](https://mitgcm.readthedocs.io/en/latest/contributing/contributing.html) is a typical, practical collaborative  model. The basic configuration is depicted below and your exercise is to set this up for the repo from the class. 

You will first want to fork (github), clone (terminal), and branch (terminal) [as explained here](https://mitgcm.readthedocs.io/en/latest/contributing/contributing.html). 

Then, in ther terminal, you will want to run:

`git remote add upstream https://github.com/PraCTES/MIT-PraCTES`

This will allow you to 

- 1. modify codes and **commit** this changes to your **local branch** (on your laptop, e.g. all offline)
- 2. **push** your branch back to your repo on `Github.com` and possibly see your changes **merged in the upstream repo** repo via a **pull request**.

But imagine a scenario where multiple people are doing this and changes happen in the main repo when your are in the middle of doing your changes to the code?

That is why the schematic shows a connection between upstream and your local clone. The `pull upstream` command effectively downloads the updated repo to your local clone. 

In the future this will allow you to **merge** updates from upstream into your local branch. We will do this in **exercise #4b** using a practice repo.

<img src="https://mitgcm.readthedocs.io/en/latest/_images/git_setup.svg" width="50%">

### exercise #4a

Create a julia package, add tests, add docs push to your github account

### exercise #4b

Pair up, setup the structure of **example 3** for each other's repo, and practice `git branch`, `git commit`, `git push`, `PRs`, `git merge`, etc on the practice repo from **example 4a**.

### exercise #5

Overload heatmap to set color scale for a custom array data type that contains a range.





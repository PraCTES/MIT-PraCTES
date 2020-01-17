

# Organizing version-controlled, reproducible, and efficient scientific projects

Here we will run through tools and methods that will allow you to set up your science & computations so that they will easy to reproduce and maintain in the future. A couple representative examples are then provided to illustrate these tools at work. Self-guided exercises are found at the end of this file. 

**Ahead of this session, it is suggested** that you create a [github.com](http://github.com/) account if you do not have one yet. The [github guides](https://guides.github.com) provide very useful examples. **Exercise #0** in fact is just `Hello World guide` using git on your laptop -- a great way to get ready for this session. If you want to get further ahead, please consider: 

- the webpages and documentations listed below.
- exercises 2 and 3 using the repo `binder` link.

_**Note: this file is work in progress -- further modifications & cleanup are expected by tomorrow.**_

![](https://github.blog/wp-content/uploads/2019/01/Community-Updated-2X.png?resize=1024,611)         |  ![](https://image.slidesharecdn.com/ci-131213183852-phpapp01/95/github-travis-ci-coveralls-maven-ci2-19-638.jpg)
:------------------------------:|:---------------------------------:
![](https://camo.githubusercontent.com/6d7260eaa159cfb838b45bf168ff77adecf8b198/68747470733a2f2f6a756c69616c616e672e6f72672f696d616765732f6c6f676f5f68697265732e706e67)  |  ![](https://insights.tuhh.de/wp-content/uploads/2019/02/jupyter-workflow-973x1024.png)

## Set up your project 

- github, git & cloud services (guides, accounts / organizations, apps / cloud)
	- Getting started with `Github` & `git`: [Hello World](https://guides.github.com/activities/hello-world/)
	- command line git (& julia, ls, cat, etc)
	- Organization examples: [JuliaDynamics](https://github.com/juliadynamics), [MITgcm](https://github.com/mitgcm)
	- Apps examples: [GitHub Help](https://help.github.com/en/github/authenticating-to-github/authorizing-oauth-apps), [zenodo](https://zenodo.org), [Travis CI](https://travis-ci.org)
- documentation (md, jupyter, docstrings, ... rst, tex, ... PRs, issues)
	- [Mastering Markdown](https://guides.github.com/features/mastering-markdown/), [MacDown](https://macdown.uranusjr.com/) app for macOS.
	- [Documenter.jl](https://juliadocs.github.io/Documenter.jl/stable/) & [docstrings](https://docs.julialang.org/en/v1/manual/documentation/index.html) in Julia.
	- [reStructuredText](https://www.sphinx-doc.org/en/master/usage/restructuredtext/index.html) and [https://readthedocs.org]() as an alternative.
- version control (git, blame, commit, releases, ... pkg registry/manager)
	- [git-handbook](https://guides.github.com/introduction/git-handbook/)
	- ...
- unit testing (ref result, formulate test, computer, ... automation, travis)
	- [Travis CI](https://docs.travis-ci.com)
	- ...
- archiving (doi, zenodo, dataverse ... vs ftp, html, cloud hosting)
	- [Making Your Code Citable](https://guides.github.com/activities/citable-code/)
	- [Dataverse](https://dataverse.harvard.edu/dataverse/ECCOv4r2) or [zenodo](http://doi.org/10.5281/zenodo.3461529) for data sets
- registering and distributing a package
	- [Pkg.jl](https://julialang.github.io/Pkg.jl/v1/), `PkgTemplates.jl` and `registries` in Julia
	- environmments / dependencies / `.julia/`
	- [mybinder.org]() by itself
	- ...

## Maintain and reproduce your results

- maintainance & user support (issues, PRs, ... email, slack, etc)
	- [Mastering Issues](https://guides.github.com/features/issues/)
	- ...
- dependencies, collabs, modularity, coding practices, ... reuse vs recode, etc
	- [Mastering Forks](https://guides.github.com/activities/forking/)
	- ...

## Real-Life Examples

- Julia & diagnostic examples
	- docs, tests, apps, ... 
	- `MeshArrays.jl`, `IndividualDisplacements.jl`, Forget & Ferreira 2019
- [MITgcm](http://mitgcm.readthedocs.io/en/latest/?badge=latest) & [ECCO](https://eccov4.readthedocs.io/en/latest/?badge=latest) examples
	- org, repo, doc, blame, issues, PRs, tests, upstream, ...
	- ECCO, archives, doc, rerun, test, notebooks, ...

## More tools and resources

- Check out these other tools: aws, docker, slack, overleaf, nextjournal, jupytext, [youtube](https://www.youtube.com/watch?v=RDxAy_zSUvg&feature=youtu.be), ...
- [https://opensource.guide/how-to-contribute/]()
- [https://www.youtube.com/user/JuliaLanguage]()

## exercise #0

Follow the directions in [Hello World](https://guides.github.com/activities/hello-world/) using your web-browser and your own laptop -- i.e., **not** the `binder` / `jupyterlab` cloud instance used for the rest of the exercises

## exercise #1

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
```

Next we will try `git commit` ...


## exercise #2

Download, compile, and run MITgcm on one of the unit tests:

```
cd ~
git clone https://github.com/MITgcm/MITgcm
cd MITgcm/verification/
./testreport -t advect_cs
```




# Organizing version-controlled, reproducible, and efficient scientific projects

Here we will run through tools and methods you can use **now** to make your science & computations reproducible in the **future**. The final section (**DIY**) focuses on two representative, real-life examples and tools that were not covered but are worth your consideration.

Exercises and examples in this session focus on [github.com](http://github.com/) and related services. Ahead of our meeting, it is suggested that you create your own github account if you don't already have one. The [github guides](https://guides.github.com) provide very useful examples which we will rely on. Trying out the **Hello World** and **Mastering Markdown** guides would be a great way to get ready for Friday's session.

## Now

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

## Future

- maintainance & user support (issues, PRs, ... email, slack, etc)
	- [Mastering Issues](https://guides.github.com/features/issues/)
	- ...
- dependencies, collabs, modularity, coding practices, ... reuse vs recode, etc
	- [Mastering Forks](https://guides.github.com/activities/forking/)
	- ...

## DYI

- Julia & diagnostic examples
	- docs, tests, apps, ... 
	- `MeshArrays.jl`, `IndividualDisplacements.jl`, Forget & Ferreira 2019
- [MITgcm](http://mitgcm.readthedocs.io/en/latest/?badge=latest) & [ECCO](https://eccov4.readthedocs.io/en/latest/?badge=latest) examples
	- org, repo, doc, blame, issues, PRs, tests, upstream, ...
	- ECCO, archives, doc, rerun, test, notebooks, ...
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


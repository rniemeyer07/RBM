# RBM documentation README

This directory contains files that build the RBM website (http://rbm.readthedocs.org). Below is a bit of information on how to edit, test, and serve the documentation.

## Style
The RBM documentation source files are written in [Markdown](https://help.github.com/articles/markdown-basics/), and configured with a single YAML configuration file (`mkdocs.yml`). Mardown also supports some html so that is an option if pure markdown can't get the job done.

## Requirements

To edit the documentation, all you need is a text editor.

To build the documentation, there are two requirements:
- [Python](https://www.python.org/) version 2.6, 2.7, 3.3 or 3.4.
- [`mkdocs`](http://www.mkdocs.org/) project to build its documentation.

## Building and serving the documentation locally

After editing the RBM documentation, and before committing it to the git repository, you'll want to build and serve the docs on your local machine.

#### build
from the top level of the RBM repository, run:

`mkdocs build`

#### serve
from the top level of the RBM repository, run:

`mkdocs serve`

For more information on how to interact with the built docs, check out the `mkdocs` [documentation](http://www.mkdocs.org/#getting-started).

## Read The Docs

The RBM documentation is served by [Read the Docs](https://readthedocs.org/) at http://vic.readthedocs.org. This allows us to provide multiple versions of the VIC documentation, served simultaneously from the same location.  

Currently, there are three builds (versions) scheduled for the RBM documentation:

1.  [`master`](http://rbm.readthedocs.org/en/master/) - this represents the docs on RBM's `master` branch
1.  [`develop`](http://rbm.readthedocs.org/en/develop/) - this represents the docs on RBM's `develop` branch
1.  `RBM.${tag}` - this represents the docs for individual tags in RBM's history.

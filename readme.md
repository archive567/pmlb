pmlb
====

[![Build
Status](https://travis-ci.org/tonyday567/pmlb.svg)](https://travis-ci.org/tonyday567/pmlb)
[![Hackage](https://img.shields.io/hackage/v/pmlb.svg)](https://hackage.haskell.org/package/pmlb)
[![lts](https://www.stackage.org/package/pmlb/badge/lts)](http://stackage.org/lts/package/pmlb)
[![nightly](https://www.stackage.org/package/pmlb/badge/nightly)](http://stackage.org/nightly/package/pmlb)

The PMBL benchmark suite for machine learning

-   [paper: PMLB: A Large Benchmark Suite for Machine Learning
    Evaluation and Comparison](https://arxiv.org/pdf/1703.00512.pdf)
-   [PMLB repo](https://github.com/EpistasisLab/penn-ml-benchmarks)

To Do:
======

-   quantiles
-   preprocessing
-   normalisation
-   linear regression
-   logistic regression
-   SVM
-   prediction distribution analytics
-   cross-validation schemes
-   grid/parameter search
-   NN

Current State
-------------

random dataset:
[postoperative-patient-data](https://github.com/EpistasisLab/penn-ml-benchmarks/raw/master/datasets/classification/postoperative-patient-data/postoperative-patient-data.tsv.gz)

rectangle check (max 1000 lines): Right (89,9)

first few lines:

    L-CORE	L-SURF	L-O2	L-BP	SURF-STBL	CORE-STBL	BP-STBL	COMFORT	target
    2	1	0	2	0	1	1	3	0
    2	0	0	0	0	1	1	2	2
    0	1	0	0	0	1	0	2	0
    2	1	1	0	0	2	0	3	0

ranges

    column:	L-CORE	L-SURF	L-O2	L-BP	SURF-STBL	CORE-STBL	BP-STBL	COMFORT	target
    min:	0.0	0.0	0.0	0.0	0.0	0.0	0.0	0.0	0.0
    max:	2.0	2.0	1.0	2.0	1.0	2.0	2.0	4.0	2.0

frequency counts for discrete columns

    ("L-CORE",[(2.0,57),(1.0,18),(0.0,13)])
    ("L-SURF",[(2.0,47),(1.0,24),(0.0,17)])
    ("L-O2",[(1.0,45),(0.0,43)])
    ("L-BP",[(2.0,56),(0.0,29),(1.0,3)])
    ("SURF-STBL",[(1.0,44),(0.0,44)])
    ("CORE-STBL",[(1.0,81),(2.0,6),(0.0,1)])
    ("BP-STBL",[(1.0,45),(2.0,22),(0.0,21)])
    ("COMFORT",[(2.0,64),(3.0,19),(4.0,2),(0.0,2),(1.0,1)])
    ("target",[(0.0,64),(2.0,24)])

recipe
------

    stack build --test --exec "$(stack path --local-install-root)/bin/pmlb-app" --file-watch

reference
=========

haskell
-------

-   [ghc
    options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
-   [pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
-   [libraries](https://www.stackage.org/)
-   [protolude](https://www.stackage.org/package/protolude)
-   [optparse-generic](https://www.stackage.org/package/optparse-generic)
-   [hoogle](https://www.stackage.org/package/hoogle)
-   [doctest](https://www.stackage.org/package/doctest)
-   [managed](https://www.stackage.org/package/managed)
-   [streaming](https://www.stackage.org/package/streaming)
-   [streaming-utils](https://www.stackage.org/package/streaming-utils)
-   [streaming-bytestring](https://www.stackage.org/package/streaming-bytestring)

machine learning
----------------

-   [bookkeeeping](https://www.eecs.tufts.edu/~dsculley/papers/ml_test_score.pdf)


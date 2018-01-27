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

uptohere
--------

random dataset:
[irish](https://github.com/EpistasisLab/penn-ml-benchmarks/raw/master/datasets/classification/irish/irish.tsv.gz)

rectangle check (max 1000 lines): Right (501,6)

first few lines:

    Sex	DVRT	Educational_level	Prestige_score	Type_school	target
    1	113.0	3	2	1	0
    1	101.0	7	2	0	0
    1	110.0	10	25	1	1
    1	121.0	5	18	1	0

ranges

    column:	Sex	DVRT	Educational_level	Prestige_score	Type_school	target
    min:	0.0	65.0	0.0	0.0	0.0	0.0
    max:	1.0	140.0	10.0	28.0	2.0	1.0

frequency counts for discrete columns

    ("Sex",[(1.0,250),(0.0,250)])
    ("DVRT",[(70.0,30),(104.0,19),(90.0,18),(109.0,17),(103.0,17)])
    ("Educational_level",[(10.0,158),(6.0,68),(5.0,65),(0.0,57),(4.0,50)])
    ("Prestige_score",[(0.0,91),(8.0,89),(12.0,46),(2.0,39),(28.0,26)])
    ("Type_school",[(1.0,325),(2.0,138),(0.0,37)])
    ("target",[(0.0,278),(1.0,222)])

recipe
------

    stack build --test --exec "$(stack path --local-install-root)/bin/pmlb-app" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/header.md app/readme.md other/footer.md -t html -o index.html --filter pandoc-include --mathjax" --exec "$(stack path --local-bin)/pandoc -f markdown -i app/readme.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch

reference
---------

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


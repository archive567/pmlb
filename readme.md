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
[backache](https://github.com/EpistasisLab/penn-ml-benchmarks/raw/master/datasets/classification/backache/backache.tsv.gz)
process result:

    id	col_2	col_3	col_4	col_5	col_6	col_7	col_8	col_9	col_10	col_11	col_12	col_13	col_14	col_15	col_16	col_17	col_18	col_19	col_20	col_21	col_22	col_23	col_24	col_25	col_26	col_27	col_28	col_29	col_30	col_31	col_32	target
    1.0	1	0	26.0	1.52	54.5	75.0	3.35	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
    2.0	3	0	23.0	1.6	59.1	68.6	2.22	1	2	1	0	0	1	0	0	0	0	0	0	1	0	0	1	0	0	0	0	0	0	0	0	0
    3.0	2	6	24.0	1.57	73.2	82.7	4.15	0	1	0	0	0	1	1	0	1	0	1	0	0	1	0	1	0	0	0	0	0	0	0	0	0
    4.0	1	8	22.0	1.52	41.4	47.3	2.81	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	1	0	0	0	0	0	0	0	0	0

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


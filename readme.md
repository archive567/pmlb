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
[647\_fri\_c1\_250\_10](https://github.com/EpistasisLab/penn-ml-benchmarks/raw/master/datasets/regression/647_fri_c1_250_10/647_fri_c1_250_10.tsv.gz)

rectangle check (max 1000 lines): Right (251,11)

first few lines:

    oz1	oz2	oz3	oz4	oz5	oz6	oz7	oz8	oz9	oz10	target
    1.2628374099731445	1.3677407503128052	0.2509389817714691	1.1943854093551636	-0.42628052830696106	1.0420236587524414	1.4141845703125	1.4196362495422363	0.07142431288957596	0.7270952463150024	0.48022332787513733
    0.4669520854949951	0.1935626119375229	0.302070289850235	-0.6626418828964233	-1.30857515335083	-0.13248036801815033	-1.5331276655197144	1.1797221899032593	0.8096869587898254	-0.2822590172290802	-1.1873418092727661
    0.38612809777259827	0.267796128988266	0.13925504684448242	1.0092955827713013	-0.3920215666294098	-1.420148253440857	1.0535929203033447	0.5269861221313477	0.18503789603710175	0.5123202204704285	-0.4282247722148895
    0.5361552834510803	0.5728064775466919	0.6049923300743103	-1.0282785892486572	0.4622384309768677	-0.10464955866336823	-1.439162254333496	-0.6369666457176208	-1.2045363187789917	0.37346744537353516	-1.6895465850830078

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


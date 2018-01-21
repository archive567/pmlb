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

last dataset used:
[Hill\_Valley\_with\_noise](https://github.com/EpistasisLab/penn-ml-benchmarks/raw/master/datasets/classification/Hill_Valley_with_noise/Hill_Valley_with_noise.tsv.gz)

process result:

    X1	X2	X3	X4	X5	X6	X7	X8	X9	X10	X11	X12	X13	X14	X15	X16	X17	X18	X19	X20	X21	X22	X23	X24	X25	X26	X27	X28	X29	X30	X31	X32	X33	X34	X35	X36	X37	X38	X39	X40	X41	X42	X43	X44	X45	X46	X47	X48	X49	X50	X51	X52	X53	X54	X55	X56	X57	X58	X59	X60	X61	X62	X63	X64	X65	X66	X67	X68	X69	X70	X71	X72	X73	X74	X75	X76	X77	X78	X79	X80	X81	X82	X83	X84	X85	X86	X87	X88	X89	X90	X91	X92	X93	X94	X95	X96	X97	X98	X99	X100	target
    39.02	36.49	38.2	38.85	39.38	39.74	37.02	39.53	38.81	38.79	37.65	39.34	38.55	39.03	37.21	36.32	37.81	38.95	36.7	39.72	37.06	37.29	36.43	36.53	36.19	38.17	37.3	36.15	36.68	36.7	36.68	36.99	38.92	37.25	37.47	36.32	35.75	35.68	34.66	34.26	35.62	36.6	34.78	34.67	34.3	33.4	31.4	31.75	31.75	32.84	33.76	35.74	34.01	33.91	36.88	34.41	35.52	36.94	36.95	35.57	38.02	37.32	39.05	37.97	37.01	38.98	38.83	38.87	38.03	38.4	38.25	38.61	36.23	37.81	37.98	38.58	38.96	38.97	39.08	38.79	38.79	36.31	36.59	38.19	37.95	39.63	39.27	37.19	37.13	37.47	37.57	36.62	36.92	38.8	38.52	38.07	36.73	39.46	37.5	39.1	0

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


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
[texture](https://github.com/EpistasisLab/penn-ml-benchmarks/raw/master/datasets/classification/texture/texture.tsv.gz)

rectangle check (max 1000 lines): Right (1000,41)

first few lines:

    A1	A2	A3	A4	A5	A6	A7	A8	A9	A10	A11	A12	A13	A14	A15	A16	A17	A18	A19	A20	A21	A22	A23	A24	A25	A26	A27	A28	A29	A30	A31	A32	A33	A34	A35	A36	A37	A38	A39	A40	target
    -1.223	-0.7979999999999999	-0.867	-0.639	-0.545	-0.412	-0.795	-0.629	-0.547	-0.868	-1.223	-0.879	-0.747	-0.818	-0.6709999999999999	-0.718	-0.991	-0.778	-0.759	-0.892	-1.223	-0.899	-0.987	-0.75	-0.6990000000000001	-0.5660000000000001	-0.911	-0.7859999999999999	-0.7	-1.015	-1.223	-0.7659999999999999	-0.555	-0.7140000000000001	-0.545	-0.5870000000000001	-0.871	-0.62	-0.568	-0.607	2
    -1.41	-1.029	-1.013	-0.895	-0.762	-0.6759999999999999	-1.043	-0.851	-0.775	-1.037	-1.41	-1.083	-0.894	-1.015	-0.825	-0.8290000000000001	-1.172	-0.941	-0.8909999999999999	-1.01	-1.41	-1.081	-1.094	-0.932	-0.8079999999999999	-0.6809999999999999	-1.071	-0.899	-0.812	-1.074	-1.41	-0.919	-0.77	-0.847	-0.6629999999999999	-0.723	-1.013	-0.748	-0.698	-0.8170000000000001	2
    -1.107	-0.649	-0.629	-0.4920000000000001	-0.3670000000000001	-0.298	-0.682	-0.478	-0.395	-0.6809999999999999	-1.107	-0.718	-0.5329999999999999	-0.6709999999999999	-0.496	-0.5329999999999999	-0.8490000000000001	-0.603	-0.575	-0.6729999999999999	-1.107	-0.8390000000000001	-0.853	-0.7	-0.596	-0.443	-0.8290000000000001	-0.68	-0.595	-0.87	-1.107	-0.6920000000000001	-0.445	-0.588	-0.371	-0.368	-0.746	-0.457	-0.379	-0.469	2
    -1.27	-0.855	-0.958	-0.7070000000000001	-0.619	-0.469	-0.872	-0.705	-0.62	-0.988	-1.27	-0.909	-0.83	-0.853	-0.716	-0.75	-1.018	-0.8059999999999999	-0.778	-0.92	-1.27	-1.019	-1.035	-0.917	-0.813	-0.721	-1.027	-0.877	-0.8220000000000001	-1.039	-1.27	-0.8290000000000001	-0.7190000000000001	-0.774	-0.617	-0.688	-0.937	-0.693	-0.657	-0.779	2

ranges

    column:	A1	A2	A3	A4	A5	A6	A7	A8	A9	A10	A11	A12	A13	A14	A15	A16	A17	A18	A19	A20	A21	A22	A23	A24	A25	A26	A27	A28	A29	A30	A31	A32	A33	A34	A35	A36	A37	A38	A39	A40	target
    min:	-1.422	-1.096	-1.064	-0.976	-0.852	-0.7509999999999999	-1.113	-0.938	-0.8690000000000001	-1.107	-1.422	-1.083	-0.973	-1.015	-0.89	-0.897	-1.172	-0.998	-0.972	-1.112	-1.422	-1.2009999999999998	-1.168	-1.123	-1.026	-0.952	-1.194	-1.072	-1.017	-1.175	-1.422	-1.045	-0.908	-0.977	-0.82	-0.826	-1.131	-0.915	-0.841	-0.957	2.0
    max:	-0.532	-0.237	-0.329	-9.0e-2	-9.000000000000001e-3	8.7e-2	-0.18600000000000005	-0.13699999999999998	-5.0e-2	-0.34700000000000003	-0.532	-0.325	-8.3e-2	-0.275	-0.17800000000000002	-0.307	-0.436	-0.192	-0.217	-0.138	-0.532	-0.338	-0.601	-0.278	-0.343	-0.251	-0.393	-0.384	-0.333	-0.58	-0.532	-0.248	-0.242	-0.29600000000000004	-0.251	-0.302	-0.504	-0.343	-0.318	-0.325	3.0

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


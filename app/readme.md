pmlb
===

[![Build Status](https://travis-ci.org/tonyday567/pmlb.svg)](https://travis-ci.org/tonyday567/pmlb) [![Hackage](https://img.shields.io/hackage/v/pmlb.svg)](https://hackage.haskell.org/package/pmlb) [![lts](https://www.stackage.org/package/pmlb/badge/lts)](http://stackage.org/lts/package/pmlb) [![nightly](https://www.stackage.org/package/pmlb/badge/nightly)](http://stackage.org/nightly/package/pmlb) 

The PMBL benchmark suite for machine learning

- [paper: PMLB: A Large Benchmark Suite for Machine Learning
Evaluation and Comparison](https://arxiv.org/pdf/1703.00512.pdf)
- [PMLB repo](https://github.com/EpistasisLab/penn-ml-benchmarks)


To Do:
===

- quantiles
- preprocessing
- normalisation
- linear regression
- logistic regression
- SVM
- prediction distribution analytics
- cross-validation schemes
- grid/parameter search
- NN

Current State
---

```include
other/uptohere.md
```

recipe
---

```
stack build --test --exec "$(stack path --local-install-root)/bin/pmlb-app" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/header.md app/readme.md other/footer.md -t html -o index.html --filter pandoc-include --mathjax" --exec "$(stack path --local-bin)/pandoc -f markdown -i app/readme.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch
```

reference
===

haskell
---

- [ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
- [pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
- [libraries](https://www.stackage.org/)
- [protolude](https://www.stackage.org/package/protolude)
- [optparse-generic](https://www.stackage.org/package/optparse-generic)
- [hoogle](https://www.stackage.org/package/hoogle)
- [doctest](https://www.stackage.org/package/doctest)
- [managed](https://www.stackage.org/package/managed) 
- [streaming](https://www.stackage.org/package/streaming) 
- [streaming-utils](https://www.stackage.org/package/streaming-utils) 
- [streaming-bytestring](https://www.stackage.org/package/streaming-bytestring) 

machine learning
---

- [bookkeeeping](https://www.eecs.tufts.edu/~dsculley/papers/ml_test_score.pdf)


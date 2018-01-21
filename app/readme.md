pmlb
===

[![Build Status](https://travis-ci.org/tonyday567/pmlb.svg)](https://travis-ci.org/tonyday567/pmlb) [![Hackage](https://img.shields.io/hackage/v/pmlb.svg)](https://hackage.haskell.org/package/pmlb) [![lts](https://www.stackage.org/package/pmlb/badge/lts)](http://stackage.org/lts/package/pmlb) [![nightly](https://www.stackage.org/package/pmlb/badge/nightly)](http://stackage.org/nightly/package/pmlb) 

The PMBL benchmark suite for machine learning

- [paper](https://arxiv.org/pdf/1703.00512.pdf)
- [github repo](https://github.com/EpistasisLab/penn-ml-benchmarks)




uptohere
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
---

- [ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
- [pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
- [libraries](https://www.stackage.org/)
- [protolude](https://www.stackage.org/package/protolude)
- [optparse-generic](https://www.stackage.org/package/optparse-generic)
- [hoogle](https://www.stackage.org/package/hoogle)
- [doctest](https://www.stackage.org/package/doctest)

# sdewac-prepro

*2015-02-06*

This module implements some preprocessing for the
[SdeWaC](http://www.ims.uni-stuttgart.de/forschung/ressourcen/korpora/sdewac.en.html)
corpus.

## Installation

Install the `conll-tools` package by following [these
steps](https://github.com/jsnajder/conll-corpus).

Get the source:

```
$ git clone http://github.com/jsnajder/sdewac-prepro sdewac-prepro
```

Then build from the source:

```
$ cd sdewac-prepro
$ cabal sandbox init
$ cabal sandbox add-source ../counts
$ cabal sandbox add-source ../conll-corpus
$ cabal install --only-dependencies
$ cabal configure
$ cabal install --bindir=bin
```

## Usage example

```
$ cd data
$ ../bin/unlematized.sh sdewac-mst.sample.conll > sdewac-mst.sample.unlemmatized
```

```
../bin/conll2lemmadict -t sdewac-mst.sample.unlemmatized sdewac-mate.sample.conll > sdewac-mate.sample.lemmadict
```

```
../bin/sdewac-prepro sdewac-mst.sample.lemmas sdewac-mate.sample.lemmadict sdewac-mst.sample.conll
```


(1) Generate a list of unlemmatized wordforms (with POSes)

> ./unlematized.sh sdewac-mstparsed.sample.conll > sdewac-mstparsed.sample.unlemmatized

  * generate a list of wordforms that have not been lemmatized (for which the lemma is `<unknown>`) in mst-parsed sdewac
  * add counts from sdewac-mst to these worforms
  * filter the list based on frequency (discard wordform occurring less than a certain number of times times)

* Step 2:
  * generate a lemmatization dictionary (essentially a wordform_POS ⇒ lemma_POS mapping) from mate-parsed sdewac for unknown wordforms from Step 1
  * `$ conll2lemmadict sdewac-mate.conll sdewac-mst-unknown-wordforms.txt > sdewac-mate-lemmadict.txt`

* Step 3:
  * generate a frequency list of lemma_POS from mst-parsed sdewac
  * `$ conll2counts sdewac-mst.conll > sdewac-mst-lemmacounts.txt`

* Step 4:
  * for each mst-parsed sdewac sentence:
    (4a) prefix PTKVZ if resulting prefix+verb exists in sdewac-mst-lemmacounts.txt
    (4b) convert hyphenated lemmas to non-hyphenated variants, if such exist in sdewac-mst-lemmacounts.txt
    (4c) if lemmais unknown, attempt backoff using sdewac-mate-lemmadict
  * `$ sdewac-prepro sdewac-mst.conll sdewac-mst-lemmacounts.txt sdewac-mate-lemmadict.txt > sdewac-preprocessed.conll`


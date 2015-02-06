# sdewac-prepro

2015-02-04

This module implements [SdeWaC](http://www.ims.uni-stuttgart.de/forschung/ressourcen/korpora/sdewac.en.html) preprocessing.

## Installation

Install the [Haskell platform](https://www.haskell.org/platform/) and upgrade [Cabal](https://www.haskell.org/cabal/download.html) to the newest stable version.

> git clone https://github.com/jsnajder/sdewac-preprocessing


## Preprocessing steps


> ./unlematized.sh sdewac-mstparsed.sample.conll > sdewac-mstparsed.sample.unlemmatized

> ../bin/conll2lemmadict -t sdewac-mstparsed.sample.unlemmatized sdewac-mateparsed.sample.conll > sdewac-mate.sample.lemmadict





(1) Generale a list of unlemmatized wordforms (with POSes)

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


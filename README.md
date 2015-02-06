# sdewac-prepro

*2015-02-06*

This package implements some preprocessing for the
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

First, let's fix CoNLL format of the MATE-parsed sDeWaC:

```
$ cd data
$ ../bin/fix-mate-conll.sh sdewac-mate.sample.conll-bogus > sdewac-mate.sample.conll
```

Get a `wordform_POS` list of unlemmatized words from MST-parsed sDeWac:

```
$ ../bin/unlemmatized.sh sdewac-mst.sample.conll > sdewac-mst.sample.unlemmatized
```

Generate a lemmatization dictionary (a `wordform_POS` => `lemma_POS` mapping)
from MATE-parsed sDeWac:

```
../bin/conll2lemmadict -t sdewac-mst.sample.unlemmatized sdewac-mate.sample.conll > sdewac-mate.sample.lemmadict
```

The above can be run as via Hadoop Map/Reduce streaming, by using `-m` and `-r`
flags for the mapper and reducer, respectively. The following simulates
Map/Reduce streaming:

```
../bin/conll2lemmadict -m -t sdewac-mst.sample.unlemmatized < sdewac-mate.sample.conll | sort | ../bin/conll2lemmadict -r > sdewac-mate.sample.lemmadict
```

Finally, we run the preprocessing of the MST-parsed sDeWac corpus, providing a
list of lemmas and the lemmatization dictionary as input:

```
../bin/sdewac-prepro sdewac-mst.sample.lemmas sdewac-mate.sample.lemmadict sdewac-mst.sample.conll
```

The preprocessing does three things:

* prefixes PTKVZ (*abgetrennter Verbzusatz*) to a verb lemma, provided the
  resulting prefix+verb exists in lemma list

* converts a hyphenated lemma to its non-hyphenated version, provided the
  non-hyphenated version exist in the lemma list

* if the lemmatization failed (the lemma is `<unknown>`), attempts backoff
  lemmatization using the lemmatization dictionary

The output is a vertical list of `(lemma, POS, flag)` triplets, aligned with
the input corpus. The flag indicates what change has been made to the lemma, if
any:

* `P` -- PTKVZ prefixation
* `H` -- dehyphenation
* `B` -- lemma backoff


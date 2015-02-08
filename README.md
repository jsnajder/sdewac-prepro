# sdewac-prepro

*2015-02-08*

This package implements some preprocessing for the
[SdeWaC](http://www.ims.uni-stuttgart.de/forschung/ressourcen/korpora/sdewac.en.html)
corpus.

## Installation

Install the [`conll-tools`](https://github.com/jsnajder/conll-corpus).
package by following [these
steps](https://github.com/jsnajder/conll-corpus#installation).

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

First, let's fix CoNLL format of the MATE-parsed SdeWaC:

```
$ cd data
$ ../bin/fix-mate-conll.sh sdewac-mate.sample.conll-bogus > sdewac-mate.sample.conll
```

Next, use `conll2counts` from the `conll-corpus` package to generate a
list of lemmas and their frequencies from MST-parsed SdeWaC:

```
$ ../bin/conll2counts -p sdewac-mst.sample.conll > sdewac-mst.sample.lemmas
```

Note that the above can be run via Hadoop Map/Reduce streaming; check out
`conll-corpus` documentation for an example.

Next, get a word list of unlemmatized words from MST-parsed SdeWaC (only
wordforms without POS-es):

```
$ ../bin/unlemmatized.sh sdewac-mst.sample.conll > sdewac-mst.sample.unlemmatized
```

Next, generate a lemmatization dictionary (a `wordform_POS` => `lemma_POS`
mapping) for the unlemmatized words by running `conll2lemmadict` from the
`conll-corpus` package on the MATE-parsed SdeWaC corpus:

```
$ ../bin/conll2lemmadict -t sdewac-mst.sample.unlemmatized sdewac-mate.sample.conll > sdewac-mate.sample.lemmadict
```

Again, note that the above can be run via Hadoop Map/Reduce streaming;
check out `conll-corpus` documentation for an example.

Finally, run the preprocessing of the MST-parsed SdeWaC corpus, providing
a list of lemmas and the lemmatization dictionary as input:

```
$ ../bin/sdewac-prepro sdewac-mst.sample.lemmas sdewac-mate.sample.lemmadict sdewac-mst.sample.conll > prepro
$ paste sdewac-mst.sample.conll prepro > sdewac-mst.sample.prepro
```

The preprocessing does three things:

* prefixes PTKVZ (*abgetrennter Verbzusatz*) to a verb lemma, provided the
  resulting prefix+verb exists in the lemma list;

* converts a hyphenated lemma to its non-hyphenated version, provided the
  non-hyphenated version occurs more frequent in the lemma list. If
  POS is "T"`, which is typical for suspended hyphenation, an attempt is made 
  to replace the POS with the most frequent POS for this lemma from the lemma 
  list;

* if the lemmatization failed (the lemma is `<unknown>`), attempts backoff
  lemmatization using the lemmatization dictionary. The dictionary is queried
  for `wordform_POS` first, then the same wordform is tried with other POS-es,
  and finally uppercased wordform is tried with the original POS.

The output is a vertical list of `(lemma, POS, flag)` triplets, aligned with
the input corpus. The flag indicates what change has been made to the lemma,
if any:

* `P` -- PTKVZ prefixation
* `H` -- dehyphenation
* `Hp` -- dehyphenation + POS change
* `B` -- lemma backoff
* `Bp` -- lemma backoff + POS change
* `Bc` -- lemma backoff + case change
* `X` -- no change


# sdewac-prepro

2015-02-04

This module implements SdeWaC preprocessing.

The SdeWaC preprocessing steps are as follows.

* Step 1: 
	* generate a list of wordforms that have not been lemmatized (for which the lemma is "<unknown>") in mst-parsed sdewac
	* add counts from sdewac-mst to these worforms
	* filter the list based on frequency (discard wordform occurring less than f_1 times)
* Step 2:
	* generate a lemmatization dictionary (essentially a wordform_POS ⇒ lemma_POS mapping) from mate-parsed sdewac for unknown wordforms from Step 1
	* $ conll2lemmadict sdewac-mate.conll sdewac-mst-unknown-wordforms.txt > sdewac-mate-lemmadict.txt
* Step 3:
	* generate a frequency list of lemma_POS from mst-parsed sdewac
	* $ conll2counts sdewac-mst.conll > sdewac-mst-lemmacounts.txt
* Step 4:
	* for each mst-parsed sdewac sentence:
		    (4a) prefix PTKVZ if resulting prefix+verb exists in sdewac-mst-lemmacounts.txt
		    (4b) convert hyphenated lemmas to non-hyphenated variants, if such exist in sdewac-mst-lemmacounts.txt
		    (4c) if lemmais unknown, attempt backoff using sdewac-mate-lemmadict
	* $ sdewac-prepro sdewac-mst.conll sdewac-mst-lemmacounts.txt sdewac-mate-lemmadict.txt > sdewac-preprocessed.conll


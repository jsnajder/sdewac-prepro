#!/bin/sh

awk 'BEGIN{OFS = "\t"}{print $1,$2,$4,substr($6,0,1),$6,$8,$10,$12}' $1

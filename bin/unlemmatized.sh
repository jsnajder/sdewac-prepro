#!/bin/bash

grep '<unknown>' $1 | awk '{print $2"_"$4}'|sort|uniq -c|awk '{print $2"\t"$1}'

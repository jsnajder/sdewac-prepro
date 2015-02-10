#!/bin/bash

tr '\t' ' ' < $1 | grep -E '^(-?"?[A-ZÜÄÖa-züöäß]+"?-?)+ ' | tr ' ' '\t'

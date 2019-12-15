#!/usr/bin/env bash
pdflatex expo.tex
biber expo
pdflatex expo.tex
pdflatex expo.tex
./DeleteJunk.sh

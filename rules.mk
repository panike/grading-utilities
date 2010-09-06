TEX:=$(shell which tex)
DVIPS:=$(shell which dvips) -q
MPOST:=$(shell which mpost)
PDFTEX:=$(shell which pdftex)
DVIPDFM:=$(shell which dvipdfm)
DVITOTXT:=$(shell which dvitotxt)
TXTTODVI:=$(shell which txttodvi)
DVITOSLIDES:=$(shell which dvitoslides)
NEWSLETTER:=$(shell which newsletter)
SBCL:=$(shell which sbcl)
BASH:=$(shell which bash)
RM:=$(shell which rm) -f
CC:=$(shell which gcc) -Wall -g
CWEAVE:=$(shell which cweave)
CTANGLE:=$(shell which ctangle)
AR:=$(shell which ar)
GS:=$(shell which gs) -q
MV:=$(shell which mv)
PS2PDF:=$(GS) -dSAFER -dNOPAUSE -dBATCH -sDEVICE=pdfwrite

SHELL:=$(shell which bash)
export SHELL

SOURCES:=Makefile $(shell /bin/ls *.tex *.mp)

%.dvi: %.tex
	$(TEX) $*

%.ps: %.dvi
	$(DVIPS) $* -o

%.pdf: %.ps
	$(PS2PDF) -sOutputFile=$@ $^

%.1: %.mp
	$(MPOST) $*

%.tex: %.w
	$(CWEAVE) $*

%.c: %.w
	$(CTANGLE) $*

%.o: %.c
	$(CC) -o $@ -c $<

#clean:
#	/bin/rm -f $(filter-out $(SOURCES),$(shell /bin/ls))

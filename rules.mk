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
CWEAVE:=$(shell which cweave) -bhp
CTANGLE:=$(shell which ctangle) -bhp
AR:=$(shell which ar)
GS:=$(shell which gs) -q
MV:=$(shell which mv)
PS2PDF:=$(GS) -dSAFER -dNOPAUSE -dBATCH -sDEVICE=pdfwrite

SHELL:=$(shell which bash)
export SHELL

SOURCES:=Makefile $(shell /bin/ls *.tex *.mp 2>/dev/null)

TEXHALT:=$(shell tex --help | grep -q -e -halt-on-error && echo "-halt-on-error")

ifndef V
	QUIET_CC= @echo "CC $@";
	QUIET_TEX = @echo "TEX $*";
	QUIET_DVIPS = @echo "DVIPS $*";
	QUIET_MPOST = @echo "MPOST $*";
	QUIET_PDFTEX = @echo "PDFTEX $*";
	QUIET_DVIPDFM = @echo "DVIPDFM $*";
	QUIET_DVITOSLIDES = @echo "DVITOSLIDES $<";
	QUIET_GS = @echo "GS $*";
	QUIET_CWEAVE = @echo "CWEAVE $*";
	QUIET_CTANGLE = @echo "CTANGLE $*";
	QUIET_PS2PDF = @echo "PS2PDF $(basename $(notdir $@))";
	REPORT_TEX_ERROR=> /dev/null || { cat $*.log; exit 1; };
	REPORT_MPOST_ERROR=> /dev/null || { cat $*.log; exit 1; };
	REPORT_PDFTEX_ERROR=> /dev/null || { cat $*.log; exit 1; };
	QUIET_CWEAVE_OUTPUT=> /dev/null
	QUIET_CTANGLE_OUTPUT=> /dev/null
	TEXFLAGS=-interaction=batchmode $(TEXHALT)
	PDFTEXFLAGS=-interaction=batchmode $(TEXHALT)
	MPFLAGS=-interaction=batchmode $(TEXHALT)
endif

%.dvi: %.tex
	$(QUIET_TEX)$(TEX) $(TEXFLAGS) $* $(REPORT_TEX_ERROR)

%.ps: %.dvi
	$(QUIET_DVIPS)$(DVIPS) $* -o

%.pdf: %.ps
	$(QUIET_PS2PDF)$(PS2PDF) -sOutputFile=$@ $<

%.1: %.mp
	$(QUIET_MPOST)$(MPOST) $(MPFLAGS) $* $(REPORT_MPOST_ERROR)

%.tex: %.w
	$(QUIET_CWEAVE)$(CWEAVE) $* $(QUIET_CWEAVE_OUTPUT)

%.c: %.w
	$(QUIET_CTANGLE)$(CTANGLE) $* $(QUIET_CTANGLE_OUTPUT)

%.o: %.c
	$(QUIET_CC)$(CC) -o $@ -c $<

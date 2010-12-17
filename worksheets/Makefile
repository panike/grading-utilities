TEX:=$(shell which tex) 
DVIPS:=$(shell which dvips) -q
MPOST:=$(shell which mpost)
PDFTEX:=$(shell which pdftex)
DVIPDFM:=$(shell which dvipdfm)
BASH:=$(shell which bash)
RM:=$(shell which rm) -f
GS:=$(shell which gs)
PS2PDF:=$(GS) -q -dSAFER -dBATCH -dNOPAUSE -sDEVICE=pdfwrite

TEXHALT:=$(shell tex --help | grep -q -e -halt-on-error && \
	echo "-halt-on-error")

ifndef V
	QUIET_TEX = @echo "TEX $*";
	QUIET_DVIPS = @echo "DVIPS $*";
	QUIET_MPOST = @echo "MPOST $*";
	QUIET_PDFTEX = @echo "PDFTEX $*";
	QUIET_DVIPDFM = @echo "DVIPDFM $*";
	QUIET_GS = @echo "GS $*";
	QUIET_PS2PDF = @echo "PS2PDF $(basename $(notdir $@))";
	REPORT_TEX_ERROR=> /dev/null || { cat $*.log; exit 1; };
	REPORT_MPOST_ERROR=> /dev/null || { cat $*.log; exit 1; };
	REPORT_PDFTEX_ERROR=> /dev/null || { cat $*.log; exit 1; };
	TEXFLAGS=-interaction=batchmode $(TEXHALT)
	PDFTEXFLAGS=-interaction=batchmode $(TEXHALT)
	MPFLAGS=-interaction=batchmode $(TEXHALT)
endif

SHELL:=$(shell which bash)
export SHELL

define mp_template
$(patsubst %.mp,%.dvi,$(1)): $(patsubst %.mp,%.1,$(1))
endef
MPS:=$(wildcard panike-222-f10-*.mp)
$(foreach mp,$(MPS),$(eval $(call mp_template,$(mp))))

SOURCES:=$(filter-out coversheet.tex,Makefile $(shell /bin/ls *.tex *.mp \
	2>/dev/null)) README

$(patsubst %.tex,%.dvi,$(shell ls panike-222-f10-*.tex)): worksheetmac.tex
.INTERMEDIATE: $(patsubst %.tex,%.dvi,$(shell ls panike-222-f10-*.tex)) \
	$(patsubst %.tex,%.log,$(shell ls panike-222-f10-*.tex)) \
	$(patsubst %.tex,%.pdf,$(shell ls panike-222-f10-*.tex)) \
	coversheet.log $(patsubst %.mp,%.mpx,$(shell ls panike-222-f10-*.mp)) \
	coversheet.ps coversheet.tex
	
ifndef KEEPMP
.INTERMEDIATE: $(patsubst %.mp,%.1,$(shell ls panike-222-f10-*.mp)) \
	$(patsubst %.mp,%.2,$(shell ls panike-222-f10-*.mp)) \
	$(patsubst %.mp,%.3,$(shell ls panike-222-f10-*.mp)) \
	$(patsubst %.mp,%.4,$(shell ls panike-222-f10-*.mp))
endif

allps:
	@make $(patsubst %.tex,%.ps,$(wildcard panike-222-f10-*.tex))
	
allpdf:
	@make $(patsubst %.tex,%.pdf,$(wildcard panike-222-f10-*.tex))

%.log %.dvi: %.tex
	$(QUIET_TEX)$(TEX) $(TEXFLAGS) $* $(REPORT_TEX_ERROR)

%.ps: %.dvi
	$(QUIET_DVIPS)$(DVIPS) $* -o

%.pdf: %.ps
	$(QUIET_PS2PDF)$(PS2PDF) -sOutputFile=$@ $<

%.2 %.3 %.4 %.mpx %.log %.1: %.mp
	$(QUIET_MPOST)$(MPOST) $(MPFLAGS) $* $(REPORT_MPOST_ERROR)

target-prereqs: $(SOURCES)
	@for i in panike-222-f10-*tex; do \
	echo "$${i/tex/pdf}:$$(make -n --debug=v $${i/tex/pdf} | \
	sed -n '/No need to remake target/{s/^.\+\x60\(.\+\)\x27.\+$$/\1/;p}' \
	| sed -n '$$ {H;g;s/\n/ /g;p;};H;')"; \
	done > $@

coversheet.tex:
	@(echo '\topskip0pt plus1fil'; echo '\footline{\hfil}'; \
	echo '\hbox to\hsize{\hfil\bf Problems for WES MATH 222 Fall 2010\hfil}'; \
	echo -n '\hbox to\hsize{\hfil Created or selected by Nathan W. Panike '; \
	echo 'and Nicos Georgiou\hfil}';\
	echo "\hbox to\hsize{\hfil Document created by $$(getent passwd $$UID | \
		awk -F: '{print $$5}' | sed 's/,.\+//g') on \
		$$(date +"%e %B %Y")\hfil}"; \
	 echo '\vfil\eject\end' ) > $@

worksheets-f10.pdf: coversheet.ps \
	$(patsubst %.tex,%.pdf,$(shell ls panike-222-f10-*.tex))
	$(QUIET_PS2PDF)$(PS2PDF) -sOutputFile=$@ $^

clean:
	$(RM) $(filter-out $(SOURCES),$(shell /bin/ls))

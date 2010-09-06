TEX:=$(shell which tex)
DVIPS:=$(shell which dvips) -q
MPOST:=$(shell which mpost)
PDFTEX:=$(shell which pdftex)
DVIPDFM:=$(shell which dvipdfm)
BASH:=$(shell which bash)
RM:=$(shell which rm) -f
GS:=$(shell which gs)
PS2PDF:=$(GS) -q -dSAFER -dBATCH -dNOPAUSE -sDEVICE=pdfwrite

SHELL:=$(shell which bash)
export SHELL

SOURCES:=$(filter-out coversheet.tex,Makefile $(shell /bin/ls *.tex *.mp))

panike-222-s10-15.dvi: panike-222-s10-15.1
panike-222-s10-17.dvi: panike-222-s10-17.1
panike-222-s10-26.dvi: panike-222-s10-26.1
panike-222-s10-28.dvi: panike-222-s10-28.1
panike-222-s10-32.dvi: panike-222-s10-32.1

%.dvi: %.tex
	$(TEX) $*

%.ps: %.dvi
	$(DVIPS) $* -o

%.pdf: %.ps
	$(PS2PDF) -sOutputFile=$@ $<

%.1: %.mp
	$(MPOST) $*

target-prereqs: $(SOURCES)
	for i in panike-222-s10-*tex; do \
	echo "$${i/tex/pdf}:$$(make -n --debug=v $${i/tex/pdf} | \
	sed -n 's/^\s\+No need to remake target .\(.\+\)../\1/p' | \
	sed -n '$$ {H;g;s/\n/ /g;p;};H;')"; \
	done > $@
$(patsubst %.tex,%.dvi,$(shell ls panike-222-s10-*.tex)): worksheetmac.tex

coversheet.tex:
	(echo '\topskip0pt plus1fil'; echo '\footline{\hfil}'; \
	 echo '\hbox to\hsize{\hfil\bf Problems for WES MATH 222 Spring 2010\hfil}'; \
	 echo '\hbox to\hsize{\hfil Created or selected by Nathan W. Panike\hfil}';\
	 echo "\hbox to\hsize{\hfil Document created by $$(getent passwd $$UID | awk -F: '{print $$5}' | sed 's/,.\+//g') on $$(date +"%e %B %Y")\hfil}"; \
	 echo '\vfil\eject\end' ) > $@

worksheets-s10.pdf: coversheet.ps \
	$(patsubst %.tex,%.ps,$(shell ls panike-222-s10-*.tex))
	$(PS2PDF) -sOutputFile=$@ $^

clean:
	$(RM) $(filter-out $(SOURCES),$(shell /bin/ls))

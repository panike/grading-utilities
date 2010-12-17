include ../rules.mk

A2PS=$(shell which a2ps)
MV=$(shell which mv)
SOURCES:=$(shell ls *.tex *.mp *.gpl 2>/dev/null) Makefile \
	tfm_files generate-gpl.lisp

$(patsubst %.tex,%.dvi,$(wildcard outline-*.tex)): outlinemac.tex

$(patsubst %.tex,%.ps,$(wildcard outline-*.tex)): %.ps: %.dvi
	$(QUIET_DVIPS)$(DVIPS) $* -o
	$(MV) -f $@ temp.ps
	$(A2PS) -2 --borders=no	-o $@ temp.ps
	$(RM) temp.ps

$(patsubst %.tex,%.pdf,$(wildcard outline-*.tex)): %.pdf: %.ps
	$(PS2PDF) -sOutputFile=$@ $<

lecture-20101120.dvi: lecture-20101120.1

%.ps: %.dvi
	$(QUIET_DVITOSLIDES)$(DVITOSLIDES) -i $< -t ./tfm_files -o $<
	$(QUIET_DVIPS)$(DVIPS) -t landscape $* -o

lecture-20101013.pdf lecture-20100908.pdf: %.pdf: %.dvi
	$(DVITOSLIDES) -i $< -t ./tfm_files -o $<
	$(DVIPDFM) -l $*

clean:
	$(RM) $(filter-out $(SOURCES),$(shell /bin/ls))

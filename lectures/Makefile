include ../rules.mk

SOURCES:=$(shell ls *.tex *.mp *.gpl) Makefile tfm_files generate-gpl.lisp

PDFS=lecture-20100120.pdf lecture-20100226.pdf

lecture-20100416.dvi: lecture-20100416.1

$(PDFS): %.pdf: %.ps
	$(GS) -dBATCH -dNOPAUSE -dSAFER -sDEVICE=pdfwrite -sOutputFile=$@ $^

%.ps: %.dvi
	$(DVITOSLIDES) -i $< -t ./tfm_files -o $<
	$(DVIPS) -t landscape $* -o

# lecture-20100120.pdf: %.pdf: %.dvi
# 	$(DVITOSLIDES) -i $< -t ./tfm_files -o $<
# 	$(DVIPDFM) -l $*

clean:
	$(RM) $(filter-out $(SOURCES),$(shell /bin/ls))

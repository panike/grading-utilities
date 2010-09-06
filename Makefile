include ./rules.mk

SOURCES:=gradesmac.tex grading.lisp Makefile worksheets \
	quizzes rules.mk server useful_functions lectures \
	useful.vim create_curve_table.c stats.lisp 355.lisp

clean:
	/bin/rm -rf $(filter-out $(SOURCES),$(shell /bin/ls))
	for i in server worksheets lectures quizzes; \
	do \
		make -C $$i clean; \
	done

GRADES:=grades-355.ps

GRADES_DVI:=$(subst ps,dvi,$(GRADES))
GRADES_TEX:=$(subst ps,tex,$(GRADES))

.INTERMEDIATE: $(GRADES_DVI) $(GRADES_TEX)

grades-%.tex: %.lisp
	. ./useful_functions; create_grades_file $< 80pt > $@

$(GRADES_DVI): %.dvi: %.tex gradesmac.tex
	$(TEX) $*
	rm $*.log

$(GRADES): %.ps: %.dvi
	$(DVIPS) -t landscape $* -o

all: $(GRADES)

355.tex: %.tex: %.lisp
	$(SBCL) --noinform \
		--load grading.lisp --load $< \
		--eval "(setf *use-newsletter* nil)" \
		--eval "(output-split-condensed-tex *$*-ARR* nil)" --eval "(sb-ext:quit)" > $@

355-sorted.tex: %-sorted.tex: %.lisp
	$(SBCL) --noinform \
		--load grading.lisp --load $< \
		--eval "(setf *use-newsletter* nil)" \
		--eval "(output-split-condensed-tex *$*-ARR*)" \
		--eval "(sb-ext:quit)" > $@

355.tex 355-sorted.tex: grading.lisp

355-sorted.ps 355.ps: %.ps: %.dvi
	. ./useful_functions; create_report $<
	$(DVIPS) -t landscape $* -o

355-sorted.pdf 355.pdf: %.pdf: %.dvi
	$(DVIPDFM) -l $*

config-file:
	. ./useful_functions; create_config_file 3*.lisp > ./server/s10config

report: 355.ps

grade-reports:
	mkdir grade-reporting; \
	cd grade-reporting; \
	$(SBCL) --noinform \
	--load ../grading.lisp --load ../355.lisp \
	--eval '(dump-for-student-notification *355-ARR*)' \
	--eval '(sb-ext:quit)'

SCORES=scores
height=200
width=100
max=100

curve.dvi: curve.0

%.0: %.mp
	$(MPOST) $*

curve.mp: $(SCORES) FORCE
	. ./useful_functions; create_curve $< $(height) $(width) $(max) > $@

FORCE:

orientit=portrait

curve.ps: %.ps: %.dvi
	if test "$(orientit)" == "landscape"; then \
		$(DVIPS) -t $(orientit) $* -o; \
	else $(DVIPS) $* -o; fi

cols=3

dumpit.lisp: $(SCORES)
	. ./useful_functions; create_stats < $< > $@

curve.tex: $(SCORES) create_curve_table dumpit.lisp
	. ./useful_functions; (echo "\input epsf"; \
	if test "$(orientit)" == "landscape"; then \
	echo '\hsize9in \vsize 6.5in'; fi; \
	echo '$$$$\epsfbox{curve.0}$$$$'; \
	output_txt_curve < $(SCORES) | \
	./create_curve_table -cols $(cols); \
	$(SBCL) --noinform --load ./stats.lisp \
		--load ./dumpit.lisp --eval "(sb-ext:quit)"; \
	echo '\bye' ) > $@ \

for-wes-archive:
	@make -s -C worksheets target-prereqs;
	@git diff --quiet worksheets/ && git diff --quiet --cached worksheets/ || { echo "Need to commit before sending to the WES archive"; exit 1; };
	@. ./useful_functions; \
	if test "$$(git for-each-ref refs/heads/last-archive)" != ""; \
	then \
		if test "$$(git rev-list HEAD...last-archive | wc -l)" -ne "0"; \
		then \
			if test "$$(get_archive_files | wc -l)" -eq "0"; \
			then \
				echo "HEAD and last-archive have no relevant differences"; \
				echo "There is nothing to do here"; \
			else mkdir copy-to-archive && (cd copy-to-archive; mkdir TeX; mkdir pdf); \
				get_archive_files | while read filename; do \
					echo "Copying $${filename} to copy-to-archive/"; \
					cp worksheets/$${filename} copy-to-archive/TeX/$${filename}; \
					copy_pdf $${filename}; \
				done; \
				git branch -M last-archive $$(convert_commit_date last-archive); \
				git branch -f last-archive HEAD; \
			fi; \
		fi; \
	else mkdir copy-to-archive && (cd copy-to-archive; mkdir TeX; mkdir pdf); \
		get_archive_files | while read filename; do \
			echo "Copying $${filename} to copy-to-archive/"; \
			cp worksheets/$${filename} copy-to-archive/TeX/$${filename}; \
			copy_pdf $${filename}; \
		done; \
		git branch last-archive HEAD; \
	fi

wscols=3
worksheetcount=0

worksheets.html: grading.lisp
	if test $(worksheetcount) -eq 0; then \
		num_worksheets=$$(cd ./worksheets; ls -1 panike-222-s10-*tex | wc -l);\
	else num_worksheets=$(worksheetcount); fi; \
	sbcl --noinform --load ./grading.lisp \
	--eval "(create-table $${num_worksheets} $(wscols))" --eval "(sb-ext:quit)" > $@

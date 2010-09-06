include ../rules.mk

quiz-20100407.dvi: quiz-20100407.1

SOURCES:=$(shell ls *.tex *.mp 2>/dev/null) Makefile

clean:
	$(RM) $(filter-out $(SOURCES),$(shell /bin/ls))

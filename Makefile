ALL_TEX = propagators.tex
PDF = propagators.pdf
LATEX = pdflatex -shell-escape
SPELL = aspell check -len_GB

.PHONY: all
all: propagators.pdf

.PHONY: open
open: propagators.pdf
	evince $(PDF)

propagators.pdf: $(ALL_TEX) #images/*
	$(LATEX) $<
	$(LATEX) $<
	$(LATEX) $<

re: clean all

.PHONY: spell
spell: $(ALL_TEX)
	for x in $(ALL_TEX) ; do \
	  $(SPELL) $$x ; \
	done

.PHONY: clean
clean:
	rm -rf $(PDF) *.loc *.toc *.log *.idx *.aux *.out *.nav *.snm *.vrb *.blg *.bbl *.pdf_tex


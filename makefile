all: modules figures pdf clean

modules:
	git submodule update --init ./


figures:
	make -C ./figures/

pdf:
	pdflatex faithfulness_poster.tex
	pdflatex faithfulness_poster.tex
	pdflatex faithfulness_poster.tex
	pdflatex faithfulness_poster.tex

clean: 
	- rm -f *.log
	- rm -f *.soc
	- rm -f *.toc
	- rm -f *.aux
	- rm -f *.out
	- rm -f main.idx
	- rm -f *.bbl
	- rm -f *.bbg
	- rm -f *.dvi
	- rm -f *.blg
	- rm -f *.lof
	- rm -f *.nav
	- rm -f *.snm
	- rm -f *~



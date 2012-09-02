.PHONE: clean


include-stamp: ../*.lisp ../*.asd
	sbcl \
	--eval '(require :sb-texinfo)' \
	--eval '(require :wo-graph)' \
	--eval '(sb-texinfo:generate-includes "include/" (list :wo-graph) :base-package :wo-graph)' \
	--eval '(quit)'
	touch include-stamp

%.html: %.texinfo style.css include-stamp
	makeinfo --html --no-split --css-include=style.css $<

%.pdf:  %.texinfo include-stamp
	texi2pdf $<

%.info: %.texinfo include-stamp
	makeinfo --no-split $<


%.neato.pdf: %.dot
	neato -Tpdf -o $@ $<

%.dot.pdf: %.dot
	dot -Tpdf -o $@ $<

html: wo-graph.html

pdf:   wo-graph.pdf

info: wo-graph.info


pages: html
	git checkout gh-pages
	cp wo-graph.html ../index.html
	git commit -a -C master
	rm ../index.html
	git checkout -f master

install-info: info
	cp wo-graph.info ~/share/info/
	install-info wo-graph.info ~/share/info/dir

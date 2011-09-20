all: talk.html

talk.html: talk.md
	pandoc --offline -s -t slidy -o $@ $<

clean:
	-rm -f talk.html

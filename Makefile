all: build/main.js build/new-trump-game.css build/index.html

build/main.js : main.hs
	hastec $< --out=$@

build/new-trump-game.css : new-trump-game.sass
	sass $< $@

build/index.html : index.html
	cp $< $@

clean:
	rm build/main.js build/new-trump-game.css build/index.html

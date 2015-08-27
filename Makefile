all: build/main.js build/board-trump.css build/index.html

build/main.js : main.hs Game/BoardTrump/CPU.hs Game/BoardTrump/Cards.hs Game/BoardTrump/Core.hs Game/BoardTrump/Player.hs Game/BoardTrump/Util.hs Game/BoardTrump.hs
	hastec $< --out=$@

build/board-trump.css : board-trump.sass
	sass $< $@

build/index.html : index.html
	cp $< $@

clean:
	rm build/main.js build/board-trump.css build/index.html

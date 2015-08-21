all: build/main.js build/new-trump-game.css build/index.html

build/main.js : main.hs Game/BoardTrump/CPU.hs Game/BoardTrump/Cards.hs Game/BoardTrump/GameState.hs Game/BoardTrump/Player.hs Game/BoardTrump/Util.hs
	hastec $< --out=$@

build/new-trump-game.css : new-trump-game.sass
	sass $< $@

build/index.html : index.html
	cp $< $@

clean:
	rm build/main.js build/new-trump-game.css build/index.html

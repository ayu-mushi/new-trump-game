all:
	hastec ./main.hs --out=./build/main.js --with-js=./gettimeofday.js
	sass ./new-trump-game.sass ./build/new-trump-game.css
	cp ./index.html ./build/index.html

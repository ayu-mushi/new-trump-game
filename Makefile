all:
	hastec ./main.hs --out=./build/main.js
	sass ./new-trump-game.sass ./build/new-trump-game.css
	cp ./index.html ./build/index.html

commit:
	git commit -m $1
	make all
	cd build; git add .; git commit -m $1

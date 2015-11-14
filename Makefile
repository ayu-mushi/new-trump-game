all: build/main.js build/board-trump.css build/index.html build/umi

build/main.js : main.hs Game/BoardTrump/CPU.hs Game/BoardTrump/Types.hs Game/BoardTrump/Core.hs Game/BoardTrump/Util.hs Game/BoardTrump.hs
	hastec $< --out=$@

build/board-trump.css : board-trump.sass
	sass $< $@

build/index.html : index.html
	cp $< $@

build/umi :
	wget https://github.com/NKMR6194/Umi/releases/download/v3.3.5-3/bootstrap-umi-3.3.5-3-dist.zip -O build/umi.zip
	unzip build/umi.zip -d build/

clean:
	rm build/main.js build/board-trump.css build/index.html

rebuild: clean all

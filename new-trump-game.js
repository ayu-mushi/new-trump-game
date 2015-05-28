//general
var constant = function(a){return function(){return a}};

//special
function mkPlayer(){
	return {
		hands:[],
		deck:[],
		selectedCardInHand:undefined
	};
}

var field,
	decks = new Array(2),
	isMyTurn,
	yours=mkPlayer(),
	mine=mkPlayer();

function mkfield(column, row){
	field = new Array(column);

	for(var i=0; i!=column; i++){
		field[i] = new Array(row);

		$("table#field").append("<tr id='r" + i + "'></tr>")

		for(var j=0; j!=row; j++){
			$("table#field tr#r"+i).append("<td class='d"+ j +"'></tr>");
		}
	}
}

//白紙は、理想的には0、表現は「w」

function expressionOfCard(card){ 
	var expr = ["w","A",,,,,,,,,,"J","Q","K"];
	return _(expr).has(card) ? expr[card] : card;
}

function summon(card, p){
	field[p[0]][p[1]] = card;
	$("table#field tr#r"+p[0]+" td.d"+p[1]).html(expressionOfCard(card));
} 

function summonByHand(i, p){
	
}

function clear(p){ //clearは単に消え、dieは墓地へ行く
	field[p[0]][p[1]] = undefined;
	$("table#field tr#r"+p[0]+" td.d"+p[1]).empty();
}

function die(p){
	
}

function move(p, q){
	
}

function forThePlayer(f, numberOfThePlayer){
	return f($(numberOfThePlayer ? "#yours" : "#mine"), (numberOfThePlayer ? yours : mine));
}

function draw(exprOfThePlayer, thePlayer){ //描画《ドロー》でなく抜札《ドロー》
	var card=thePlayer.deck.pop();
	thePlayer.hands.push(card);

	exprOfThePlayer.children(".hand").append("<li>"+expressionOfCard(card)+"</li>");
	exprOfThePlayer.children(".deck").text(thePlayer.deck.length);
}

function hurigoma(){
	isMyTurn = Math.round(Math.random());
}

function shuffle(arr){
	var i, j, temp;
	arr = arr.slice();
	i = arr.length;
	if(i===0){
		return arr;
	}
	while(--i){
		j = Math.floor(Math.random() * (i + 1));
		temp = arr[i];
		arr[i] = arr[j];
		arr[j] = temp;
	}
	return arr;
}

function mkDeck(n, m){
	var d = [];
	for(var i=0; i!=n; i++){
		for(var j=0; j!=m; j++){
			d[i*m + j]=j;
		}
	}
	return d;
}

var getCost = _.iff(_.lt(10), constant(2), constant(0));
var getEnergy = _.iff(_.eq(0), constant(2), constant(1));

function selectInHand(exprOfThePlayer, thePlayer, i){
	exprOfThePlayer.find("ol.hand li.selected").removeClass("selected");
	exprOfThePlayer.find("ol.hand li:nth-child("+i+")").addClass("selected");
	thePlayer.selectedCardInHand = i;
}

function gameLoop(){
	$("#yours ol.hand li").click(function(){
		forThePlayer(_.auto_partial(3, selectInHand), 1)
			($("#yours ol.hand li").index(this)+1);
	});
}

function init(){
	mkfield(5, 3);
	_([0,1]).each(_.partial(forThePlayer,
		function(thePlayerExpr,thePlayer){
			thePlayer.deck = mkDeck(4,14);//第2(実)引数が14なのは白紙を含めているから
			thePlayer.deck = shuffle(thePlayer.deck);
			for(var i=3;i;i--){
				draw(thePlayerExpr,thePlayer);
			}
			}));
	hurigoma();
}

init();
gameLoop();

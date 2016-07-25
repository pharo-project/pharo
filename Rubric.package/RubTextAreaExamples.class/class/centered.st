examples
centered
	"self centered"

	RubEditingArea new
		beWrapped;
		width: 300;
		updateTextWith: 'AAAAAA' , Character cr asString , 'BBBB';
		selectFrom: 5 to: 8;
		centered;
		openInWorld
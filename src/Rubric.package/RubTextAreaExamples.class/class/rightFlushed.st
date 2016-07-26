examples
rightFlushed
	"self rightFlushed"

	RubEditingArea new
		beWrapped;
		width: 300;
		updateTextWith: 'AAAAAA' , Character cr asString , 'BBBB';
		rightFlush;
		openInWorld
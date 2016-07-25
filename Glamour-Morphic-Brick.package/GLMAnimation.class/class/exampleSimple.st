sources
exampleSimple
	|brick|

	brick := GLMLabelBrick new
		color: Color lightBlue;
		textPosition: #center.

	GLMAnimation new
		linear;
		duration: 5000;
		from: 50@50;
		to: 400@400;
		animation: [ :b :v | b extent: v ];
		onStepped: [ :b :delta | b text: ((delta * 100) asInteger asString, '%') ];
		onCompleted: [ :b | b invalidateWith: [ b color: Color lightGreen; text: 'Done! 100%' ] ];
		animate: brick.
	
	brick asBrickedMorph
		extent: 400@400;
		openInWindow
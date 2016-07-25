initialization
initialize
	super initialize.
	counter := 0.
	self stringMorph
		contents: 'Keys pressed: ' , counter printString;
		openInWorld
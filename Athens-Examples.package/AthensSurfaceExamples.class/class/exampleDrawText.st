examples
exampleDrawText
"
self subclasses anyOne exampleDrawText
"
	| surf  font |
	
	font := LogicalFont familyName: 'Arial' pointSize: 20.

	surf := self newSurface: 300@100.
	
	surf drawDuring: [:can |
		"clear background"
		surf clear: Color white.

		"set font and color"
		can setFont: font.
		can setPaint: Color black.
		
		"translate an origin by font's ascent, otherwise 
		we will see only things below baseline"
		can pathTransform translateX: 0 Y: (font getPreciseAscent).
		can drawString: 'Hello Athens!'.
	].
	Display getCanvas drawImage: 	surf asForm  at: 0@0

examples
draw2Strings
"
self subclasses anyOne exampleDrawText
"
	| surf  font1 font2 ascent advance |
	
	font1 := LogicalFont familyName: 'Arial' pointSize: 10.
	font2 := LogicalFont familyName: 'Tahoma' pointSize: 20.

	ascent := font1 getPreciseAscent max: font2 getPreciseAscent.
	
	surf := self newSurface: 600@200.
	
	surf drawDuring: [:can |
		surf clear: Color white.
		can pathTransform loadIdentity.

		can pathTransform translateX: 30 Y: 30;
		scaleBy: 1.9.

		can setPaint: (Color black).
		can setFont: font1.
		can pathTransform translateX: 0 Y: ascent.

		advance := can drawString: 'Keep '.
		can setFont: font2.

		"The #drawString: method answers a cumulative advance of rendered string,
		so by translating origin with advance, we can draw another string on 'same' line "
		can pathTransform translateBy: advance.
		
		advance := can drawString: 'IT'.
		can setFont: font1.
		can pathTransform translateBy: advance.
		can drawString: 'in mind.'.
		
	].
	Display getCanvas drawImage: 	surf asForm  at: 0@0

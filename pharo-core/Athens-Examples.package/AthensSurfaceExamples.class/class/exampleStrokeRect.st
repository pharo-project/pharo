examples
exampleStrokeRect
" 
Draw a frame rectangle, rotate & transform it in a loop

 self exampleStrokeRect
"

	| surf |
	
	surf := self newSurface: 400@400.
	
	surf drawDuring: [:can | | path |
	
		can pathTransform translateX: 200 Y: 200.	
		can pathTransform scaleBy: 8.
		
		1 to: 1000 do: [:i |
		
			(can setStrokePaint: Color random) width: 1.
			can pathTransform restoreAfter: [
				can pathTransform rotateByDegrees: i*5.
				can pathTransform scaleBy: (1- ( i/2000)).
				surf clear.
				can drawShape: (0@0 corner: 25@25) ].
			Display getCanvas drawImage: 	surf asForm  at: 0@0. 
		]	
	].

examples
example3stroke
" 
Draw simple stroke  path, changing the transformation and colors to get some animated effects.

 self  example3stroke 
"

	| surf |
	
	surf := self newSurface: 400@400.
	
	surf drawDuring: [:can | |  path |
	
		(can setStrokePaint: Color blue) width: 1.

		can pathTransform translateX: 200 Y: 200.	
		can pathTransform scaleBy: 1.

		path := can createPath: [:pathbuilder |
				pathbuilder 
					absolute;
					moveTo: -25 @ -25;
					curveVia: 25@ -25 to: 25@25;
					curveVia: -25@25 to: -25@ -25
			].
		
		1 to: 1000 do: [:i |
		
			(can setStrokePaint: Color random) width: 1.
			can pathTransform restoreAfter: [
				can pathTransform rotateByDegrees: i*5.
				can pathTransform scaleBy: (1- ( i/2000)).
				can drawShape: path.
			].
			Display getCanvas drawImage: 	surf asForm  at: 0@0. 
		]	
	].

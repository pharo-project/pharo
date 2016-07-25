examples
example3
" 
Draw simple filled path, changing the transformation and colors to get some animated effects.

 self example3 
"

	| surf |
	
	surf := self newSurface: 800@800.
	
	surf drawDuring: [:can | | transform path |
	
		can setPaint: Color blue.

		can pathTransform translateX: 200 Y: 200.	
		can pathTransform scaleBy: 6.

		path := can createPath: [:pathbuilder |
			pathbuilder 
				absolute;
				moveTo: -25 @ -25;
				curveVia: 25@ -25 to: 25@25;
				curveVia: -25@25 to: -25@ -25
		].
		
		
			1 to: 1000 do: [:i |
				can setPaint: Color random.

				can pathTransform restoreAfter: [

					can pathTransform rotateByDegrees: i*5.
					can pathTransform scaleBy: (1- ( i/2000)).
					can drawShape: path.
				].
				Display getCanvas drawImage: 	surf asForm  at: 0@0. 
		]	
	].

examples
example6stroke
"
self example6stroke


Draw two rectangles, stoked by linear gradient. 

Note that it should produce same rectangles with exact same fills, because gradient paint 
coordinates are affected by pathTransform matrix.
"

	| surf paint |
	
	surf := self newSurface: 100@100.
	
	paint := surf 
		createLinearGradient: { 0->Color red .  1->Color green } 
		start: 0@0 
		stop: 50@50.
	
	surf drawDuring: [:can | | stroke |
		surf clear.
		
		stroke := can setStrokePaint: paint.
		stroke width: 4.
		can drawShape: (0@0 corner: 50@50).
		
		can pathTransform translateX: 50 Y: 50.
		can drawShape: (0@0 corner: 50@50).
		
	].

	Display getCanvas drawImage: 	surf asForm  at: 0@0

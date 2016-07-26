examples
example6point1
"Draw two rectangles, filled by linear gradient. 

Note that it should produce same rectangles with exact same fills, because gradient paint 
coordinates are affected by pathTransform matrix.


self example6point1
"

	| surf paint |
	
	surf := self newSurface: 100@200.
	
	paint := surf 
		createLinearGradient: { 0->Color red .  1->Color green } 
		start: 0@0 
		stop: 30@30.
	
	surf drawDuring: [:can |
		surf clear.
		
		can setPaint: paint.

		can drawShape: (0@0 corner: 50@50).
	].

	Display getCanvas drawImage: 	surf asForm  at: 0@0

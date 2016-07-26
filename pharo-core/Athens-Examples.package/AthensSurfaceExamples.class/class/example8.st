examples
example8
"Draw a rectangle filled by radial gradient. 
self example8
"

	| surf paint |
	
	surf := self newSurface: 200@200.
	
	paint := surf 
		createRadialGradient: { 0->Color red . 1->Color green } 
		center: 100@100
		radius: 100.
	
	surf drawDuring: [:can |
			surf clear.

			can pathTransform loadIdentity.
			can setPaint: paint.

			can drawShape: (0@0 corner: 200@200).
		
			Display getCanvas drawImage: 	surf asForm  at: 0@0
	].


examples
example7
"Draw two rectangles, filled by linear gradient. 
Rotate the gradient in a loop.

Note that it should produce same rectangles with exact same fills, because gradient paint 
coordinates are affected by pathTransform matrix.

self example7
"

	| surf paint |
	
	surf := self newSurface: 100@100.
	
	paint := surf 
		createLinearGradient: { 0->Color red .  1->Color green } 
		start: 0@0 
		stop: 50@50.
	
	surf drawDuring: [:can |

		1 to: 10000 do: [:i |
			surf clear.

			can pathTransform loadIdentity.

			can paintTransform loadIdentity translateX: 15 Y: 15; rotateByDegrees: (i/10000 * 360).		
			can setPaint: paint.

			can drawShape: (0@0 corner: 50@50).
		
			can pathTransform translateX: 50 Y: 50.
			can drawShape: (0@0 corner: 50@50).

			Display getCanvas drawImage: 	surf asForm  at: 0@0
		].		
	].


examples
example9
"
Fill the rectangle using image paint.
self example9
"

	| surf paint f  |
	
	surf := self newSurface: 200@200.
	
	surf asForm getCanvas fillRectangle: surf asForm boundingBox color: Color white.
	
	f := Form extent: 10@10 depth: 32.
	f getCanvas fillRectangle: (0@0 corner: 5@5) color: (Color red alpha: 0.1).
	f getCanvas fillRectangle: (5@5 corner: 10@10) color: (Color green alpha: 0.1).

	paint := surf createFormPaint: f.
	
	surf drawDuring: [:can |

			can setPaint: paint.

			can drawShape: (0@0 corner: 100@200).
		
			Display getCanvas drawImage: 	surf asForm  at: 0@0
	].


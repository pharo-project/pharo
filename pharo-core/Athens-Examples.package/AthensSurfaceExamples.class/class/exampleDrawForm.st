examples
exampleDrawForm
"self exampleDrawForm"
	| surf surf2   |

	surf2 := self newSurface: 100@100.
	surf2 drawDuring: [:c |
		c setPaint: (Color red alpha: 0.5) .
		c drawShape: (0@0 corner: 100@100).
		].
	
	surf := self newSurface: 300@300.
	
	surf drawDuring: [:can |
		surf clear.
		can pathTransform loadIdentity.

		can setShape: (0@0 corner: 300@300).
		can setPaint: Color black.
		can draw.
		can pathTransform translateX: 30 Y: 30. 
		can setPaint: surf2.
		can draw.

		can pathTransform translateX: 30 Y: 30. 
		can draw.

	].
	Display getCanvas drawImage: surf asForm  at: 0@0

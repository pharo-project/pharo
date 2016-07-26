examples
exampleClip

"
self exampleClip
"
	| surf |
	
	surf := self newSurface: 100@100.
	
	surf drawDuring: [:can |
		surf clear.
		can pathTransform loadIdentity.
		can setPaint: (Color blue).
		can drawShape: (0@0 corner: 100@ 100).

		can pathTransform translateX: -20 Y: -20.
		can clipBy: (20@20 corner: 80@80) during: [
			can pathTransform translateX: 20 Y: 20.
			can setPaint: (Color red).
			can drawShape: (0@0 corner: 100@ 100).
		]
	].
	Display getCanvas drawImage: 	surf asForm  at: 0@0

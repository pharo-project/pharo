*Athens-Morphic
asAthensPaintOn: anAthensCanvas
	^ (anAthensCanvas surface createSolidColorPaint: color)
		origin: origin;
		extent: extent;
		yourself
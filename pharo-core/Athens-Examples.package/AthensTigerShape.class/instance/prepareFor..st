as yet unclassified
prepareFor: surface
	fill ifTrue: [
		fillPaint := surface createSolidColorPaint: fillPaint.
	].

	stroke ifTrue: [
		strokePaint := 
			surface createStrokePaintFor: (surface createSolidColorPaint: strokePaint).
	].

	
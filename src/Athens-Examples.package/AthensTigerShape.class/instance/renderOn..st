as yet unclassified
renderOn: can

	fill ifTrue: [ 
		can setPaint: fillPaint.
		can drawShape: path.	
	].

	stroke ifTrue: [
		can setStrokePaint: strokePaint.	
		can drawShape: path.	
	]
drawing
drawOnAthensCanvas: aCanvas
	| path |
	path := aCanvas
		createPath: [ :builder | 
			builder
				absolute;
				moveTo: self vertices first.
			self vertices allButFirstDo: [ :pt | builder lineTo: pt ].
			builder close ].
	aCanvas setShape: path.
	aCanvas setPaint: self backgroundColor.
	aCanvas draw.
	borderWidth > 0
		ifTrue: [ 
			(aCanvas setStrokePaint: borderColor)
				capSquare;
				width: borderWidth.
			aCanvas draw ]
drawing
displayIconAt: aPosition onAthensCanvas: aCanvas
	self icon ifNil: [ ^self ].
	aCanvas pathTransform
		restoreAfter: [ 
			aCanvas paintOptions composeOver.
			aCanvas setPaint: self icon.
			aCanvas pathTransform translateBy: aPosition.
			aCanvas drawShape: self icon boundingBox]
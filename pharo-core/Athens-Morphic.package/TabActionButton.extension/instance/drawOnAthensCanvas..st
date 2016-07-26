*Athens-Morphic
drawOnAthensCanvas: anAthensCanvas
	super drawOnAthensCanvas: anAthensCanvas.
	icon ifNil: [ ^ self ].
	icon form ifNil: [ ^ self ].
	anAthensCanvas setPaint: icon form.
	anAthensCanvas pathTransform
		restoreAfter: [ 
			anAthensCanvas pathTransform translateBy: self topLeft.
			anAthensCanvas drawShape: (0@0 extent: icon form extent) ]
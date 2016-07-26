*Athens-Morphic
drawIconAndLabelOnAthensCanvas: anAthensCanvas
	anAthensCanvas pathTransform
		restoreAfter: [ 
			self drawIconOnAthensCanvas: anAthensCanvas.
			self drawLabelOnAthensCanvas: anAthensCanvas ]
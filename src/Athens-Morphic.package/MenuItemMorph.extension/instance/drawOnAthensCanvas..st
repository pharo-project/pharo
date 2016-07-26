*Athens-Morphic
drawOnAthensCanvas: anAthensCanvas
	| bnd |
	bnd := self bounds topLeft + self layoutInset.
	anAthensCanvas pathTransform
		restoreAfter: [ 
			anAthensCanvas pathTransform translateBy: bnd.
			self drawBackgroundOnAthensCanvas: anAthensCanvas.
			self drawIconAndLabelOnAthensCanvas: anAthensCanvas.
			self drawSubMenuMarkerOnAthensCanvas: anAthensCanvas ]
drawing
render
	surface drawDuring: [ :canvas |
		surface clear.
		canvas pathTransform restoreAfter: [ 
			"before rendering transform from a global coordinates (Morphic) to relative coordinates (Athens)"
			canvas pathTransform translateBy: self bounds origin negated.
			self fullDrawOnAthensCanvas: canvas.
		].
	].

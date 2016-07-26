transformations
restoreAfter: aBlock
	|previous|
	previous := self copy.
	aBlock ensure: [ 
		"self copyFromMatrix: previous. 
		"self loadAffineTransform: previous ]
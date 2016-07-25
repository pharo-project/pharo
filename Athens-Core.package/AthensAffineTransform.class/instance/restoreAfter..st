transformations
restoreAfter: aBlock
	|previous|
	previous := self copy.
	aBlock ensure: [ 	self loadAffineTransform: previous ]
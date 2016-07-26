transformations
multiplyBy: anAthensAffineTransform
	
	self primMultiplyBy: (
		self class new loadAffineTransform: anAthensAffineTransform)
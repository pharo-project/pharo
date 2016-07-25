accessing
image: anImage 
	
	image := anImage. 
	super extent: (2 * self borderWidth) asPoint + image extent.
	self changed
accessing
form: aForm
	image := aForm. 
	super extent: (2 * self borderWidth) asPoint + image extent.
	self changed
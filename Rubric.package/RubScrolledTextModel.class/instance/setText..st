text managing
setText: aText 
	self text:  aText.
	self announcer announce: RubTextUpdatedInModel.
	^ true
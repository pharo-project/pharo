text managing
setInitialText: aText 
	self setText:  aText.
	self clearUserEdits.
	^ true
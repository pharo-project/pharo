accessing
setDescription: aText 
	self status reset.
	self addToStatus: aText.
	self changed: #getDescription
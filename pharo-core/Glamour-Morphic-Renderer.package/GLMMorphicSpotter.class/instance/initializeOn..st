initialization
initializeOn: aRequest
	model := aRequest.
	self initialize.
	self title: aRequest prompt.
	self text: aRequest default

initialization
initializeOn: aRequest
	model := aRequest.
	listValues := listLabels := #().
	listIndex := 0.
	self initialize.
	self title: aRequest prompt.
	self text: aRequest default

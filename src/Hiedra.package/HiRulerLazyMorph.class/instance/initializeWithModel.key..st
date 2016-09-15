initialization
initializeWithModel: aModel key: anObject
	self initialize.
	
	model := aModel.
	key := anObject.
	
	self syncExtent.
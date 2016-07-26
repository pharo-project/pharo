registration
register: aSessionHandler inCategory: aCategory atPriority: anInteger 
	self unregisterHandler: aSessionHandler handledId.
	aCategory 
		register: aSessionHandler
		atPriority: anInteger
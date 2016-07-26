asserting
assertHandledId: aSessionHandlerList equals: aCollection
	self 
		assert: (aSessionHandlerList collect: #handledId)
		equals: aCollection
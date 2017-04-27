initialization
initialize

	super initialize.
	
	blocks := OrderedCollection with:
		(OmBlock
			store: self
			startPosition: 0
			endPosition: self blockSize - 1
			firstEntryReference: (self referenceToLocalName: '1')).
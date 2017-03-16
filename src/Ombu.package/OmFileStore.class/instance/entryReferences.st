accessing
entryReferences
	
	self checkIfMustRefresh.
	
	^ (entryCount to: 1 by: -1) collect: [:index |
			self referenceToLocalName: index asString ]
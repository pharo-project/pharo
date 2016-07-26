accessing
listAtCategoryNamed: aName		
	^ (self protocolOrganizer methodsInProtocolNamed: aName) asArray
accessing
referenceToLocalName: aString

	self flag: #fix. "Remove this method (moved to superclass)"
	^ OmReference globalName: self globalName localName: aString
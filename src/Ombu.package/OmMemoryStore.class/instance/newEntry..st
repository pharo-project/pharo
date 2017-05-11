writing
newEntry: anEntry

	| localName newReference |
	localName := (entries size + 1) asString.
	newReference := self referenceToLocalName: localName.
	anEntry tags at: self selfReferenceKey put: newReference.
	entries add: anEntry.

writing
newEntry: anEntry

	self critical: [
		| newReference localName |
		entryCount := entryCount + 1.

		"Build new entry"
		localName := entryCount asString.
		newReference := self referenceToLocalName: localName.
		anEntry tags at: self selfReferenceKey put: newReference.

		"Update head"
		headReference := newReference.

		"Cache (weak)"
		self entryByLocalName at: localName put: anEntry.

		"Defer write"
		self entryBuffer addLast: anEntry -> localName.
		self writingDeferrer schedule.
		]
accessing
narrowWith: aString
	self narrowString: aString.
	self loadEntries.
	self nextModel ifNotNil: [:nm | nm narrowWith: aString].
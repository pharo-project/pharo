accessing method dictionary
deregisterLocalSelector: aSymbol
	self basicLocalSelectors notNil ifTrue: [
		self basicLocalSelectors remove: aSymbol ifAbsent: []]
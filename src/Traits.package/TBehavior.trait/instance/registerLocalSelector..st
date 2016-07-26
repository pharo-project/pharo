accessing method dictionary
registerLocalSelector: aSymbol
	self basicLocalSelectors notNil ifTrue: [
		self basicLocalSelectors add: aSymbol]
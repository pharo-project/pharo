tests
testMethodRemovalWithMethodAdded

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass removeSelector: #fortyTwo.
	self setHeadAsInputEntry.
	aClass compile: 'fortyTwo ^42' classified: 'number'.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpMethodRemoval.
		self assert: output methodAffectedSourceCode equals: 'fortyTwo ^42'.
		self assert: output methodAffectedProtocol equals: 'number'.
		]
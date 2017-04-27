tests
testMethodModificationWithMethodRemoved

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass compile: 'fortyTwo ^nil' classified: 'number'.
	self setHeadAsInputEntry.
	aClass removeSelector: #fortyTwo.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpMethodAddition.
		self assert: output methodAffectedSourceCode equals: 'fortyTwo ^nil'.
		self assert: output methodAffectedProtocol equals: 'number'.
		]
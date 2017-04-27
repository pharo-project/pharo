tests
testMethodAdditionWithMethodRemoved

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.
	aClass removeSelector: #fortyTwo.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpMethodAddition.
		self assert: output methodAffectedSourceCode equals: 'fortyTwo ^42'.
		self assert: output methodAffectedProtocol equals: 'number'.
		]
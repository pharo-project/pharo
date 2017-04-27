tests
testMethodAdditionWithSourceCodeChanged

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.
	aClass compile: 'fortyTwo ^nil'.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpMethodModification.
		self assert: output oldSourceCode equals: 'fortyTwo ^nil'.
		self assert: output oldProtocol equals: 'number'.
		self assert: output newSourceCode equals: 'fortyTwo ^42'.
		self assert: output newProtocol equals: 'number'.
		]
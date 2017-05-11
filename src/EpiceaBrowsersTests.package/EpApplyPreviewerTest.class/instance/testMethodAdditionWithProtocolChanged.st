tests
testMethodAdditionWithProtocolChanged

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.
	(aClass>>#fortyTwo) protocol: 'none'.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpMethodModification.
		self assert: output oldSourceCode equals: 'fortyTwo ^42'.
		self assert: output oldProtocol equals: 'none'.
		self assert: output newSourceCode equals: 'fortyTwo ^42'.
		self assert: output newProtocol equals: 'number'.
		]
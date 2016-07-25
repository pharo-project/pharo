tests
testCreationVariableSize
	window := (GLMFinder new)
		variableSizePanes;
		openOn: 123.
	self assert: window submorphs last class equals: GeneralScrollPane
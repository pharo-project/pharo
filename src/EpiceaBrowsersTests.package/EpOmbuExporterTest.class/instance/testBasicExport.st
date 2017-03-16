tests
testBasicExport
	| selection outputResult |
	
	selection := inputLog1 entries.
	
	EpOmbuExporter new 
		outputLog: outputLog;
		fileOut: selection.
	
	outputResult := outputLog events.
	
	self assert: outputResult size equals: selection size.
	self 
		assert: outputResult asSet 
		equals: (selection collect: [ :each | each content ]) asSet
		
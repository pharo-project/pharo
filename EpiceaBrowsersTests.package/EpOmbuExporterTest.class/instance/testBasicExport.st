tests
testBasicExport
	| selection outputResult |
	
	selection := inputLog1 entries.
	
	EpOmbuExporter new 
		outputLog: outputLog;
		fileOut: selection.
	
	outputResult := (outputLog entries collect: [ :each | each content ]) asSet.
	
	self assert: outputLog entries size equals: selection size.
	self assert: outputResult equals: (selection collect: [ :each | each content ]) asSet
		
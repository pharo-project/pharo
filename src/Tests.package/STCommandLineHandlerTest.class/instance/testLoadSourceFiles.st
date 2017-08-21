tests
testLoadSourceFiles
	| codeLoader |
	
	codeLoader := STCommandLineHandler new.
	
	codeLoader loadSourceFiles: {
		'/not/existing/codeLoad1.st'. 
		'/not/existing/codeLoad2.st'}.
	
	self assert: (codeLoader instVarNamed: 'sourceFiles') notNil.
	self assert: (codeLoader instVarNamed: 'sourceFiles') size = 2.
	self assert: ((codeLoader instVarNamed: 'sourceFiles') allSatisfy: [ :each | each isKindOf: File ]).
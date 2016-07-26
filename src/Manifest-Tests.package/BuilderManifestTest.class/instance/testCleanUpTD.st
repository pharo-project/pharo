tests
testCleanUpTD
	| manifestBuilder |
	
	manifestBuilder := TheManifestBuilder of: MFClassA. .
	MFClassA compile:  'foo'.
	manifestBuilder installToDoOf: 'test' version: 0.
	manifestBuilder addToDo: (MFClassA>>#foo) of: 'test' version: 0. 
	
	self assert: (manifestBuilder containsToDo: (MFClassA>>#foo) onRule: 'test' version: 0).
	
	MFClassA removeSelector: #foo.
	manifestBuilder cleanUp.
	
	self assert: ((manifestBuilder toDoOf: 'test' version: 0 ) size = 0) .

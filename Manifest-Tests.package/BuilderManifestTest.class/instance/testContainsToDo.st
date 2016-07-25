tests
testContainsToDo
	
	| manifestBuilder |
	

	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installToDoOf: 'test' version: 0.
	
	manifestBuilder addToDo: MFClassA of: 'test' version: 0. 
	
	self assert: (manifestBuilder containsToDo: MFClassA onRule: 'test' version: 0).
	self deny: (manifestBuilder containsToDo: MFClassB onRule: 'test' version: 0).
	
	
	
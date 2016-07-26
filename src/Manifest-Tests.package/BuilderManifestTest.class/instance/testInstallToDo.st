tests
testInstallToDo
	| manifestBuilder |
	
	manifestBuilder := TheManifestBuilder of: MFClassA. .
	self deny: (manifestBuilder hasToDoOf: 'test' version: 0).
	manifestBuilder installToDoOf: 'test' version: 0.
	self assert: (manifestBuilder hasToDoOf: 'test' version: 0).
	self assert: (manifestBuilder toDoOf: 'test' version: 0) notNil.
	
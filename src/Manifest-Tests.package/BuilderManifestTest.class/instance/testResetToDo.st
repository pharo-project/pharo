tests
testResetToDo
	
	| manifestBuilder array|
	
	array := {MFClassA. MFClassB  }.
	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installToDoOf: 'test' version: 0.
	
	manifestBuilder addAllToDo: array of: 'test' version: 0. 
	
	self assert: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = MFClassA]).
	self assert: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = MFClassB]).
	
	manifestBuilder resetToDoOf: 'test' version: 0. 
	
	self assert:  (manifestBuilder manifest ruletestV0TODO size = 0).
	
	
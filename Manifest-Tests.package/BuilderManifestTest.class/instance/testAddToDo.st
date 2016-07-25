tests
testAddToDo
	
	| manifestBuilder cl mth|
	cl := MFClassA.
	mth := MFClassA >> #method.
	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installToDoOf: 'test' version: 0.
	
	self deny: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = cl]).
	self deny: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = mth]).
	
	manifestBuilder addToDo: cl of: 'test' version: 0. 
	manifestBuilder addToDo: mth of: 'test' version: 0. 
	
	self assert: ((manifestBuilder  toDoOf: 'test' version: 0) anySatisfy: [:each| each = cl]).
	self assert: ((manifestBuilder  toDoOf: 'test' version: 0) anySatisfy: [:each| each = mth]).
	
	manifestBuilder removeToDo: cl of: 'test' version: 0. 
	manifestBuilder removeToDo: mth of: 'test' version: 0. 
	
	self deny: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = cl]).
	self deny: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = mth]).
	
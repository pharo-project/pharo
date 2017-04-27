tests
testClassAdditionWithInstanceVariablesChanged

	| aClass |
	aClass := classFactory newClass.
	self setHeadAsInputEntry.
	
	aClass addInstVarNamed: #x.
	
	self assert: aClass instVarNames equals: #(x).
	self applyInputEntry.
	self assert: aClass instVarNames equals: #()
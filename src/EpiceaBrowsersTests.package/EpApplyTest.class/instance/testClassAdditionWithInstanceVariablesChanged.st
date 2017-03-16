tests
testClassAdditionWithInstanceVariablesChanged

	| aClass anEntry |
	aClass := classFactory newClass.
	anEntry := monitor log head.
	
	aClass addInstVarNamed: #x.
	
	self assert: aClass instVarNames equals: #(x).
	self apply: { anEntry }.
	self assert: aClass instVarNames equals: #()
tests
testClassAdditionWithMetaclassInstanceVariablesChanged

	| aClass |
	aClass := classFactory newClass.
	self setHeadAsInputEntry.
	
	aClass theMetaClass instanceVariableNames: 'x'.
	
	self assert: aClass theMetaClass instVarNames equals: #(x).
	self applyInputEntry.
	self assert: aClass theMetaClass instVarNames equals: #()
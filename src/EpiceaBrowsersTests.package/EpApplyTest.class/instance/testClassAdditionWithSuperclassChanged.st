tests
testClassAdditionWithSuperclassChanged

	| aClass |
	aClass := classFactory newClass.
	self setHeadAsInputEntry.
	
	aClass superclass: Array.
	
	self assert: aClass superclass equals: Array.
	self applyInputEntry.
	self assert: aClass superclass equals: Object
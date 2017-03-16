tests
testClassAdditionWithSuperclassChanged

	| aClass anEntry |
	aClass := classFactory newClass.
	anEntry := monitor log head.
	
	aClass superclass: Array.
	
	self assert: aClass superclass equals: Array.
	self apply: { anEntry }.
	self assert: aClass superclass equals: Object
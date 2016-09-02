tests-class relationship
testChangingClassOfObject

	| classObject object |
	classObject := Association.
	object := 0@1.
	
	MirrorPrimitives setClass: classObject to: object.
	
	self assert: object class equals: classObject.
	self assert: object equals: 0->1
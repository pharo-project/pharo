tests-class relationship
testGettingClassOfObject

	| actual |
	actual := MirrorPrimitives classOf: 0@0.
	
	self assert: actual equals: (0@0) class
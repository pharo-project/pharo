tests-hashes
testGettingIdentityHashOfObject

	| actual object |
	object := 0@0.
	
	actual := MirrorPrimitives identityHashOf: object.
	
	self assert: actual equals: object basicIdentityHash
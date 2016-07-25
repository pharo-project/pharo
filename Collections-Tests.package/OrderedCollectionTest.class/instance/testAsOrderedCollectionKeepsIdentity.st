tests - converting
testAsOrderedCollectionKeepsIdentity
	
	|x y|
	x := (1 to: 5) asOrderedCollection.
	y := x asOrderedCollection.
	self assert: y identityHash = x identityHash  
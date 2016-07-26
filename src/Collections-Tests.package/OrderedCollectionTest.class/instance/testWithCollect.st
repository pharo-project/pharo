tests - enumerating
testWithCollect
	"Allows one to collect some element of two collections into another collection with element corresponding to the condition in the blocks"
	"self run: #testWithCollect"
	
	| c1 c2 res |
	c1 := #(-1 2 -3 4 -5 6 -7 8) asOrderedCollection.
	c2 := #(-9 10 -11 12 -13 14 -15 16) asOrderedCollection.
	res := c1 with: c2 collect: [:each1 :each2 | each1 < each2
		ifTrue: [each1]
		ifFalse: [each2]].
	self assert: (res = #(-9 2 -11 4 -13 6 -15 8) asOrderedCollection).
	
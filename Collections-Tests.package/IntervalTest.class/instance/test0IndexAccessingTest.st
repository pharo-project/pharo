tests - fixture
test0IndexAccessingTest
	self accessCollection.
	self assert: self accessCollection size = 5.
	self subCollectionNotIn.
	self subCollectionNotIn detect: [ :each | (self accessCollection includes: each) not ] ifNone: [ self assert: false ].
	self elementNotInForIndexAccessing.
	self deny: (self accessCollection includes: self elementNotInForIndexAccessing).
	self elementInForIndexAccessing.
	self assert: (self accessCollection includes: self elementInForIndexAccessing).
	self collectionOfFloat.
	self collectionOfFloat do: [ :each | self deny: each class = SmallInteger ]
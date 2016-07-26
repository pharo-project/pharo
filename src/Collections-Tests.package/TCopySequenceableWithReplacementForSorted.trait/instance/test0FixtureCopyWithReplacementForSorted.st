tests - fixture
test0FixtureCopyWithReplacementForSorted
	self collectionOfSize5.
	self assert: self collectionOfSize5 size = 5.
	self replacementCollection.
	self deny: self replacementCollection isEmpty.
	self empty.
	self assert: self empty isEmpty
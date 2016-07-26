tests - fixture
test0CopyTest
	self empty.
	self assert: self empty size = 0.
	self nonEmpty.
	self assert: (self nonEmpty size = 0) not.
	self collectionWithElementsToRemove.
	self assert: (self collectionWithElementsToRemove size = 0) not.
	self collectionWithElementsToRemove do: [ :each | self assert: (self nonEmpty includes: each) ].
	self elementToAdd.
	self deny: (self nonEmpty includes: self elementToAdd).
	self collectionNotIncluded.
	self collectionNotIncluded do: [ :each | self deny: (self nonEmpty includes: each) ]
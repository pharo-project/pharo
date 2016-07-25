tests - fixture
test0FixtureCopyWithReplacementTest
	self replacementCollection.
	self oldSubCollection.
	self collectionWith1TimeSubcollection.
	self assert: (self howMany: self oldSubCollection in: self collectionWith1TimeSubcollection) = 1
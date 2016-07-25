tests - fixture
test0FixtureAddForUniquenessTest
	self element.
	self collectionWithElement.
	self assert: (self collectionWithElement includes: self element).
	self collectionWithoutElement.
	self assert: (self collectionWithElement includes: self element)
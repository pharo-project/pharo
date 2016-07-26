tests - fixture
test0FixtureAddForUniquenessTest
	self element.
	self collectionWithElement.
	self assert: (self collectionWithElement includes: self element).
	self collectionWithoutElement.
	self deny: (self collectionWithoutElement includes: self element)
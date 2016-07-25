tests - fixture
test0FixtureRequirementsOfTAddTest
	self collectionWithElement.
	self otherCollection.
	self element.
	self assert: (self collectionWithElement includes: self element).
	self deny: (self otherCollection includes: self element)
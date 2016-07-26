tests - fixture
test0FixtureIncludeWithIdentityTest
	| anElement |
	self collectionWithCopyNonIdentical.
	anElement := self collectionWithCopyNonIdentical anyOne.
	self deny: anElement == anElement copy
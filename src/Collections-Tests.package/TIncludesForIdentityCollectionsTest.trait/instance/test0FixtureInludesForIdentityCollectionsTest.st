tests - fixture
test0FixtureInludesForIdentityCollectionsTest
	self identityCollectionWithElementsCopyNotIdentical.
	self identityCollectionWithElementsCopyNotIdentical do: [ :each | self deny: each == each copy ]
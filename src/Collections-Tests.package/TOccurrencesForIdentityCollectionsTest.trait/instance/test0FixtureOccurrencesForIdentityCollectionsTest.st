tests - fixture
test0FixtureOccurrencesForIdentityCollectionsTest
	self identityCollectionWithElementsCopyNotIdentical.
	self identityCollectionWithElementsCopyNotIdentical do: [ :each | self deny: each copy == each ]
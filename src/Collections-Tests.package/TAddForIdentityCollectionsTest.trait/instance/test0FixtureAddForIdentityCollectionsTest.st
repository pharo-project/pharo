tests - fixture
test0FixtureAddForIdentityCollectionsTest
	
	self identityCollectionWithElementsCopyNotIdentical.
	self identityCollectionWithElementsCopyNotIdentical  do: [ : each | self deny: each == each copy ].
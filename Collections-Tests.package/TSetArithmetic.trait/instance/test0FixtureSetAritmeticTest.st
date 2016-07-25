tests - fixture
test0FixtureSetAritmeticTest
	self collection.
	self deny: self collection isEmpty.
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self anotherElementOrAssociationNotIn.
	self collection isDictionary
		ifTrue: [ self deny: (self collection associations includes: self anotherElementOrAssociationNotIn key) ]
		ifFalse: [ self deny: (self collection includes: self anotherElementOrAssociationNotIn) ].
	self collectionClass
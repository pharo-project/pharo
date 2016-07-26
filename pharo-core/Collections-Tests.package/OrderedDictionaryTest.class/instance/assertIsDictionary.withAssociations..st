assertions
assertIsDictionary: anObject withAssociations: anAssociationCollection
	self
		assert: (anObject isMemberOf: self dictionaryClass);
		assert: anObject associations = anAssociationCollection
assertions
assertIsDictionary: anObject includesAll: anAssociationCollection
	self
		assert: (anObject isMemberOf: self dictionaryClass);
		assert: (anAssociationCollection allSatisfy: [:each | anObject includesAssociation: each])
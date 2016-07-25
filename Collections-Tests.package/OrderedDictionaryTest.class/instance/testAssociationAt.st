tests
testAssociationAt
	| dictionary |

	dictionary := self dictionaryWithSingleAssociation.
	self
		should: [dictionary associationAt: self absentKey] raise: Error;
		assert: (dictionary associationAt: self singleKey) = self singleAssociation.
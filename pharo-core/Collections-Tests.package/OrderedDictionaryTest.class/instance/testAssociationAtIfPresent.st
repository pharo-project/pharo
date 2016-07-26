tests
testAssociationAtIfPresent
	| dictionary |

	dictionary := self dictionaryWithSingleAssociation.
	self
		assert: (dictionary associationAt: self absentKey ifPresent: [self absentValue]) isNil;
		assert: (dictionary associationAt: self singleKey ifPresent: [self newValue]) = self newValue.
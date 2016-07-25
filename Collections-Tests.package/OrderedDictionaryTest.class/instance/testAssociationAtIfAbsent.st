tests
testAssociationAtIfAbsent
	| dictionary |

	dictionary := self dictionaryWithSingleAssociation.
	self
		assert: (dictionary associationAt: self absentKey ifAbsent: [self absentValue]) = self absentValue;
		assert: (dictionary associationAt: self singleKey ifAbsent: [self absentValue]) = self singleAssociation.
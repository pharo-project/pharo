tests
testKeyAtIdentityValue
	| dictionary singleValue |

	dictionary := self dictionaryWithSingleAssociation.
	singleValue := dictionary at: self singleKey.
	self
		should: [dictionary keyAtIdentityValue: self absentValue] raise: Error;
		should: [dictionary keyAtIdentityValue: singleValue copy] raise: Error;
		assert: (dictionary keyAtIdentityValue: singleValue) = self singleKey.
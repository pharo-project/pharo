tests
testKeyAtValue
	| dictionary |

	dictionary := self dictionaryWithSingleAssociation.
	self
		should: [dictionary keyAtValue: self absentValue] raise: Error;
		assert: (dictionary keyAtValue: self singleValue) = self singleKey.
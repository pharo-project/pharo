tests
testAt
	| dictionary |

	dictionary := self dictionaryWithSingleAssociation.
	self
		should: [dictionary at: self absentKey] raise: Error;
		assert: (dictionary at: self singleKey) = self singleValue.
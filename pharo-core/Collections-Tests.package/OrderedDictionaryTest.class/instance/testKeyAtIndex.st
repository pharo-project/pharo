tests
testKeyAtIndex
	| dictionary |

	self
		should: [self emptyDictionary keyAtIndex: 0] raise: Error;
		should: [self emptyDictionary keyAtIndex: 1] raise: Error.

	dictionary := self dictionaryWithAssociations.
	self dictionaryKeys withIndexDo: [:each :i |
		self assert: (dictionary keyAtIndex: i) = each].
	self
		should: [dictionary keyAtIndex: 0] raise: Error;
		should: [dictionary keyAtIndex: dictionary size + 1] raise: Error.
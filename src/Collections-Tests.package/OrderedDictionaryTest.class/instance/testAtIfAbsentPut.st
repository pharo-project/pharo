tests
testAtIfAbsentPut
	| dictionary |

	dictionary := self dictionaryWithSingleAssociation.
	self
		assert: (dictionary at: self singleKey ifAbsentPut: [self newValue]) = self singleValue;
		assert: (dictionary at: self newKey ifAbsentPut: [self newValue]) = self newValue.
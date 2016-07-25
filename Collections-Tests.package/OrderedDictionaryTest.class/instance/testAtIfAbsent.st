tests
testAtIfAbsent
	| dictionary |

	dictionary := self dictionaryWithSingleAssociation.
	self
		assert: (dictionary at: self absentKey ifAbsent: [self absentValue]) = self absentValue;
		assert: (dictionary at: self singleKey ifAbsent: [self absentValue]) = self singleValue.
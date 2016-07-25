tests
testKeyAtValueIfAbsent
	| dictionary |

	dictionary := self dictionaryWithSingleAssociation.
	self
		assert: (dictionary keyAtValue: self absentValue ifAbsent: [self absentKey]) = self absentKey;
		assert: (dictionary keyAtValue: self singleValue ifAbsent: [self absentKey]) = self singleKey.
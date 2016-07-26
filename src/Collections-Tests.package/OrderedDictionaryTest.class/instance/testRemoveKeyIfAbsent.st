tests
testRemoveKeyIfAbsent
	| dictionary |

	dictionary := self dictionaryWithSingleAssociation.
	self
		assert: (dictionary removeKey: self absentKey ifAbsent: [self absentValue]) = self absentValue;
		assert: (dictionary removeKey: self singleKey ifAbsent: [self absentValue]) = self singleValue;
		deny: (dictionary includesKey: self singleKey).
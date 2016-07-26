tests
testKeyAtIdentityValueIfAbsent
	| dictionary singleValue |

	dictionary := self dictionaryWithSingleAssociation.
	singleValue := dictionary at: self singleKey.
	self assert:
		(dictionary
			keyAtIdentityValue: self absentValue
			ifAbsent: [self absentKey]) = self absentKey.
	self assert:
		(dictionary
			keyAtIdentityValue: singleValue copy
			ifAbsent: [self absentKey]) = self absentKey.
	self assert:
		(dictionary
			keyAtValue: singleValue
			ifAbsent: [self absentKey]) = self singleKey.
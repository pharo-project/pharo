tests
testIdentityIndexOfKeyIfAbsent
	| dictionary |

	dictionary := self dictionaryWithAssociations.
	dictionary keys withIndexDo: [:each :i |
		self
			assert: (dictionary identityIndexOfKey: each ifAbsent: [self absentValue]) = i;
			assert:
				(dictionary
					identityIndexOfKey: each copy
					ifAbsent: [self absentValue]) = self absentValue].

	self assert:
		(dictionary
			identityIndexOfKey: self absentKey
			ifAbsent: [self absentValue]) = self absentValue.
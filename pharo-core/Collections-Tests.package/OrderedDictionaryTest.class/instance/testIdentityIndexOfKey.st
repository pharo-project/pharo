tests
testIdentityIndexOfKey
	| dictionary |

	dictionary := self dictionaryWithAssociations.
	dictionary keys withIndexDo: [:each :i |
		self
			assert: (dictionary identityIndexOfKey: each) = i;
			assert: (dictionary identityIndexOfKey: each copy) isZero].
	self assert: (dictionary identityIndexOfKey: self absentKey) isZero.
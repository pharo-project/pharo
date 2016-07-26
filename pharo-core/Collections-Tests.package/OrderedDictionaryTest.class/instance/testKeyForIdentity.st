tests
testKeyForIdentity
	| dictionary value |

	dictionary := self dictionaryWithSingleAssociation.
	value := dictionary at: self singleKey.
	self
		assert: (dictionary keyForIdentity: value) = self singleKey;
		assert: (dictionary keyForIdentity: value copy) isNil;
		assert: (dictionary keyForIdentity: self absentValue) isNil.
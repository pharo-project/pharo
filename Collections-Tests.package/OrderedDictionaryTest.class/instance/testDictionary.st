tests
testDictionary
	| dictionary |

	dictionary := self emptyDictionary.
	self
		assert: dictionary dictionary class == self internalDictionaryClass;
		assert: dictionary dictionary = self emptyInternalDictionary.
	dictionary := self dictionaryWithSingleAssociation.
	self
		assert: dictionary dictionary class == self internalDictionaryClass;
		assert: dictionary dictionary = self internalDictionaryWithSingleAssociation.
	dictionary := self dictionaryWithAssociations.
	self
		assert: dictionary dictionary class == self internalDictionaryClass;
		assert: dictionary dictionary = self internalDictionaryWithAssociations.
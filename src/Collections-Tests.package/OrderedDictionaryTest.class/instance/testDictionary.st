tests
testDictionary
	| dictionary |

	dictionary := self emptyDictionary.
	self
		assert: dictionary dictionary class == self internalDictionaryClass;
		assert: dictionary dictionary = self emptyInternalDictionary.

	dictionary := self dictionaryWithOrderedAssociations.
	self
		assert: dictionary dictionary class == self internalDictionaryClass;
		assert: dictionary dictionary = self internalDictionaryWithAssociations.
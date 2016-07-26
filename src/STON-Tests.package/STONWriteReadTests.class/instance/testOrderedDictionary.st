tests
testOrderedDictionary
	"OrderedDictionary is special because it does not inherit from Dictionary.
	It might also not exist in some dialects, where this test could be skipped."
	
	| dictionaries orderedDictionaryClass orderedIdentityDictionaryClass |
	orderedDictionaryClass := Smalltalk at: #OrderedDictionary ifAbsent: [ Dictionary ].
	orderedIdentityDictionaryClass := Smalltalk at: #OrderedIdentityDictionary ifAbsent: [ IdentityDictionary ].
	dictionaries := STON listClass withAll: {
		orderedDictionaryClass new at: 1 put: 1; at: 2 put: 2; yourself.
		orderedDictionaryClass new at: #a put: 1; at: #b put: -2; at: #c put: 0; yourself.
		orderedDictionaryClass new.
		orderedIdentityDictionaryClass new at: 1 put: 1; at: 2 put: 2; yourself.
		orderedIdentityDictionaryClass new at: #a put: 1; at: #b put: -2; at: #c put: 0; yourself.
		orderedIdentityDictionaryClass new }.
	self serializeAndMaterialize: dictionaries
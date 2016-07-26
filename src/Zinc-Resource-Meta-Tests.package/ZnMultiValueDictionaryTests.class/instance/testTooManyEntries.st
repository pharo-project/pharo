testing
testTooManyEntries
	| dictionary |
	dictionary := ZnMultiValueDictionary new.
	self
		should: [ 1 to: dictionary limit + 1 do: [ :each | 
						dictionary at: each put: each ] ]
		raise: ZnTooManyDictionaryEntries
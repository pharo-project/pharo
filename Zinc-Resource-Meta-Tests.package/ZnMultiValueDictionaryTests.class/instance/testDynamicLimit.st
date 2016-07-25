testing
testDynamicLimit
	ZnMaximumNumberOfDictionaryEntries
		value: 10
		during: [ 
			| dictionary |
			dictionary := ZnMultiValueDictionary new.
			self assert: dictionary limit equals: 10.
			1 to: 10 do: [ :each | dictionary at: each put: each ].
			self 
				should: [ dictionary at: 11 put: 11 ]
				raise: ZnTooManyDictionaryEntries ]
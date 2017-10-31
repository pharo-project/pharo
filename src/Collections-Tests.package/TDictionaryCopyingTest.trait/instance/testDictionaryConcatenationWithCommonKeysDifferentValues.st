tests - copying
testDictionaryConcatenationWithCommonKeysDifferentValues

	| dictionary1 dictionary2 result value |
	
	dictionary1 := self nonEmptyDict.
	value := self nonEmptyDifferentFromNonEmptyDict   values anyOne.
	dictionary2 := dictionary1 copy.
	dictionary2 keys do: [ :key | dictionary2 at: key put: value ].
	
	
	result := dictionary1 , dictionary2.
	self assert: result size = ( dictionary2 size).
	
	dictionary2 associationsDo: [ :assoc | self assert: (result at: assoc key) = assoc value ]
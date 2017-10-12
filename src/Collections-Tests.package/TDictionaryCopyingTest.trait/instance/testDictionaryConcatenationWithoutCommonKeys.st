tests - copying
testDictionaryConcatenationWithoutCommonKeys

	| dictionary1 dictionary2 result |
	dictionary1 := self nonEmptyDict.
	dictionary2 := self nonEmptyDifferentFromNonEmptyDict.
	result := dictionary1 , dictionary2.
	self assert: result size = (dictionary1 size + dictionary2 size).
	dictionary1 associationsDo: [ :assoc | self assert: (result at: assoc key) = assoc value ].
	dictionary2 associationsDo: [ :assoc | self assert: (result at: assoc key) = assoc value ]
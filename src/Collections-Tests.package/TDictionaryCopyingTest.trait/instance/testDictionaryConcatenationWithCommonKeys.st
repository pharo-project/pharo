tests - copying
testDictionaryConcatenationWithCommonKeys

	| dictionary1 dictionary2 result |
	dictionary1 := self nonEmptyDict.
	dictionary2 := self nonEmptyDict.
	result := dictionary1 , dictionary2.
	self assert: result size = ( dictionary2 size).
	
	dictionary2 associationsDo: [ :assoc | self assert: (result at: assoc key) = assoc value ]
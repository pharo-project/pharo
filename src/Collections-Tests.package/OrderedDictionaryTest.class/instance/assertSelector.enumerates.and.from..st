assertions
assertSelector: aSelector enumerates: aFirstCollection and: aSecondCollection from: aDictionary
	| firstEnumerated secondEnumerated|

	firstEnumerated := OrderedCollection new.
	secondEnumerated := OrderedCollection new.
	aDictionary perform: aSelector with: [:first :second |
		firstEnumerated add: first.
		secondEnumerated add: second].
	self
		assert: firstEnumerated asArray = aFirstCollection asArray;
		assert: secondEnumerated asArray = aSecondCollection asArray.
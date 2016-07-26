assertions
assertSelector: aSelector enumerates: aFirstCollection and: aSecondCollection from: aDictionary
	| firstEnumerated secondEnumerated|

	firstEnumerated := OrderedCollection new.
	secondEnumerated := OrderedCollection new.
	self emptyDictionary perform: aSelector with: [:first :second |
		firstEnumerated add: first.
		secondEnumerated add: second].
	self
		assert: firstEnumerated isEmpty;
		assert: secondEnumerated isEmpty.

	aDictionary perform: aSelector with: [:first :second |
		firstEnumerated add: first.
		secondEnumerated add: second].
	self
		assert: firstEnumerated asArray = aFirstCollection asArray;
		assert: secondEnumerated asArray = aSecondCollection asArray.
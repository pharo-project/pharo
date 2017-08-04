accessing nested
at: firstKey at: secondKey put: aValue
	| subDictionary |
	subDictionary := self at: firstKey ifAbsentPut: [ Dictionary new ].
	^ subDictionary at: secondKey put: aValue

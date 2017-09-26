nested dictionaries
at: firstKey at: secondKey put: aValue
	"Set a value at secondKey in the dictionary returned by firstKey."
	
	| subDictionary |
	subDictionary := self at: firstKey ifAbsentPut: [ Dictionary new ].
	^ subDictionary at: secondKey put: aValue

tests - dictionnary enumerating
testKeysDo
	| collection keys |
	collection := self nonEmptyDict .
	keys := OrderedCollection new.
	collection keysDo: [ :key  | 
		keys add: key.
		].
	
	collection keys do: [ :key | self assert: (collection keys occurrencesOf: key) = (keys occurrencesOf: key)]
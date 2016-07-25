tests - dictionnary enumerating
testKeysAndValuesDo

	
	| collection keys |
	collection := self nonEmptyDict .
	keys := OrderedCollection new.
	collection keysAndValuesDo: [ :key :value | 
		keys add: key.
		self assert: (collection at: key) = value ].
	
	collection keys do: [ :key | self assert: (collection keys occurrencesOf: key) = (keys occurrencesOf: key)]
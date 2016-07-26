tests - dictionnary enumerating
testAssociationsSelect
	| collection keys result |
	collection := self nonEmptyDict .
	keys := OrderedCollection new.
	result := collection associationsSelect: [ :assoc  | 
		keys add: assoc key.
		true].
	
	collection keys do: [ :key | self assert: (collection keys occurrencesOf: key) = (keys occurrencesOf: key)].
	self assert: result = collection.
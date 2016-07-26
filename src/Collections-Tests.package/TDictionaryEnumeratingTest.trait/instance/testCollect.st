tests - dictionnary enumerating
testCollect
	| collection values result |
	collection := self nonEmptyDict .
	values := OrderedCollection new.
	result := collection collect: [ :value  | 
		values add: value.
		].
	
	collection values do: [ :value | self assert: (collection values occurrencesOf: value) = (values occurrencesOf: value)].
	self assert: result = collection.
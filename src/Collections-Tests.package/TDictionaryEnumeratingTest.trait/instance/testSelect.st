tests - dictionnary enumerating
testSelect
	| collection values result |
	collection := self nonEmptyDict .
	values := OrderedCollection new.
	result := collection select: [ :value  | 
		values add: value.
		true].
	
	collection values do: [ :value| self assert: (collection values occurrencesOf: value) = (values occurrencesOf: value)].
	self assert: result = collection.
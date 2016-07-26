tests - dictionnary enumerating
testValuesDo
	| collection values |
	collection := self nonEmptyDict .
	values := OrderedCollection new.
	collection valuesDo: [ :value  | 
		values add: value.
		].
	
	collection values do: [ :value | self assert: (collection values occurrencesOf: value) = (values occurrencesOf: value)]
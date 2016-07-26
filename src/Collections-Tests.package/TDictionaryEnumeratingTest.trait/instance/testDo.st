tests - dictionnary enumerating
testDo
	| t collection |
	collection := self nonEmptyDict .
	t := OrderedCollection new.
	collection do: [:
		value | t add: value
		].
	
	t do: [ :each | self assert: (t occurrencesOf: each ) = ( collection values occurrencesOf: each) ].
tests - dictionary keys values associations access
testValues
	
	| collection result |
	collection := self nonEmpty .
	result := collection values.
	
	self assert: result size = collection size.
	result do: [:each | self assert: (collection occurrencesOf:each ) = (result occurrencesOf: each) ].
	
tests
testCSLineEnders
	self
		assert: String newCSLineEnders asArray
		equals: { Character lf . Character cr }
tests
testNewPerson

	| person |
	
	person := SlotExamplePerson new.
	self assert: person name isNil.
	self assert: person directedMovies isCollection.
	self assert: person directedMovies isEmpty.
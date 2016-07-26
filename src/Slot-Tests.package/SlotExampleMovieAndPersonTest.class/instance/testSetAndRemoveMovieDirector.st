tests
testSetAndRemoveMovieDirector

	| movie person |
	
	movie := SlotExampleMovie named: 'Jaws'.
	person := SlotExamplePerson named: 'Peter'.
	
	movie director: person.
	self assert: movie director equals: person.
	self assert: person directedMovies anyOne equals: movie.
	
	person directedMovies remove: movie.
	self assert: movie director isNil.
	self assert: person directedMovies isEmpty.	
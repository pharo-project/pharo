tests
testAddMovieDirector

	| movie person |
	
	movie := SlotExampleMovie named: 'Jaws'.
	person := SlotExamplePerson named: 'Peter'.
	
	person directedMovies add: movie.
	self assert: movie director equals: person.
	self assert: person directedMovies size equals: 1.
	self assert: person directedMovies anyOne equals: movie.
tests
testChangeMovieDirector

	| movie p1 p2 |
	
	movie := SlotExampleMovie named: 'Jaws'.
	p1 := SlotExamplePerson named: 'P1'.
	p2 := SlotExamplePerson named: 'P2'.
	
	movie director: p1.
	self assert: movie director equals: p1.
	self assert: p1 directedMovies anyOne equals: movie.
	self assert: p2 directedMovies isEmpty.
	
	movie director: p2.
	self assert: movie director equals: p2.
	self assert: p1 directedMovies isEmpty.
	self assert: p2 directedMovies anyOne equals: movie.
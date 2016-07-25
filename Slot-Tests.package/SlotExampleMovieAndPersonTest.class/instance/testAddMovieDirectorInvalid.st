tests
testAddMovieDirectorInvalid

	| person |
	
	person := SlotExamplePerson named: 'Peter'.
	
	self should: [ person directedMovies add: 'not-a-movie' ] raise: Error
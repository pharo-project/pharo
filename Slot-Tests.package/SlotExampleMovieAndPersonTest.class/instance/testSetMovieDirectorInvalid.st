tests
testSetMovieDirectorInvalid

	| movie |
	
	movie := SlotExampleMovie named: 'Jaws'.
	
	self should: [ movie director: 'not-a-person' ] raise: Error.
	self assert: movie director isNil
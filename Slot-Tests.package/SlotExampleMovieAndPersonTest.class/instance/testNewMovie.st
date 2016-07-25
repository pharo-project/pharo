tests
testNewMovie

	| movie |
	
	movie := SlotExampleMovie new.
	self assert: movie name isNil.
	self assert: movie director isNil.
	self assert: movie actors isCollection.
	self assert: movie actors isEmpty.
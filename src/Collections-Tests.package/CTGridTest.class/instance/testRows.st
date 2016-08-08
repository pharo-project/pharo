tests - iterating
testRows

	self assert: grid3x2BooksCreatedWithRows rows first equals: #('A Time to Kill' 'John Grisham' 1989).
	self assert: grid3x2BooksCreatedWithRows rows second equals: #('Blood and Smoke' 'Stephen King' 2010).
	self assert: grid3x2BooksCreatedWithRows rows third equals: #('Spin' 'Robert Charles Wilson' 2006)
tests - instance creation
testAnotherCreationWithRows

	
	self assert: (grid3x2BooksCreatedWithRows atRow: 1 atColumn: 1) equals: 'A Time to Kill'.
	self assert: (grid3x2BooksCreatedWithRows atRow: 2 atColumn: 2) equals: 'Stephen King'.
	self assert: (grid3x2BooksCreatedWithRows atRow: 1 atColumn: 2) equals: 'John Grisham'.
	self assert: (grid3x2BooksCreatedWithRows atRow: 2 atColumn: 1) equals: 'Blood and Smoke'.
	
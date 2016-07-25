tests
testCreateUnexistentCategory
	| categoryToAdd |
	categoryToAdd := KMCategory named: #TestCategory.
	
	self assert: self categoryContainer categories size = 0.
	self categoryContainer addCategory: categoryToAdd.
	self assert: self categoryContainer categories size = 1.
	self assert: (self categoryContainer includesCategory: categoryToAdd).
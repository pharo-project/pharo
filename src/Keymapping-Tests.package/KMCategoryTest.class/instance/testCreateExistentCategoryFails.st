tests
testCreateExistentCategoryFails
	| categoryToAdd |
	categoryToAdd := KMCategory named: #TestCategory.
	
	self assert: self categoryContainer categories size = 0.
	self categoryContainer addCategory: categoryToAdd.
	self assert: self categoryContainer categories size = 1.
	self should: [self categoryContainer addCategory: categoryToAdd] raise: Error.
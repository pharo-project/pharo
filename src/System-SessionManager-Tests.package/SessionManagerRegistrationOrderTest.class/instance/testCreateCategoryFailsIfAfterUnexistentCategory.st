tests
testCreateCategoryFailsIfAfterUnexistentCategory
	| customCategory |
	
	self 
		should: [customCategory := manager createCategory: 'Second' after: 'First']
		raise: NotFound.
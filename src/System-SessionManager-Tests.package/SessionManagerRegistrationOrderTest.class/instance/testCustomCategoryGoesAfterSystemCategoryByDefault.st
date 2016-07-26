tests
testCustomCategoryGoesAfterSystemCategoryByDefault
	| customCategory |
	customCategory := manager createCategory: 'Custom'.
	customCategory register: 'aCustomAction'.
	
	manager systemCategory register: 'aSystemAction' atPriority: 10.
	
	self 
		assert: manager startupList
		equals: #( 'aSystemAction' 'aCustomAction' ).
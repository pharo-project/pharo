tests
testCustomCategoryOrderIsPreserved
	| customCategory |
	customCategory := manager createCategory: 'First'.
	customCategory register: 'first'.	
	
	customCategory := manager createCategory: 'Second' after: 'First'.
	customCategory register: 'second'.
	
	self 
		assert: manager startupList 
		equals: #( 'first' 'second' ).
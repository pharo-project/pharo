testing
testProtocolIsCleared
	self clickOnListItem: self mockCategoryName.
	self clickOnListItem: 'MCMockASubclass'.
	self clickOnListItem: 'as yet unclassified'.
	self clickOnListItem: 'MCMockClassA'.
	
	self denyAListHasSelection: 'as yet unclassified'.
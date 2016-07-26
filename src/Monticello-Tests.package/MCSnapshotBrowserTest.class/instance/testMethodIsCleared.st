testing
testMethodIsCleared
	self clickOnListItem: self mockCategoryName.
	self clickOnListItem: 'MCMockClassA'.
	self clickOnListItem: 'boolean'.
	self clickOnListItem: 'falsehood'.
	self clickOnListItem: AllProtocol defaultName.
	
	self denyAListHasSelection: 'falsehood'.
testing
testComment
	self clickOnButton: '?'.
	self assertTextIs: ''.
	
	self clickOnListItem: self mockCategoryName.
	self assertTextIs: ''.
	
	self clickOnListItem: 'MCMockClassA'.
	self assertTextIs: self classAComment.
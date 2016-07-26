running
tearDown
	super tearDown.
	
	self class environment organization removeCategory: #'EpiceaTestingWith-A-Really-WeirdCategoryName'.

tests
testFullBrowseLastValueNotNilSelectedCategoryIsNil
	| newNautilus |
	self compiledMethod: self class >> #testFullBrowseLastValueNotNil.
	self testOpenOnMethodLastValueNotNil.
	nautilus selectedCategory: nil.
	nautilus selectedMethod: compiledMethod. 
	self assert: nautilus selectedCategory isNil.
	self assert: nautilus selectedMethod equals: compiledMethod.
	expectedMethodCategory := nil.
	newNautilus := nautilus ui fullBrowse.
	nautilus close.
	nautilus := newNautilus.
	self assertSelectedCompiledMethod.
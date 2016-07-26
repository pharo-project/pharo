tests
testFullBrowseLastValueNotNil2
	| newNautilus |
	self compiledMethod: self class >> #testFullBrowseLastValueNotNil.
	self testOpenOnMethodLastValueNotNil.
	newNautilus := nautilus ui fullBrowse.
	nautilus close.
	nautilus := newNautilus.
	self assertSelectedCompiledMethod.
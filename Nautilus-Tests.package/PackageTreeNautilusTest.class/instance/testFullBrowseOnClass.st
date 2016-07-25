tests
testFullBrowseOnClass
	| newNautilus |
	self testOpenOnMethodLastValueIsNil.
	nautilus ui browseClass: compiledMethod methodClass.
	expectedMethodCategory := nil.
	expectedCompiledMethod := nil.
	newNautilus := nautilus ui fullBrowse.
	nautilus close.
	nautilus := newNautilus.
	self assertSelectedCompiledMethod.
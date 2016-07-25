tests
testFullBrowseLastValueNotNil
	| newNautilus |
	self testOpenOnMethodLastValueNotNil.
	newNautilus := nautilus ui fullBrowse.
	nautilus close.
	nautilus := newNautilus.
	self assertSelectedCompiledMethod.
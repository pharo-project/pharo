tests
testFullBrowseLastValueIsNil
	| newNautilus |
	self testOpenOnMethodLastValueIsNil.
	newNautilus := nautilus ui fullBrowse.
	nautilus close.
	nautilus := newNautilus.
	self assertSelectedCompiledMethod.
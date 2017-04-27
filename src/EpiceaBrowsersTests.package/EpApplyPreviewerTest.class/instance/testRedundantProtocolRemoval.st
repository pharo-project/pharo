tests
testRedundantProtocolRemoval

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	aClass removeProtocol: 'protocol'.
	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.
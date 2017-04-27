tests
testRedundantProtocolAddition

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.
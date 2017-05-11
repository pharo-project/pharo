tests
testRedundantClassAddition

	| aClass |
	aClass := classFactory newClass.
	self setHeadAsInputEntry.
	
	self assertEmptyPreviewLog.
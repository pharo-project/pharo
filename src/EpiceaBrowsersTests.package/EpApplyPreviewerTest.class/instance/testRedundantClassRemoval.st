tests
testRedundantClassRemoval

	| aClass |
	aClass := classFactory newClass.
	aClass removeFromSystem.
	self setHeadAsInputEntry.
	
	self assertEmptyPreviewLog.
tests
testRedundantMethodRemovalWithAbsentBehavior

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass removeSelector: #fortyTwo.
	self setHeadAsInputEntry.
	aClass removeFromSystem.

	self assertEmptyPreviewLog.
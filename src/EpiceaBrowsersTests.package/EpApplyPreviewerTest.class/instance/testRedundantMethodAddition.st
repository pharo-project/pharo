tests
testRedundantMethodAddition

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.

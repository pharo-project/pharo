tests
testKeepChangeWhenMethodModificationWithAbsentBehavior

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass compile: 'fortyTwo ^nil' classified: 'number'.
	self setHeadAsInputEntry.
	aClass removeFromSystem.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpMethodModification ]

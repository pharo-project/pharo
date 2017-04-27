tests
testKeepChangeWhenMethodAdditionWithAbsentBehavior

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.
	aClass removeFromSystem.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpMethodAddition ]

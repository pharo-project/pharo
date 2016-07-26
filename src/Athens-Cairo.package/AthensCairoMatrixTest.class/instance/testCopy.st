tests
testCopy

	| m |
	m := AthensCairoMatrix new.
	self assert: m getHandle equals: m copy getHandle.
	self assert: m getHandle ~~ m copy getHandle.
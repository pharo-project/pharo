tests
testInverted

	| m |
	m := AthensCairoMatrix new.
	self assert: m getHandle ~~ m inverted getHandle.
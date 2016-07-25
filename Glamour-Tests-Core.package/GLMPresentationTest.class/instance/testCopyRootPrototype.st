tests
testCopyRootPrototype
	| presentation newPresentation yetAnotherPresentation |
	presentation := GLMPresentation new.
	newPresentation := presentation copy.
	self assert: newPresentation rootPrototype equals: presentation.
	self assert: newPresentation parentPrototype equals: presentation.
	yetAnotherPresentation := newPresentation copy.
	self assert: yetAnotherPresentation rootPrototype equals: presentation.
	self assert: yetAnotherPresentation parentPrototype equals: newPresentation
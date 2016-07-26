tests
testCopyNotTheSameRegistry

	| presentation newPresentation |
	presentation := GLMPresentation new.
	newPresentation := presentation copy.
	self assert: presentation glmSubscriptions ~~ newPresentation glmSubscriptions
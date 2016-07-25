testing
testSingleTraitCreation
	| trait |
	trait := factory newTrait.
	self assert: (Smalltalk globals allTraits includes: trait).
	self assert: trait traitUsers size = 0.
	self assert: trait traits size = 0
tests
testHasTitleIcon
	| presentation |
	presentation := GLMPresentation new.
	self assert: presentation hasTitleIcon not.
	presentation titleIcon: 'something'.
	self assert: presentation hasTitleIcon
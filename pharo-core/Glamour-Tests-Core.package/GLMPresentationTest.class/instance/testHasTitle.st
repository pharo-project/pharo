tests
testHasTitle
	| presentation |
	presentation := GLMPresentation new.
	self assert: presentation hasTitle not.
	presentation title: 'something'.
	self assert: presentation hasTitle
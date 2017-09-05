tests
testFirstLabelSecondLabel
	testedInstance firstLabel: 'label 1' secondLabel: 'label 2'.
	self assert: testedInstance firstButton label asString equals: 'label 1'.
	self assert: testedInstance secondButton label asString equals: 'label 2'
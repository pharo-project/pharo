tests
testwhenChangedDo
	| numberOfChanges |
	numberOfChanges := 0.
	testedInstance whenChangedDo: [ numberOfChanges := numberOfChanges + 1 ].
	testedInstance redSlider value: 66.
	testedInstance greenSlider value: 17.
	testedInstance blueSlider value: 200.
	self assert: numberOfChanges = 3
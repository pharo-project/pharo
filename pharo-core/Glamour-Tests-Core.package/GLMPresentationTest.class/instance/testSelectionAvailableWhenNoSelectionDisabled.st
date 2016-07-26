tests
testSelectionAvailableWhenNoSelectionDisabled
	| presentation |
	presentation := GLMPresentation new.

	presentation selection: 1.
	self assert: presentation selection equals: 1.
	self assert: presentation rawSelection equals: 1
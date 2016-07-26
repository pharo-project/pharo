tests
testRawSelectionAvailableWhenNoSelectionActive
"testSelectionNotAvailableWhenNoSelectionActive
testSelectionAvailableWhenNoSelectionDisabled"
	| presentation |
	presentation := GLMPresentation new.
	presentation acceptsSelection: false.

	presentation selection: 1.
	self assert: presentation selection equals: nil.
	self assert: presentation rawSelection equals: 1
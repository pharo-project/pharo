tests
testSelectionIntervalInitialization
	| presentation |
	presentation := GLMRubricTextPresentation new
		initialize: [ :pres | pres selectionInterval: (3 to: 9) ].
	self assert: presentation selectionInterval equals: nil.
	presentation initializePresentation.
	self assert: presentation selectionInterval equals: (3 to: 9)
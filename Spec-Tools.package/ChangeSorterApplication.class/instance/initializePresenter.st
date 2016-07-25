initialization
initializePresenter

	prettyButton activationAction: [ 
		prettyPrint := true.
		self updateTextContents ].
	prettyButton deactivationAction: [ 
		prettyPrint := false.
		self updateTextContents  ].
	diffButton activationAction: [ 
		showDiff := true.
		self updateTextContents ].
	diffButton deactivationAction: [ 
		showDiff := false.
		self updateTextContents  ].
	self registerChangeActions.
	self registerClassActions.
	self registerSelectorActions.
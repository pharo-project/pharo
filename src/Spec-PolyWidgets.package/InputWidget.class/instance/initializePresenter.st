initialization
initializePresenter

	input whenTextIsAccepted: [:text |
		self ok == false
			ifFalse: [ value value: text ] ].
		
	title whenChangedDo: [ self updateTitle ]
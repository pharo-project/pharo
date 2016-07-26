initialization
initializeDialogWindow: aWindow

	valid whenChangedDo: [ :b | aWindow toolbar okButton enabled: b ].
	aWindow toolbar okButton enabled: false.
	issueNumber 
		bindKeyCombination: Character cr control unix | $m control mac
		toAction: [ self valid ifTrue: [ aWindow triggerOkAction ] ]
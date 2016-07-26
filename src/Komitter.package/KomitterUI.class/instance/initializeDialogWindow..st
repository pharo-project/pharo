initialization
initializeDialogWindow: aWindow

	self bindKeyCombination: (Character cr control unix | $m control mac) toAction: [ aWindow triggerOkAction ].

	aWindow toolbar okButton enabled: canCommit value.

	canCommit whenChangedDo: [ :b | aWindow toolbar okButton enabled: b ].
	
	self focusOrder
		removeAll;
		add: message;
		add: remotes;
		add: aWindow toolbar;
		add: tree;
		add: previousMessages
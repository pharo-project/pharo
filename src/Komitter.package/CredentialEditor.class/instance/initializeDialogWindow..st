initialize-release
initializeDialogWindow: aWindow

	self 
		bindKeyCombination: $m meta 
		toAction: [ aWindow triggerOkAction ]
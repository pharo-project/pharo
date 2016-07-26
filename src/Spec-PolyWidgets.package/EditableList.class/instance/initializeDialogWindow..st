initialization
initializeDialogWindow: aWindow
	super initializeDialogWindow: aWindow.
	aWindow okAction: [ self performOkAction ]
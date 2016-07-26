items creation
buildSearchModeToolBar: aWindow
	^aWindow newToolbar: {
		self buildSearchingTextArea.
		self buildSearchButton.
		self buildRegExChooser.
		self buildSearchModeDropListIn: aWindow.
		self buildEnvironmentButton.
		self buildAllClassesButton}.
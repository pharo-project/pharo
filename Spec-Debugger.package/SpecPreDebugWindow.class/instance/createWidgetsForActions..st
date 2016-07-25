building widgets
createWidgetsForActions: aCollection

	self createNotifierPaneWidgets.
	self createButtonWidgetsForActions: aCollection.

	self setFocusOrderForActions: aCollection
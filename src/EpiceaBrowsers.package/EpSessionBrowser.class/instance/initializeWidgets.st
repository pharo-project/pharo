initialization
initializeWidgets

	self instantiateModels: {
		#statusBarModel -> #LabelModel.
		#selecteDirectoryButtonModel -> #ButtonModel.
		#sessionsTreeModel -> #TreeModel.
		#openButtonModel -> #ButtonModel.
		#refreshButtonModel -> #ButtonModel }
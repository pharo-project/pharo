initialization
initializeWidgets

	self instantiateModels: {
		#directoryTextModel -> #TextInputFieldModel.
		#selectDirectoryButtonModel -> #ButtonModel.
		#logNodesTreeModel -> #TreeModel.
		#refreshButtonModel -> #ButtonModel }
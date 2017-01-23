initialization
initializeWidgets

	self instantiateModels: {
		#enableMonitorButtonModel -> #ButtonModel.
		#createLogButtonModel -> #ButtonModel.
		#selectDirectoryButtonModel -> #ButtonModel.
		#logNodesTreeModel -> #TreeModel.
		#refreshButtonModel -> #ButtonModel }
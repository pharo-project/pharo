initialization
initializeWidgets

	self instantiateModels: {
		#itemsModel -> #FastTableModel.
		#toolbarModel -> #EpLogBrowserToolbarModel.
		#entryContentModel -> #DiffModel. 
		}.

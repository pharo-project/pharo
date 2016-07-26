initialization
initializeWidgets

	self instantiateModels: {
		#statusBarModel -> #LabelModel.
		#itemsModel -> #TreeModel.

		"tabs"
		#tabManagerModel -> #TabManagerModel.
		#entryContentTabModel -> #TabModel.
		#filtersTabModel -> #TabModel.
		
		"tabs content"
		#entryContentModel -> #DiffModel. 
		#filtersModel -> #TreeModel. 
		}.
		
	self whenWindowChanged: [ :windowModel | 
		self subscribeToLog.
		windowModel whenClosedDo: [ self unsubscribeFromLog ] ].
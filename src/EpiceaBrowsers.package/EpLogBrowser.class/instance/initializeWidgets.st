initialization
initializeWidgets

	self instantiateModels: {
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
		windowModel whenClosedDo: [ self unsubscribeFromLog ] ].
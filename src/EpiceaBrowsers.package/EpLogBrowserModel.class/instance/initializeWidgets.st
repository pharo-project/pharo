initialization
initializeWidgets

	self instantiateModels: {
		#itemsModel -> #FastTableModel.

		"tabs"
		#tabManagerModel -> #TabManagerModel.
		#entryContentTabModel -> #TabModel.
		#filtersTabModel -> #TabModel.
		
		"tabs content"
		#entryContentModel -> #DiffModel. 
		#filtersModel -> #TreeModel. 
		}.

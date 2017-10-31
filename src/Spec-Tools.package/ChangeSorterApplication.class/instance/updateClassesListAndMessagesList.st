api
updateClassesListAndMessagesList
	| sel |
	sel := methodsListPresenter selectedItem.
	
	self updateClassesList.
	methodsListPresenter setSelectedItem: sel.
	self updateMessagesList.
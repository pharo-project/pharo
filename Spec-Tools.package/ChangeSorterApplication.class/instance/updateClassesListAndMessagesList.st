api
updateClassesListAndMessagesList
	| sel |
	sel := methodsListModel selectedItem.
	
	self updateClassesList.
	methodsListModel setSelectedItem: sel.
	self updateMessagesList.
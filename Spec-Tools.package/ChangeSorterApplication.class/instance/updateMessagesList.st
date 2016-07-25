api
updateMessagesList
	| sel |
	sel := methodsListModel selectedItem.
	
	self selectedClass
			ifNil: [ methodsListModel items: {} ]
			ifNotNil: [:class | methodsListModel items: (self selectedChangeSet selectorsInClass: class name) sort].
				
	methodsListModel setSelectedItem: sel
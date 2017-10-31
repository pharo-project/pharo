api
updateMessagesList
	| sel |
	sel := methodsListPresenter selectedItem.
	
	self selectedClass
			ifNil: [ methodsListPresenter items: {} ]
			ifNotNil: [:class | methodsListPresenter items: (self selectedChangeSet selectorsInClass: class name) sort].
				
	methodsListPresenter setSelectedItem: sel
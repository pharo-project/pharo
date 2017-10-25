initialization
registerClassActions
	
	classesListPresenter whenSelectedItemChanged: [:class | 
		methodsListPresenter resetSelection.
		self updateTextContents.
		textPresenter behavior: class.
		textPresenter aboutToStyle: false.
		class 
			ifNil: [ methodsListPresenter items: {} ]
			ifNotNil: [ methodsListPresenter items: (self selectedChangeSet selectorsInClass: class name) sort ]].
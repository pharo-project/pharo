initialization
registerClassActions
	
	classesListModel whenSelectedItemChanged: [:class | 
		methodsListModel resetSelection.
		self updateTextContents.
		textModel behavior: class.
		textModel aboutToStyle: false.
		class 
			ifNil: [ methodsListModel items: {} ]
			ifNotNil: [ methodsListModel items: (self selectedChangeSet selectorsInClass: class name) sort ]].
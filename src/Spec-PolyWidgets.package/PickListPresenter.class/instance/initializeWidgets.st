initialization
initializeWidgets
	
	listModel := self newList.
	listModel displayBlock: [ :item | item ].
	self focusOrder add: listModel
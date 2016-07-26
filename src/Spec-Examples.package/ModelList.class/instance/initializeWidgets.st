initialization
initializeWidgets

	list := self newList.
	
	list items: (AbstractWidgetModel allSubclasses 
		sorted: [:a :b | a name < b name ]).
		
	self focusOrder add: list
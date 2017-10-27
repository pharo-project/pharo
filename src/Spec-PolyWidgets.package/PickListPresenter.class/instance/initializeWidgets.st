initialization
initializeWidgets
	
	listPresenter := self newList.
	listPresenter displayBlock: [ :item | item ].
	self focusOrder add: listPresenter
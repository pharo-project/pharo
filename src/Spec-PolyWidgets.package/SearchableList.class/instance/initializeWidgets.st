initialization
initializeWidgets

	listPresenter := self newList.
	searchPresenter := self newTextInput.
		
	searchPresenter 
		autoAccept: true;
		ghostText: 'filter'.
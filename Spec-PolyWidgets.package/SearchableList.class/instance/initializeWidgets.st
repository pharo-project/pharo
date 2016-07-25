initialization
initializeWidgets

	listModel := self newList.
	searchModel 	:= self newTextInput.
		
	searchModel 
		autoAccept: true;
		ghostText: 'filter'.
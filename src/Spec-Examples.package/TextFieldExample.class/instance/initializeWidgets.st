initialization
initializeWidgets

	methodBrowser := self instantiate: MessageBrowser.
	textField := self newTextInput.
	
	textField 
		ghostText: 'Enter a class name';
		autoAccept: true;
		removeEntryCompletion.
	methodBrowser 
		sortingBlock: [:a :b | a selector < b selector ];
		displayBlock: [:m | { m selector } ]